##' @importFrom R6 R6Class

Biome <- R6Class(
  "Biome",
  private = list(
    fileRoot = ".",
    deps = list(),
    create = list(),
    retrieve = list(),
    checkTime = list(),
    store = list(),
    memCache = list()
  )
)

Biome$set("public", "setRoot", function(dir) {
  private$fileRoot <- dir
})

noop <- function(...){}

match.parse <- function(pattern, x, ...) {
  result <- regexpr(pattern, x, ...)
  attr(result, 'text') <- x
  result
}

match.extr <- function(parse, i) {
  start <- attr(parse, 'capture.start')[i]
  substr(attr(parse, 'text'), start, start+attr(parse, 'capture.length')[i]-1)
}

## TODO - don't memCache everything?
Biome$set("public", "registerArtifact", function(name, deps, create, retrieve, checkTime, store) {
  if (missing(deps) || is.null(deps))
    deps <- character()
  if (name %in% names(private$deps))
    stop("'", name, "' is already a registered artifact")
  private$deps[[name]] <- deps
  private$create[[name]] <- create
  private$retrieve[[name]] <- retrieve
  private$checkTime[[name]] <- checkTime
  private$store[[name]] <- store
  invisible()
})

Biome$set("public", "registerRDSArtifact", function(name, deps, create, path) {
  self$registerArtifact(name,
                        deps,
                        create,
                        retrieve=function() readRDS(path),
                        checkTime=function() file.mtime(path),
                        store=function(object) {
                          if (!file.exists(dirname(path)))
                            dir.create(dirname(path), recursive=TRUE)
                          saveRDS(object, path)
                        })
})

Biome$set("public", "registerCSVArtifact", function(name, deps, create, path, readFun=read.csv, writeFun=write.csv, ...) {
  self$registerArtifact(name,
                        deps,
                        create,
                        retrieve=function() readFun(path, ...),
                        checkTime=function() file.mtime(path),
                        store=function(object) {
                          if (!file.exists(dirname(path)))
                            dir.create(dirname(path), recursive=TRUE)
                          writeFun(object, path)
                        })
})

Biome$set("public", "registerStaticFileArtifact", function(name, path, readFun=readRDS, ...) {
  self$registerArtifact(name,
                        create=noop,
                        retrieve=function() readFun(path, ...),
                        checkTime=function() file.mtime(path),
                        store=noop)
})

##' Register a function that produces an artifact
##'
##' @usage
##' App <- Biome$new()
##'
Biome$set("public", "registerFunction", function(func, funcBody=func, funcName=deparse(substitute(func)),
                                                 path=paste0(funcName, ".rds")) {
  funcArgs <- names(formals(funcBody))
  self$registerRDSArtifact(funcName, funcArgs, funcBody, paste0(funcName, ".rds"))
})



Biome$set("public", "getArtifact", function(name) {
  self$assertArtifactRegistered(name)

  if (!self$isCurrent(name)) {
    depObjs <- lapply(private$deps[[name]], function(n) self$getArtifact(n))
    private$memCache[[name]] <- do.call(private$create[[name]], depObjs)
    private$store[[name]](private$memCache[[name]])
  }

  if (!(name %in% names(private$memCache))) {
    private$memCache[[name]] <- private$retrieve[[name]]()
  }

  return(private$memCache[[name]])
})

Biome$set("public", "isCurrent", function(name) {
  self$assertArtifactRegistered(name)
  deps <- private$deps[[name]]

  mtime <- private$checkTime[[name]]()
  if(is.na(mtime)) return(FALSE)

  for (n in deps) {
    if (!self$isCurrent(n) || mtime < private$checkTime[[n]]())
      return(FALSE)
  }

  return(TRUE)
})

Biome$set("public", "artifactRegistered", function(name) {
  name %in% names(private$deps)
})

Biome$set("public", "assertArtifactRegistered", function(name) {
  if (!self$artifactRegistered(name)) stop("No such artifact '", name, "'")
})


Biome$set("public", "listArtifacts", function(name) {
  names(private$deps)
})

Biome$set("public", "getDependencyGraph", function() {
  ## TODO return igraph object
})
