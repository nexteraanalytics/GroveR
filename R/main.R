##' @importFrom R6 R6Class
##' @export
GroveR <- R6Class(
  "GroveR",
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

GroveR$set("public", "setRoot", function(dir) {
  private$fileRoot <- dir
})

noop <- function(...){}

## TODO - don't memCache everything?
GroveR$set("public", "registerArtifact", function(name, deps, create, retrieve, checkTime, store, clobber=FALSE) {
  if (missing(deps) || is.null(deps))
    deps <- character()
  if (!clobber && name %in% names(private$deps))
    stop("'", name, "' is already a registered artifact")
  private$deps[[name]] <- deps
  private$create[[name]] <- create
  private$retrieve[[name]] <- retrieve
  private$checkTime[[name]] <- checkTime
  private$store[[name]] <- store
  invisible()
})

GroveR$set("public", "registerRDSArtifact", function(name, deps, create, path, ...) {
  path <- file.path(private$fileRoot, path)
  self$registerArtifact(name,
                        deps,
                        create,
                        retrieve=function() readRDS(path),
                        checkTime=function() file.mtime(path),
                        store=function(object) {
                          if (!file.exists(dirname(path)))
                            dir.create(dirname(path), recursive=TRUE)
                          saveRDS(object, path)
                        }, ...)
})

GroveR$set("public", "registerCSVArtifact", function(name, deps, create, path, readFun=read.csv, writeFun=write.csv, ...) {
  path <- file.path(private$fileRoot, path)
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

GroveR$set("public", "registerStaticFileArtifact", function(name, path, readFun=readRDS, ...) {
  path <- file.path(private$fileRoot, path)
  self$registerArtifact(name,
                        create=noop,
                        retrieve=function() {
                          if(!file.exists(path))
                            stop("Can't read '", path, "', no such file")
                          readFun(path, ...)
                        },
                        checkTime=function() file.mtime(path),
                        store=function(object) stop("Can't write '", name, "', it was declared as static"))
})

GroveR$set("public", "registerImage", function(name, deps, create, path=name, type=c("png", "pdf"), clobber=FALSE, ...) {
  ## TODO: infer 'type' from 'path' if missing
  ## TODO: infer 'deps' from 'create' if missing
  path <- file.path(private$fileRoot, path)
  args <- list(path, ...)
  devFunc <- switch(match.arg(type),
                    png=png,
                    pdf=pdf)
  self$registerArtifact(name,
                        deps,
                        create=function(...) {
                          do.call(devFunc, args)
                          on.exit(dev.off())
                          args2 <- list(...)
                          do.call(create, args2)
                        },
                        checkTime=function() file.mtime(path),
                        store=noop,
                        clobber=clobber,
                        retrieve=noop)
})

GroveR$set("public", "registerFunction", function(func, funcBody=func, funcName=deparse(substitute(func)),
                                                 path=paste0(funcName, ".rds"), ...) {
  stopifnot(inherits(funcBody, "function"))
  funcArgs <- names(formals(funcBody))
  self$registerRDSArtifact(funcName, funcArgs, funcBody, path=path, ...)
})

##' Register a do-what-I-mean artifact
##'
##' Register a function, or data item, as an artifact.
##'
##' @examples
##' App <- GroveR$new()
##' `%auto%` <- App$auto
##'
##' ## 'thingy' depends on 'dep1' and 'dep2'
##' thingy %auto% function(dep1, dep2) {
##'   rbind(dep1, dep2) # Or whatever
##' }
##'
##' ## Or also create a local function
##' thingy2 <- function(dep1, dep2) {
##'   rbind(dep1, dep2) # Or whatever
##' }
##' App$auto(thingy2)
##'
##' @name set
GroveR$set("public", "auto", function(what, how=what, name=deparse(substitute(what)), ...) {
  if(inherits(how, "function")) {
    self$registerFunction(funcName=name, funcBody=how, ...)
  } else {
    self$registerArtifact(name,
                          create=noop,
                          store=noop,
                          retrieve=function() how,
                          checkTime=function() -Inf,
                          ...)
  }
})

GroveR$set("private", "fetchDeps", function(name) {
  lapply(private$deps[[name]], function(n) self$getArtifact(n))
})

GroveR$set("private", "runCreate", function(name) {
  flog.info("Generating GroveR artifact '%s'", name)
  do.call(private$create[[name]], private$fetchDeps(name))
})

##' @importFrom futile.logger flog.info
GroveR$set("public", "getArtifact", function(name) {
  self$assertArtifactRegistered(name)

  if (!self$isCurrent(name)) {
    private$memCache[[name]] <- private$runCreate(name)
    private$store[[name]](private$memCache[[name]])
  }

  if (!(name %in% names(private$memCache))) {
    private$memCache[[name]] <- private$retrieve[[name]]()
  }

  return(private$memCache[[name]])
})

GroveR$set("public", "isCurrent", function(name) {
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

GroveR$set("public", "artifactRegistered", function(name) {
  name %in% names(private$deps)
})

GroveR$set("public", "assertArtifactRegistered", function(name) {
  if (!self$artifactRegistered(name)) stop("No such artifact '", name, "'")
})


GroveR$set("public", "artifactNames", function(name) {
  names(private$deps)
})

GroveR$set("public", "showArtifact", function(name) {
  self$assertArtifactRegistered(name)
  list( deps = private$deps[[name]],
        create = private$create[[name]],
        retrieve = private$retrieve[[name]],
        checkTime = private$checkTime[[name]],
        store = private$store[[name]]
  )
})

GroveR$set("public", "getDependencyGraph", function() {
  ## TODO return igraph object
})

GroveR$set("public", "asGraphViz", function() {
  out <- 'digraph {\n  rankdir=TB;\n  node [style=filled fillcolor="white" color="black"];\n'
  for(art in names(private$deps)) {
    color <- if(self$isCurrent(art)) "green" else "red"
    out <- paste0(out, sprintf('  "%s" [fillcolor=%s];\n', art, color))
    if(length(private$deps[[art]]) > 0) {
      deps <- paste0('"', paste(private$deps[[art]], collapse='" "'), '"')
      out <- paste0(out, sprintf('  "%s" -> {%s};\n', art, deps))
    }
  }
  out <- paste0(out, "}\n")
  out
})
