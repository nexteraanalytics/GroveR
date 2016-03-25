##' @importFrom R6 R6Class
##' @export
GroveR <- R6Class(
  "GroveR",
  portable = FALSE,
  private = list(
    fileRoot = ".",
    artDefs = list(),
    memCache = list()
  )
)

ArtifactDef <- R6Class(
  public = list(
    initialize = function(deps, create, retrieve, checkTime, store) {
      self$deps <- deps
      self$create <- create
      self$retrieve <- retrieve
      self$checkTime <- checkTime
      self$store <- store
    },

    deps = character(),
    create = NULL,
    retrieve = NULL,
    checkTime = NULL,
    store = NULL,

    show = function() {
      list( deps = self$deps,
            create = self$create,
            retrieve = self$retrieve,
            checkTime = self$checkTime,
            store = self$store
      )
    }
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
  if (!clobber && name %in% self$artifactNames())
    stop("'", name, "' is already a registered artifact")

  private$artDefs[[name]] <- ArtifactDef$new(deps, create, retrieve, checkTime, store)
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
  ## TODO: does this work with ggplot2?
  stopifnot(length(path)==1)

  path <- file.path(private$fileRoot, path)
  args <- list(path, ...)

  if (missing(type)) {
    type <- sub("^.+\\.([a-zA-Z0-9]+)$", "\\1", path)
  } else if (is.character(type)) {
    type <- match.arg(type)
  }
  devFunc <- match.fun(type)

  if (missing(deps)) {
    deps <- names(formals(create))
  }

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
  ## TODO let caller override funcArgs for dependencies
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
##' ## Or, register a data object
##' thingy3 %auto% data.frame(x=1:5, y=2:6)
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
  lapply(self$depNames(name), function(n) self$getArtifact(n))
})

GroveR$set("public", "depNames", function(name) {
  self$assertArtifactRegistered(name)
  private$artDefs[[name]]$deps
})

GroveR$set("private", "runCreate", function(name) {
  self$assertArtifactRegistered(name)
  flog.info("Generating GroveR artifact '%s'", name)
  do.call(private$artDefs[[name]]$create, private$fetchDeps(name))
})

##' @importFrom futile.logger flog.info
GroveR$set("public", "getArtifact", function(name) {
  self$assertArtifactRegistered(name)

  if (!self$isCurrent(name)) {
    private$memCache[[name]] <- private$runCreate(name)
    private$artDefs[[name]]$store(private$memCache[[name]])
  }

  if (!(name %in% names(private$memCache))) {
    private$memCache[[name]] <- private$artDefs[[name]]$retrieve()
  }

  return(private$memCache[[name]])
})

GroveR$set("public", "isCurrent", function(name) {
  self$assertArtifactRegistered(name)
  deps <- self$depNames(name)

  mtime <- private$artDefs[[name]]$checkTime()
  if(is.na(mtime)) return(FALSE)

  for (n in deps) {
    if (!self$isCurrent(n) || mtime < private$artDefs[[n]]$checkTime())
      return(FALSE)
  }

  return(TRUE)
})

GroveR$set("public", "artifactRegistered", function(name) {
  name %in% self$artifactNames()
})

GroveR$set("public", "assertArtifactRegistered", function(name) {
  if (!self$artifactRegistered(name)) stop("No such artifact '", name, "'")
})

GroveR$set("public", "artifactNames", function(name) {
  names(private$artDefs)
})

GroveR$set("public", "showArtifact", function(name) {
  self$assertArtifactRegistered(name)
  private$artDefs[[name]]$show()
})

GroveR$set("public", "getDependencyGraph", function() {
  art.names <- sort(artifactNames())
  num.arts  <- length(art.names)
  adjm <- matrix(0, nrow = num.arts, ncol = num.arts, dimnames = list(art.names, art.names))

  for(art in art.names) adjm[depNames(art), art] <- 1

  out <- graph_from_adjacency_matrix(adjm)
  vertex_attr(out, 'isCurrent') <- sapply(art.names, isCurrent)
  out
})

GroveR$set("public", "plotDependencyGraph", function(vertex.size = 15) {
  ig <- getDependencyGraph()
  convert <- c('red', 'green')
  vertex_attr(ig, 'color') <- convert[1 * vertex_attr(ig, 'isCurrent') + 1]
  plot(ig, vertex.size = vertex.size)
})

GroveR$set("public", "asGraphViz", function() {
  out <- 'digraph {\n  rankdir=BT;\n  node [style=filled fillcolor="white" color="black"];\n'
  for(art in self$artifactNames()) {
    color <- if(self$isCurrent(art)) "green" else "red"
    out <- paste0(out, sprintf('  "%s" [fillcolor=%s];\n', art, color))
    if(length(self$depNames(art)) > 0) {
      deps <- paste0('"', paste(self$depNames(art), collapse='" "'), '"')
      out <- paste0(out, sprintf('  {%s} -> "%s";\n', deps, art))
    }
  }
  out <- paste0(out, "}\n")
  out
})
