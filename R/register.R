## Copyright (c) 2016 Windlogics, Inc.
## See the DESCRIPTION file for licensing information.

##' @include objects.R
NULL

##' Set the application root directory
##' @name setRoot
##' @usage App$setRoot(dir)
##'
##' @param dir new value of the root directory
.public("setRoot", function(dir) {
  fileRoot <<- dir
})

## TODO - don't memCache everything?
.public("registerArtifact", function(name, deps, create, retrieve, checkTime, store, clobber=FALSE) {
  if (missing(deps) || is.null(deps))
    deps <- character()
  if (!clobber && name %in% artifactNames())
    stop("'", name, "' is already a registered artifact")

  artDefs[[name]] <<- ArtifactDef$new(deps, create, retrieve, checkTime, store)
  invisible()
})

.public("registerRDSArtifact", function(name, deps, create, path, ...) {
  path <- file.path(fileRoot, path)
  registerArtifact(name,
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

.public("registerCSVArtifact", function(name, deps, create, path, readFun=read.csv, writeFun=write.csv, ...) {
  path <- file.path(fileRoot, path)
  registerArtifact(name,
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

.public("registerStaticFileArtifact", function(name, path, readFun=readRDS, ...) {
  path <- file.path(fileRoot, path)
  registerArtifact(name,
                   create=.noop,
                   retrieve=function() {
                     if(!file.exists(path))
                       stop("Can't read '", path, "', no such file")
                     readFun(path, ...)
                   },
                   checkTime=function() file.mtime(path),
                   store=function(object) stop("Can't write '", name, "', it was declared as static"))
})

.public("registerImage", function(name, deps, create, path=name, type=c("png", "pdf"), clobber=FALSE, ...) {
  ## TODO: infer 'type' from 'path' if missing
  ## TODO: does this work with ggplot2?
  stopifnot(length(path)==1)

  path <- file.path(fileRoot, path)
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

  registerArtifact(name,
                   deps,
                   create=function(...) {
                     do.call(devFunc, args)
                     on.exit(dev.off())
                     args2 <- list(...)
                     do.call(create, args2)
                   },
                   checkTime=function() file.mtime(path),
                   store=.noop,
                   clobber=clobber,
                   retrieve=.noop)
})

.public("registerFunction", function(func, funcBody=func, funcName=deparse(substitute(func)),
                                     path=paste0(funcName, ".rds"), ...) {
  ## TODO let caller override funcArgs for dependencies
  stopifnot(inherits(funcBody, "function"))
  funcArgs <- names(formals(funcBody))
  registerRDSArtifact(funcName, funcArgs, funcBody, path=path, ...)
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
.public("auto", function(what, how=what, name=deparse(substitute(what)), ...) {
  if(inherits(how, "function")) {
    registerFunction(funcName=name, funcBody=how, ...)
  } else {
    registerArtifact(name,
                     create=.noop,
                     store=.noop,
                     retrieve=function() how,
                     checkTime=function() -Inf,
                     ...)
  }
})
