## Copyright (c) 2016-2018 Windlogics, Inc.
## See the DESCRIPTION file for licensing information.

##' @include objects.R
NULL

##' Set the application root directory
##' @name setRoot
##' @examples
##' App <- GroveR$new()
##' App$setRoot(dir)
##'
##' @param dir new value of the root directory
.public("setRoot", function(dir) {
  fileRoot <<- dir
})

#' Register a generic artifact
#'
#' @name registerArtifact
#' @usage
#' registerArtifact(name,
#'                  deps,
#'                  create,
#'                  retrieve,
#'                  checkTime,
#'                  store,
#'                  clobber=FALSE)
#'
#' @param name name of the artifact
#' @param deps names of other artifacts this artifact depends on
#' @param create function to create this artifact object from its dependency objects
#' @param retrieve function to retrieve this artifact from cache
#' @param checkTime function to fetch the time of last update for this artifact
#' @param store function to store this artifact to cache
#' @param clobber whether to allow redifinition of this artifact if it is already defined
.public("registerArtifact", function(name, deps, create, retrieve, checkTime, store, clobber=FALSE) {
  ## TODO - don't memCache everything?
  if (missing(deps) || is.null(deps))
    deps <- character()
  if (!clobber && name %in% artifactNames())
    stop("'", name, "' is already a registered artifact")

  artDefs[[name]] <<- ArtifactDef$new(deps, create, retrieve, checkTime, store)
  invisible()
})

#' Register an on-disk RDS file artifact
#'
#' Registers an artifact created by the `create` function from its dependencies, and cached to
#' disk using \code{\link{saveRDS}}/\code{\link{readRDS}}.
#'
#' @name registerRDSArtifact
#' @usage
#' registerRDSArtifact(name,
#'                     deps,
#'                     create,
#'                     path,
#'                     readFun=readRDS,
#'                     writeFun=saveRDS,
#'                     ...)
#'
#' @inheritParams registerArtifact  // TODO Doesn't work yet
#' @param path path to cached file
#' @param readFun function to read file from disk
#' @param writeFun function to write file to disk
#' @param ... further arguments passed to \link{registerArtifact}
.public("registerRDSArtifact", function(name, deps, create, path, readFun=readRDS, writeFun=saveRDS, ...) {
  path <- file.path(fileRoot, path)
  registerArtifact(name,
                   deps,
                   create,
                   retrieve=function() readFun(path),
                   checkTime=function() file.mtime(path),
                   store=function(object) {
                     if (!file.exists(dirname(path)))
                       dir.create(dirname(path), recursive=TRUE)
                     writeFun(object, path)
                   }, ...)
})

#' Register an on-disk CSV file artifact
#'
#' @name registerCSVArtifact
#' @usage
#' registerCSVArtifact(name,
#'                     deps,
#'                     create,
#'                     path,
#'                     readFun=read.csv,
#'                     writeFun=write.csv,
#'                     ...)
#'
#' @inheritParams registerArtifact  // TODO Doesn't work yet
#' @param readFun function to read file from disk
#' @param writeFun function to write file to disk
#' @param ... further arguments passed to \link{registerArtifact}
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

#' Register an on-disk static file artifact
#'
#' The file is assumed to already exist, so unlike `registerRDSArtifact`, there are no `create` or `writeFun` arguments.
#'
#' @name registerStaticFileArtifact
#' @usage
#' registerStaticFileArtifact(name,
#'                            path,
#'                            readFun=readRDS,
#'                            ...)
#'
#' @inheritParams registerArtifact  // TODO Doesn't work yet
#' @param readFun function to read file from disk
#' @param ... further arguments passed to \link{registerArtifact}
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

#' Register an image file artifact
#'
#' Unlike most other artifact types, the `create` function is expected to plot things to a graphics
#' device instead of returning an R object.  Before calling the `create` function, we open the
#' appropriate graphics device (`png` or `pdf`), then afterward we close it using \code{\link{dev.off}}.
#'
#' @name registerImage
#' @usage
#' registerImage(name,
#'               deps,
#'               create,
#'               path=name,
#'               type=c("png", "pdf"),
#'               clobber=FALSE,
#'               ...)
#'
#' @inheritParams registerArtifact  // TODO Doesn't work yet
#' @param type type of image file to create
#' @param ... further arguments passed to the graphics device function - NOT to
#'   \link{registerArtifact} like similar methods do
.public("registerImage", function(name, deps, create, path=name, type=c("png", "pdf"), clobber=FALSE, ...) {
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

#' Register a function that creates an artifact
#'
#' @name registerFunction
#' @usage
#' registerFunction(func,
#'                  funcBody=func,
#'                  funcName=deparse(substitute(func)),
#'                  path=paste0(funcName, ".rds"),
#'                  ...)
#'
#' @inheritParams registerArtifact  // TODO Doesn't work yet
#' @param type type of image file to create
#' @param ... further arguments passed to \link{registerArtifact}
.public("registerFunction", function(func, funcBody=func, funcName=deparse(substitute(func)),
                                     path=paste0(funcName, ".rds"), ...) {
  ## TODO let caller override funcArgs for dependencies
  stopifnot(inherits(funcBody, "function"))
  funcArgs <- names(formals(funcBody))
  registerRDSArtifact(funcName, funcArgs, funcBody, path=path, ...)
})

##' Register a do-what-I-mean artifact
##'
##' Register a function or data item as an artifact.
##'
##' @usage
##' auto(what,
##'      how=what,
##'      name=deparse(substitute(what)),
##'      ...)
##'
##' @examples
##' \dontrun{
##' App <- GroveR$new()
##' `%auto%` <- App$auto
##'
##' ## 'thingy' artifact depends on 'dep1' and 'dep2'
##' thingy %auto% function(dep1, dep2) {
##'   rbind(dep1, dep2) # Or whatever
##' }
##'
##' ## Or also create a local function - registration is equivalent to the above
##' thingy <- function(dep1, dep2) {
##'   rbind(dep1, dep2) # Or whatever
##' }
##' App$auto(thingy)
##'
##' ## Or, register a static data item as an artifact
##' thingy2 %auto% data.frame(x=1:5, y=2:6)
##' }
##'
##' @name auto
##' @param what the name of the artifact to register, as a bare (unquoted) word
##' @param how a function to compute the new artifact from its dependencies, or (if not a function)
##'   a data object to register as the artifact (so it can be used as a dependency of other things)
##' @param name the name of the artifact to register, as a simple character vector
##' @param ... further arguments passed to \code{\link{registerFunction}} or \code{\link{registerArtifact}}
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
