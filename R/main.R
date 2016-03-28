##' @importFrom R6 R6Class
##' @export
GroveR <- R6Class(
  "GroveR",  ## TODO not really necessary, right?
  portable = FALSE,
  private = list(
    fileRoot = ".",
    artDefs = list(),
    memCache = list()
  )
)

ArtifactDef <- R6Class(
  portable = FALSE,
  public = list(
    initialize = function(deps, create, retrieve, checkTime, store) {
      deps <<- deps
      create <<- create
      retrieve <<- retrieve
      checkTime <<- checkTime
      store <<- store
    },

    deps = character(),
    create = NULL,
    retrieve = NULL,
    checkTime = NULL,
    store = NULL,

    show = function() {
      list( deps = deps,
            create = create,
            retrieve = retrieve,
            checkTime = checkTime,
            store = store
      )
    }
  )
)

GroveR$set("public", "setRoot", function(dir) {
  fileRoot <<- dir
})

noop <- function(...){}

## TODO - don't memCache everything?
GroveR$set("public", "registerArtifact", function(name, deps, create, retrieve, checkTime, store, clobber=FALSE) {
  if (missing(deps) || is.null(deps))
    deps <- character()
  if (!clobber && name %in% artifactNames())
    stop("'", name, "' is already a registered artifact")

  artDefs[[name]] <<- ArtifactDef$new(deps, create, retrieve, checkTime, store)
  invisible()
})

GroveR$set("public", "registerRDSArtifact", function(name, deps, create, path, ...) {
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

GroveR$set("public", "registerCSVArtifact", function(name, deps, create, path, readFun=read.csv, writeFun=write.csv, ...) {
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

GroveR$set("public", "registerStaticFileArtifact", function(name, path, readFun=readRDS, ...) {
  path <- file.path(fileRoot, path)
  registerArtifact(name,
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
                        store=noop,
                        clobber=clobber,
                        retrieve=noop)
})

GroveR$set("public", "registerFunction", function(func, funcBody=func, funcName=deparse(substitute(func)),
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
GroveR$set("public", "auto", function(what, how=what, name=deparse(substitute(what)), ...) {
  if(inherits(how, "function")) {
    registerFunction(funcName=name, funcBody=how, ...)
  } else {
    registerArtifact(name,
                          create=noop,
                          store=noop,
                          retrieve=function() how,
                          checkTime=function() -Inf,
                          ...)
  }
})

GroveR$set("private", "fetchDeps", function(name) {
  lapply(depNames(name), function(n) getArtifact(n))
})

GroveR$set("public", "depNames", function(name) {
  assertArtifactRegistered(name)
  artDefs[[name]]$deps
})

GroveR$set("private", "runCreate", function(name) {
  assertArtifactRegistered(name)
  flog.info("Generating GroveR artifact '%s'", name)
  do.call(artDefs[[name]]$create, fetchDeps(name))
})

##' @importFrom futile.logger flog.info
GroveR$set("public", "getArtifact", function(name) {
  assertArtifactRegistered(name)

  if (!isCurrent(name)) {
    memCache[[name]] <<- runCreate(name)
    artDefs[[name]]$store(memCache[[name]])
  }

  if (!(name %in% names(memCache))) {
    memCache[[name]] <<- artDefs[[name]]$retrieve()
  }

  return(memCache[[name]])
})

GroveR$set("public", "isCurrent", function(name) {
  assertArtifactRegistered(name)
  deps <- depNames(name)

  mtime <- artDefs[[name]]$checkTime()
  if(is.na(mtime)) return(FALSE)

  for (n in deps) {
    if (!isCurrent(n) || mtime < artDefs[[n]]$checkTime())
      return(FALSE)
  }

  return(TRUE)
})

GroveR$set("public", "artifactRegistered", function(name) {
  name %in% artifactNames()
})

GroveR$set("public", "assertArtifactRegistered", function(name) {
  if (!artifactRegistered(name)) stop("No such artifact '", name, "'")
})

GroveR$set("public", "artifactNames", function(name) {
  names(artDefs)
})

GroveR$set("public", "showArtifact", function(name) {
  assertArtifactRegistered(name)
  artDefs[[name]]$show()
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
  for(art in artifactNames()) {
    color <- if(isCurrent(art)) "green" else "red"
    out <- paste0(out, sprintf('  "%s" [fillcolor=%s];\n', art, color))
    if(length(depNames(art)) > 0) {
      deps <- paste0('"', paste(depNames(art), collapse='" "'), '"')
      out <- paste0(out, sprintf('  {%s} -> "%s";\n', deps, art))
    }
  }
  out <- paste0(out, "}\n")
  out
})
