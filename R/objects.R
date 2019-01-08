## Copyright (c) 2016-2018 Windlogics, Inc.
## See the DESCRIPTION file for licensing information.

.initializer <- function(...) {
  args <- list(...)

  if (length(args) > 0 && is.null(names(args)))
    stop("Unknown unnamed argument")

  for (name in names(args)) {
    if (name %in% names(self))
      self[[name]] <- args[[name]]
    else if (is.null(name))
      stop("Unknown unnamed argument")
    else
      stop("Unknown argument '", name, "'")
  }
}


#' GroveR class object
#' @section Usage:
#' \preformatted{
#' App <- GroveR$new()
#' `%createdBy%` <- App$auto
#' }
#'
#'
#'
#' @importFrom R6 R6Class
#' @export
GroveR <- R6Class(
  portable = FALSE,
  public = list(
    fileRoot = ".",
    initialize = .initializer
  ),
  private = list(
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

## Some little helper functions
.public  <- function(...) GroveR$set("public",  ...)
.private <- function(...) GroveR$set("private", ...)
.noop <- function(...){}

## For R 3.1.x compatibility
if (!exists("dir.exists")) {
  dir.exists <- function(paths) file.exists(paths) & file.info(paths)$isdir
}
if (!exists("file.mtime")) {
  file.mtime <- function(...) file.info(..., extra_cols = FALSE)$mtime
}
