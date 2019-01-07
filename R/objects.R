## Copyright (c) 2016-2018 Windlogics, Inc.
## See the DESCRIPTION file for licensing information.

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
