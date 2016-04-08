## Copyright (c) 2016 Windlogics, Inc.
## See the DESCRIPTION file for licensing information.

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

## Some little helper functions
.public  <- function(...) GroveR$set("public",  ...)
.private <- function(...) GroveR$set("private", ...)
.noop <- function(...){}
