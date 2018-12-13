## Copyright (c) 2016-2018 Windlogics, Inc.
## See the DESCRIPTION file for licensing information.

##' @include objects.R
NULL

.private("fetchDeps", function(name) {
  lapply(depNames(name), function(n) getArtifact(n))
})

#' Names of dependencies for an artifact
#'
#' @name depNames
#' @param name name of the artifact whose dependencies you would like to query
#' @return character vector containing the names of this artifact's dependency artifacts
.public("depNames", function(name) {
  assertArtifactRegistered(name)
  artDefs[[name]]$deps
})

.private("runCreate", function(name) {
  assertArtifactRegistered(name)
  flog.info("Generating GroveR artifact '%s'", name)
  do.call(artDefs[[name]]$create, fetchDeps(name))
})

#' Retrieve an artifact, building it if necessary
#'
#' If the artifact has already been built and is up to date with its dependencies, return it.
#' Otherwise, build it (recursively creating any dependencies that don't exist or are out of date)
#' and return it.
#'
#' @name getArtifact
#' @param name name of the artifact to return
#'
#' @importFrom futile.logger flog.info
.public("getArtifact", function(name) {
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

#' Last modified time of an artifact
#'
#' @name getModTime
#' @param name name of the artifact
#' @return POSIXct containing the datetime of when the given artifact was built, or `NA` if it has
#'   never been built.
.public("getModTime", function(name) {
  assertArtifactRegistered(name)
  artDefs[[name]]$checkTime()
})

#' Up-to-date check for artifact
#'
#' Perform a recursive check on this artifact and its dependencies to see whether any of them
#' need to be rebuilt.  If so, return `FALSE`, otherwise return `TRUE`.
#'
#' @name isCurrent
#' @param name name of the artifact
#' @return logical value indicating whether the artifact is up to date with respect to its
#'   dependencies (`TRUE`) or whether it needs to be rebuilt (`FALSE`).
.public("isCurrent", function(name) {
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

#' Check whether an artifact is registered
#'
#' @name artifactRegistered
#' @param name name of the artifact
#' @return logical value indicating whether an artifact with the given name is registered
.public("artifactRegistered", function(name) {
  name %in% artifactNames()
})

#' Check whether an artifact is registered
#'
#' Throws an exception if an artifact with the given name has been registered, otherwise does nothing.
#'
#' @name assertArtifactRegistered
#' @param name name of the artifact
.public("assertArtifactRegistered", function(name) {
  if (!artifactRegistered(name)) stop("No such artifact '", name, "'")
})

#' Names of registered artifacts
#'
#' @return the names of all currently registered artifacts
#'
#' @name artifactNames
#' @param name name of the artifact
.public("artifactNames", function(name) {
  names(artDefs)
})

#' Inspect information about an artifact
#'
#' @name artifactInfo
#' @param name name of the artifact
#' @return list with the following components:
#' \describe{
#'   \item{deps}{names of dependency artifacts}
#'   \item{create}{function to create artifact from its dependencies}
#'   \item{retrieve}{function to retrieve artifact from cache}
#'   \item{checkTime}{function returning time of artifact creation}
#'   \item{store}{function to store artifact in cache}
#' }
.public("artifactInfo", function(name) {
  assertArtifactRegistered(name)
  artDefs[[name]]$show()
})

#' Dependency graph among artifacts
#'
#' @return a graph of the relationship between all artifacts, as an `igraph` graph object.
#'   A vertex attribute `"isCurrent"` indicates which artifacts are up-to-date.
#'
#' @importFrom igraph graph_from_adjacency_matrix vertex_attr<-
#' @name getDependencyGraph
.public("getDependencyGraph", function() {
  art.names <- sort(self$artifactNames())
  num.arts  <- length(art.names)
  adjm <- matrix(0, nrow = num.arts, ncol = num.arts, dimnames = list(art.names, art.names))

  for(art in art.names) {
    deps <- depNames(art)
    unknowns <- setdiff(deps, art.names)
    if (length(unknowns) > 0)
      stop("Unknown artifact '", unknowns[1], "' as dependency of '", art, "'")
    if (length(deps) > 0)
      adjm[deps, art] <- 1
  }

  out <- graph_from_adjacency_matrix(adjm)
  vertex_attr(out, 'isCurrent') <- sapply(art.names, isCurrent)
  out
})

#' Plot graph of dependencies using `igraph`
#'
#' Currently up-to-date artifacts are colored green; out-of-date or non-built artifacts are red.
#'
#' @param vertex.size size of each node - passed to `plot.igraph`
#' @name plotDependencyGraph
#' @importFrom igraph vertex_attr<- vertex_attr
.public("plotDependencyGraph", function(vertex.size = 15) {
  ig <- getDependencyGraph()
  convert <- c('red', 'green')
  vertex_attr(ig, 'color') <- convert[1 * vertex_attr(ig, 'isCurrent') + 1]
  plot(ig, vertex.size = vertex.size)
})

#' Export dependency graph as GraphViz code
#'
#' Currently up-to-date artifacts are colored green; out-of-date artifacts are red; non-built artifacts
#' are white.
#' @name asGraphViz
.public("asGraphViz", function() {
  out <- 'digraph {\n  rankdir=BT;\n  node [style=filled fillcolor="white" color="black"];\n'
  for(art in artifactNames()) {
    color <- if(isCurrent(art)) "green" else if(!is.na(getModTime(art))) "red" else "white"
    out <- paste0(out, sprintf('  "%s" [fillcolor=%s];\n', art, color))
    if(length(depNames(art)) > 0) {
      deps <- paste0('"', paste(depNames(art), collapse='" "'), '"')
      out <- paste0(out, sprintf('  {%s} -> "%s";\n', deps, art))
    }
  }
  out <- paste0(out, "}\n")
  out
})
