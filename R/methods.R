## Copyright (c) 2016-2018 Windlogics, Inc.
## See the DESCRIPTION file for licensing information.

##' @include objects.R
NULL

.private("fetchDeps", function(name) {
  lapply(depNames(name), function(n) getArtifact(n))
})

.public("depNames", function(name) {
  assertArtifactRegistered(name)
  artDefs[[name]]$deps
})

.private("runCreate", function(name) {
  assertArtifactRegistered(name)
  flog.info("Generating GroveR artifact '%s'", name)
  do.call(artDefs[[name]]$create, fetchDeps(name))
})

##' @importFrom futile.logger flog.info
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

.public("getModTime", function(name) {
  assertArtifactRegistered(name)
  artDefs[[name]]$checkTime()
})

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

.public("artifactRegistered", function(name) {
  name %in% artifactNames()
})

.public("assertArtifactRegistered", function(name) {
  if (!artifactRegistered(name)) stop("No such artifact '", name, "'")
})

.public("artifactNames", function(name) {
  names(artDefs)
})

.public("showArtifact", function(name) {
  assertArtifactRegistered(name)
  artDefs[[name]]$show()
})

#' @importFrom igraph graph_from_adjacency_matrix vertex_attr<-
.public("getDependencyGraph", function() {
  art.names <- sort(artifactNames())
  num.arts  <- length(art.names)
  adjm <- matrix(0, nrow = num.arts, ncol = num.arts, dimnames = list(art.names, art.names))

  for(art in art.names) adjm[depNames(art), art] <- 1

  out <- graph_from_adjacency_matrix(adjm)
  vertex_attr(out, 'isCurrent') <- sapply(art.names, isCurrent)
  out
})

#' @importFrom igraph vertex_attr<- vertex_attr
.public("plotDependencyGraph", function(vertex.size = 15) {
  ig <- getDependencyGraph()
  convert <- c('red', 'green')
  vertex_attr(ig, 'color') <- convert[1 * vertex_attr(ig, 'isCurrent') + 1]
  plot(ig, vertex.size = vertex.size)
})

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
