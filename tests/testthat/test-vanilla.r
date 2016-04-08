## Copyright (c) 2016 Windlogics, Inc.
## See the DESCRIPTION file for licensing information.

context("Vanilla interfaces")

futile.logger::flog.threshold('WARN')

test_that("Vanilla interfaces work", {
  root <- 'foo/bar'
  toproot <- 'foo'

  ## Initialize files
  if(dir.exists(toproot))
    unlink(toproot, recursive = TRUE)
  if(!dir.exists(root))
    dir.create(root, recursive = TRUE)
  on.exit(if(dir.exists(toproot)) unlink(toproot, recursive = TRUE))

  saveRDS(1:9, "foo/bar/scada.met.clearsky.data.rds")
  saveRDS(1:4, "foo/bar/inverter.met.info.rds")

  App <- GroveR$new()
  App$setRoot(root)

  App$registerRDSArtifact(
    name="long.term.projections",
    deps=c("geomodel.data", "scada.met.clearsky.data", "inverter.met.info"),
    create=function(geomodel.data, scada.met.clearsky.data, inverter.met.info) {
      list(g=length(geomodel.data),
           s=length(scada.met.clearsky.data),
           i=length(inverter.met.info))
    },
    path="ltp.rds")

  App$registerRDSArtifact(
    name="geomodel.data",
    create=function() {
      1:7
    },
    path="geomodel.data.rds")

  App$registerStaticFileArtifact(
    name="scada.met.clearsky.data",
    path="scada.met.clearsky.data.rds")

  App$registerStaticFileArtifact(
    name="inverter.met.info",
    path="inverter.met.info.rds")

  res <- App$getArtifact('long.term.projections')

  testthat::expect_equal(res$g, 7)
  testthat::expect_equal(res$s, 9)
  testthat::expect_equal(res$i, 4)

  ## Catch non-functions early
  expect_error(App$registerFunction(foo, "bar"), "function.*not TRUE")
})
