## Copyright (c) 2016 Windlogics, Inc.
## See the DESCRIPTION file for licensing information.

context("Auto interfaces")

futile.logger::flog.threshold('WARN')

test_that("auto interfaces work", {
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
  `%auto%` <- App$auto

  long.term.projections %auto% function(geomodel.data, scada.met.clearsky.data, inverter.met.info) {
    list(g=length(geomodel.data),
         s=length(scada.met.clearsky.data),
         i=length(inverter.met.info))
  }

  geomodel.data %auto% 1:7
  scada.met.clearsky.data %auto% function() readRDS("scada.met.clearsky.data.rds")
  inverter.met.info %auto% function() readRDS("inverter.met.info.rds")

  registered <- App$artifactNames()
  expect_equal(sort(registered), c("geomodel.data", "inverter.met.info",
                                   "long.term.projections", "scada.met.clearsky.data"))


  res <- App$getArtifact('long.term.projections')

  expect_equal(res$g, 7)
  expect_equal(res$s, 9)
  expect_equal(res$i, 4)
})
