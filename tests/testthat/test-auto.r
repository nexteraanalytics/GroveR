context("R interfaces")

SolarOPADeps <- Biome$new()
SolarOPADeps$setRoot('foo/bar')

`%auto%` <- SolarOpaApp$registerFunction

long.term.projections %auto% function(geomodel.data, scada.met.clearsky.data, inverter.met.info) {
  list(g=length(geomodel.data),
       s=length(scada.met.clearsky.data),
       i=length(inverter.met.info))
}

geomodel.data %auto% function() 1:7
scada.met.clearsky.data %auto% readRDS("foo/bar/scada.met.clearsky.data.rds")
inverter.met.info %auto% readRDS("foo/bar/inverter.met.info.rds")

if(!dir.exists('foo/bar'))
  dir.create('foo/bar', recursive = TRUE)
saveRDS(1:9, "foo/bar/scada.met.clearsky.data.rds")
saveRDS(1:4, "foo/bar/inverter.met.info.rds")

res <- SolarOPADeps$getArtifact('long.term.projections')

testthat::expect_equal(res$g, 7)
testthat::expect_equal(res$s, 9)
testthat::expect_equal(res$i, 4)
