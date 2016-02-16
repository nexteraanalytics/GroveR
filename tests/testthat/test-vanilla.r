context("Vanilla interfaces")

SolarOPADeps <- R6Class(
  "SolarOPADeps",
  inherit = Biome)

SolarOPADeps <- Biome$new()

SolarOPADeps$registerRDSArtifact(
  name="long.term.projections",
  deps=c("geomodel.data", "scada.met.clearsky.data", "inverter.met.info"),
  create=function(geomodel.data, scada.met.clearsky.data, inverter.met.info) {
    list(g=length(geomodel.data),
         s=length(scada.met.clearsky.data),
         i=length(inverter.met.info))
  },
  path="foo/bar/ltp.rds")

SolarOPADeps$registerRDSArtifact(
  name="geomodel.data",
  create=function() {
    1:7
  },
  path="foo/bar/geomodel.data.rds")

SolarOPADeps$registerStaticFileArtifact(
  name="scada.met.clearsky.data",
  path="foo/bar/scada.met.clearsky.data.rds")

SolarOPADeps$registerStaticFileArtifact(
  name="inverter.met.info",
  path="foo/bar/inverter.met.info.rds")

if(!dir.exists('foo/bar'))
  dir.create('foo/bar', recursive = TRUE)
saveRDS(1:9, "foo/bar/scada.met.clearsky.data.rds")
saveRDS(1:4, "foo/bar/inverter.met.info.rds")

res <- SolarOPADeps$getArtifact('long.term.projections')

testthat::expect_equal(res$g, 7)
testthat::expect_equal(res$s, 9)
testthat::expect_equal(res$i, 4)
