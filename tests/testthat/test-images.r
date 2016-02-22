context("Images")

futile.logger::flog.threshold('WARN')

test_that("images work", {
  root <- 'foo/bar'
  toproot <- 'foo'

  ## Initialize files
  if(dir.exists(toproot))
    unlink(toproot, recursive = TRUE)
  if(!dir.exists(root))
    dir.create(root, recursive = TRUE)
  on.exit(if(dir.exists(toproot)) unlink(toproot, recursive = TRUE))

  App <- Grove$new()
  App$setRoot(root)

  App$registerImage(
    name='foo.pdf',
    create=function() {
      plot(1, 1)
    },
    type='pdf'
  )

  x <- App$getArtifact('foo.pdf')
  expect_true(file.exists(file.path(root, 'foo.pdf')))

})
