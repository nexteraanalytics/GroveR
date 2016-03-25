context("Images")

futile.logger::flog.threshold('WARN')

test_that("images work", {
  root <- 'foo/bar'
  toproot <- dirname(root)

  ## Initialize files
  if(dir.exists(toproot))
    unlink(toproot, recursive = TRUE)
  if(!dir.exists(root))
    dir.create(root, recursive = TRUE)
  on.exit(if(dir.exists(toproot)) unlink(toproot, recursive = TRUE))

  App <- GroveR$new()
  App$setRoot(root)

  ## Image without args or type
  App$registerImage(
    name='baz.pdf',
    create=function() {
      plot(1, 1)
    }
  )
  x <- App$getArtifact('baz.pdf')
  expect_true(file.exists(file.path(root, 'baz.pdf')))


  ## Image without args
  App$registerImage(
    name='foo.pdf',
    create=function() {
      plot(1, 1)
    },
    type='pdf'
  )
  x <- App$getArtifact('foo.pdf')
  expect_true(file.exists(file.path(root, 'foo.pdf')))


  ## Image with args
  App$registerImage(
    name='bar.pdf',
    deps=c('a', 'b'),
    create=function(a, b) {
      plot(mean(a), mean(b))
    },
    type='pdf'
  )
  App$auto(a, 1:5)
  App$auto(b, 2:9)
  x <- App$getArtifact('bar.pdf')
  expect_true(file.exists(file.path(root, 'bar.pdf')))


  ## Image with args, implicitly declared
  App$registerImage(
    name='bam.pdf',
    create=function(d, e) {
      plot(mean(d), mean(e))
    }
  )
  App$auto(d, 1:5)
  App$auto(e, 2:9)
  x <- App$getArtifact('bam.pdf')
  expect_true(file.exists(file.path(root, 'bam.pdf')))
})
