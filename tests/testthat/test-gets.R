test_that("I can parse a file", {
  suppressWarnings(rdfObj <- loadFile(fixTestFilePath('extdata/rhea.ttl')))
  assign('rdfObj', rdfObj, envir = parent.env(environment()))
  expect_equal(names(rdfObj), c('world', 'model', 'storage'))
})

test_that("I can get the class names", {
  suppressWarnings(cls <- getClasses(rdfObj))
  assign('cls', cls, envir = parent.env(environment()))
  expect_s3_class(cls, 'data.frame')
})

test_that("I can get the methods for one class", {
  suppressWarnings(mets <- getMethods(cls$cp1[1], rdfObj))
  expect_s3_class(mets, 'data.frame')
})

test_that("I can get the instances for one class (needs to connect to a remote endpoint)", {
  suppressWarnings(insts <- getInstances(cls$classIri[4],'https://sparql.rhea-db.org/sparql' ))
  expect_s3_class(insts, 'data.frame')
})


