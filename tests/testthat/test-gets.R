

test_that("I can get the class names", {
  cls <- getClasses(voidEndpoint = 'http://localhost:7200/repositories/uniprotvoid', voidGraph = 'https://sparql.uniprot.org/.well-known/void#_graph_uniprot')
  assign('cls', cls, envir = parent.env(environment()))
  expect_equal(names(cls), c('literalSparql', 'iriSparql'))
})

test_that("I can get the methods for one class", {

  mets <- getMethods('http://purl.uniprot.org/core/Book_Citation', cls)
  expect_equal(names(mets), c('literalProperties', 'iriProperties'))
})



test_that("I can make a package with a void endpoint)", {

  #f <- makePackage('BeatlesR','http://localhost:7200/repositories/beatles', voidEndpoint  = 'http://localhost:7200/repositories/beatles', voidGraph = 'http://example.org/void')
  f <- makePackage('UniprotR','https://sparql.uniprot.org', voidEndpoint = 'http://localhost:7200/repositories/uniprotvoid')
  expect_true(file.exists(f))
  unlink(f)
})



