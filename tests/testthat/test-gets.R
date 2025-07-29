

test_that("I can get the class names", {
  cls <- getClasses(voidEndpoint = 'http://localhost:7200/repositories/uniprotvoid', voidGraph = 'https://sparql.uniprot.org/.well-known/void#_graph_uniprot')
  cls2 <- getClasses(voidEndpoint = 'http://localhost:7200/repositories/uniprotvoid')

  assign('cls', cls, envir = parent.env(environment()))
  expect_equal(names(cls), c('literalSparql', 'iriSparql'))
})

test_that("I can get the methods for one class", {

  mets <- getMethods('http://purl.uniprot.org/core/Book_Citation', cls)
  expect_equal(names(mets), c('literalProperties', 'iriProperties'))
})



test_that("I can make a package with a void endpoint)", {

  #f <- make_package('BeatlesR','http://localhost:7200/repositories/beatles', voidEndpoint  = 'http://localhost:7200/repositories/beatles', voidGraph = 'http://example.org/void')
  f <- make_package('UniprotR','https://sparql.uniprot.org', voidEndpoint = 'http://localhost:7200/repositories/uniprotvoid', voidGraph = 'https://sparql.uniprot.org/.well-known/void#_graph_uniprot')
 # f<- make_package('RheaR','https://sparql.rhea-db.org/sparql', voidEndpoint = 'https://sparql.rhea-db.org/sparql')
#  f<- make_package('IDSMR','https://idsm.elixir-czech.cz/sparql', voidEndpoint = 'http://localhost:7200/repositories/IDSM_void')
  #f2 <- make_package('UniprotR2','https://sparql.uniprot.org', voidEndpoint = 'http://localhost:7200/repositories/uniprotvoid')

  expect_true(file.exists(f))
  unlink(f)
})
# iriProperties -> objectProperties
# literalProperties -> dataProperties (to check OWL nomenclature)
# todo: deduplicate the unique dataframe(s)
# todo: implement filters
# return rather a dataframe for feeding into %>% ?


PREFIX skos: <http://www.w3.org/2004/02/skos/core#>
SELECT *
  WHERE {
    {SELECT ?Taxon
      WHERE { ?Taxon a <http://purl.uniprot.org/core/Taxon>}
      LIMIT 1000

    }
    OPTIONAL{ ?Taxon skos:narrowerTransitive ?narrowerTransitive}
    OPTIONAL{ ?Taxon ^skos:narrowerTransitive ?narrowerTransitive2 }
    OPTIONAL{ ?narrowerTransitive2 skos:narrowerTransitive ?Taxon }
    }

