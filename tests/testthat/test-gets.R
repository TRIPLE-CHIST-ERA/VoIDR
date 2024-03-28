test_that("multiplication works", {
  expect_equal(2 * 2, 4)
})

rdfobj <- loadFile('inst/extdata/rhea.ttl')
cls <- getClasses(rdfobj)
cls
meths <- sapply(cls$cp1, getMethods, rdfobj)
instances <- sapply(cls$classIri, getInstances, 'https://sparql.rhea-db.org/sparql' )

getInstancesAll <- function(classIri, endpoint){
  sparql <- paste0('SELECT ?cpInstance $p $o WHERE { ?cpInstance a <',classIri,'>.
                                                     ?cpInstance $p $o
                                                }')
  SPARQL_query(endpoint, sparql)
}
x <-getInstances('http://www.w3.org/ns/sparql-service-description#Dataset', 'https://sparql.rhea-db.org/sparql')
x
y <- getMethods('https://sparql.rhea-db.org/.well-known/void#void!Graph', rdfobj)
cls
meths
