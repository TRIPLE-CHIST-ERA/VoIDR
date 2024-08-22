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
  suppressWarnings(mets <- getMethods(cls$cp1[4], rdfObj))
  expect_s3_class(mets, 'data.frame')
})

test_that("I can get the instances for one class (needs to connect to a remote endpoint)", {
  print(cls$classIri[4])
  suppressWarnings(insts <- getInstances(cls$classIri[4],'https://sparql.rhea-db.org/sparql' ))
  expect_s3_class(insts, 'data.frame')
})



sparql <-  paste0('PREFIX sh:<http://www.w3.org/ns/shacl#>
PREFIX sd:<http://www.w3.org/ns/sparql-service-description#>
PREFIX void:<http://rdfs.org/ns/void#>
PREFIX void_ext:<http://ldf.fi/void-ext#>
SELECT DISTINCT   ?classFrom
where {
  ?cp1 void:class ?classFrom .
  ?cp1 void:propertyPartition ?pp1 .
  ?pp1 void:property ?propIri .
  ?pp1 void:triples ?triples .
  {
    ?pp1 void_ext:datatypePartition ?cp2 .
    ?cp2 void_ext:datatype ?datatypeTo .
  }
  } ')

x <- rdflib::rdf_query(rdfObj, sparql)

cName <- x[1,1]
cName

sparql <-  paste0('SELECT *
                  WHERE {
                    ?cpInstance a <',cName,'> .
                    ?cpInstance ?p $o .
                  }')
endpoint <-'https://sparql.rhea-db.org/sparql'
SPARQL_query(endpoint, sparql)
