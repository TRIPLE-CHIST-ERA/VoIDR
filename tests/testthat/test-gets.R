test_that("I can parse a file", {
  suppressWarnings(rdfObj <- loadFile(fixTestFilePath('extdata/rhea.ttl')))
  suppressWarnings(rdfObj <- loadFile(fixTestFilePath('extdata/void.rdf')))
  assign('rdfObj', rdfObj, envir = parent.env(environment()))
  expect_equal(names(rdfObj), c('world', 'model', 'storage'))
})

test_that("I can get the class names", {
  suppressWarnings(cls <- getClasses(rdfObj))
  cls <- getClasses(voidEndpoint = 'https://sparql.uniprot.org')
  assign('cls', cls, envir = parent.env(environment()))
  expect_s3_class(cls, 'data.frame')
})

test_that("I can get the methods for one class", {
  suppressWarnings(mets <- getMethods(cls$voidName[1], rdfObj))
  mets <- getMethods(className = 'https://sparql.uniprot.org/.well-known/void#uniprot!External_Sequence', voidEndpoint = 'https://sparql.uniprot.org')
  expect_s3_class(mets, 'data.frame')
})

test_that("I can get the instances for one class (needs to connect to a remote endpoint)", {
  print(cls[4,])
 # suppressWarnings(insts <- getInstances(cls$classIri[4],'https://sparql.rhea-db.org/sparql' ))
  suppressWarnings(class4 <- getEntity(cls$voidName[1], cls$classIri[1], rdfObj, 'https://sparql.rhea-db.org/sparql' ))
  expect_s3_class(insts, 'data.frame')
})

test_that("I can make a package with a void file)", {

  f <- makePackage('BeatlesR','http://localhost:7200/repositories/beatles', voidFile = 'extdata/void.rdf')
  expect_true(file.exists(f))
  unlink(f)
})

test_that("I can make a package with a void endpoint)", {

  #f <- makePackage('BeatlesR','http://localhost:7200/repositories/beatles', voidEndpoint  = 'http://localhost:7200/repositories/beatles', voidGraph = 'http://example.org/void')
  f <- makePackage('UniprotR','https://sparql.uniprot.org', voidEndpoint  = 'https://sparql.uniprot.org')
  expect_true(file.exists(f))
  unlink(f)
})

test_that("I can get the descriptions)", {

  #desc <- getDescriptions(endpoint = 'http://localhost:7200/repositories/beatles')
  desc <- getDescriptions(filters = list(class = 'http://purl.uniprot.org/core/Book_Citation'), endpoint = 'https://sparql.uniprot.org')
  desc <- getDescriptions(filters = list(class = 'http://purl.uniprot.org/core/Book_Citation', property = mets$propIri), endpoint = 'https://sparql.uniprot.org')
  expect_true(file.exists(f))
  unlink(f)
})


allEntities <- apply(cls,1, function(x) getEntity(x[1], x[2], rdfObj, 'http://localhost:7200/repositories/beatles'), simplify = FALSE)
#x <- allEntities[[2]][1:1000,]
names(allEntities) <- lapply(cls$classIri, function(x)sub('.*[/|#]', '',x))
x <- allEntities[[1]]
#getEntity(cls[1,1], cls[1,2], rdfObj, 'http://localhost:7200/repositories/beatles')

getCls <-  paste0('PREFIX sh:<http://www.w3.org/ns/shacl#>
PREFIX sd:<http://www.w3.org/ns/sparql-service-description#>
PREFIX void:<http://rdfs.org/ns/void#>
PREFIX void_ext:<http://ldf.fi/void-ext#>
SELECT DISTINCT ?cp1,  ?classIri
where {
  ?cp1 void:class ?classIri .
  ?cp1 void:propertyPartition ?pp1 .
  ?pp1 void:property ?propIri .
  ?pp1 void:triples ?triples .
  {
    ?pp1 void_ext:datatypePartition ?cp2 .
    ?cp2 void_ext:datatype ?datatypeTo .
  }
  } ')

x <- rdflib::rdf_query(rdfObj, getCls)

cName <- x[8,1]
cName
cIri <- x[8,2]
cIri
mets <- getMethods(cName, rdfObj)
mets
propFilter <- paste(mets$propIri, collapse='>, <')
sparql <-  paste0('SELECT *
                  WHERE {
                    ?cpInstance a <',cIri,'> .
                    ?cpInstance ?p ?value
                    FILTER(?p IN (<', propFilter, '>))
                  }')
endpoint <-'https://sparql.rhea-db.org/sparql'
long_df <- SPARQL_query(endpoint, sparql)
wide_df <- tidyr::pivot_wider(long_df, id_cols= 1, names_from = 'p', values_from= 'value', values_fn = function(x)paste(x, collapse= ', '))
colnames(wide_df) <- sapply(colnames(wide_df), function(x) sub('.*#','',x))
str(wide_df)

y <- reshape2::dcast(x, Class~p, value.var = 'value', fun.aggregate = function(x)paste(x, collapse= ', '))
y <- tidyr::pivot_wider(x, id_cols= 1, names_from = 'p', values_from= 'value', values_fn = function(x)paste(x, collapse= ', '))


g <- makeOneFunction(cls[1,1], cls[1,2], rdfObj, 'http://localhost:7200/repositories/beatles')
package.skeleton(name='testBeatles', list = c("getSong"))
