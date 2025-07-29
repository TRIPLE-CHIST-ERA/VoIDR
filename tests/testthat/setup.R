fix_test_file_path <- function(path) {
  if (dir.exists("inst")) {
    paste0("inst/", path)
  } else {
    path
  }
}

#sp <- 'select * WHERE {<http://rdf.rhea-db.org/GenericHeteropolysaccharide> ?p ?o}'
#sp <- 'select * WHERE {?s ?p <http://rdf.rhea-db.org/GenericCompound> }'
#ghp <- sparql_query('https://sparql.rhea-db.org/sparql', sp)
