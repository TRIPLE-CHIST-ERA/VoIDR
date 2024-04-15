fixTestFilePath <- function(path){
  if(dir.exists('inst')){
      return(paste0('inst/', path))
  } else {
    return(path)
  }
}


sp <- 'select * WHERE {<http://rdf.rhea-db.org/GenericHeteropolysaccharide> ?p ?o}'
sp <- 'select * WHERE {?s ?p <http://rdf.rhea-db.org/GenericCompound> }'

ghp <- SPARQL_query('https://sparql.rhea-db.org/sparql', sp)
