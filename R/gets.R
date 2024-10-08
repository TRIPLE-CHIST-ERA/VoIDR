

getMethods <- function(className, voidFile = NULL, voidEndpoint = NULL, voidGraph = NULL){
  # only one of voidFile and voidEndpoint; favour voidEndpoint
 if(is.null(voidEndpoint)){
   voidGraph <- NULL
 }

sparql <-  'PREFIX sh:<http://www.w3.org/ns/shacl#>
PREFIX sd:<http://www.w3.org/ns/sparql-service-description#>
PREFIX void:<http://rdfs.org/ns/void#>
PREFIX void_ext:<http://ldf.fi/void-ext#>
SELECT DISTINCT  ?propIri ?datatypeTo ?classTo ?classFrom '
if(!is.null(voidGraph)){
  sparql <- paste0(sparql, 'FROM <', voidGraph, '> ')
}
sparql <- paste0(sparql, '
where {
  bind (<', className, '> as ?cp1)
  ?cp1 void:class ?classFrom .
  ?cp1 void:propertyPartition ?pp1 .
  ?pp1 void:property ?propIri .
  ?pp1 void:triples ?triples .
  {
    ?pp1 void_ext:datatypePartition ?cp2 .
    ?cp2 void_ext:datatype ?datatypeTo .
  } UNION {
    ?pp1 void:classPartition ?cp2 .
    ?cp2 void:class ?classTo .
    ?graph void:classPartition ?cp3 .
    ?cp3 void:class ?classTo .
  }
  } ')
 if(!is.null(voidEndpoint)){
   voidEndpoint %>% SPARQL_query(sparql) %>% return
 } else {
   voidFile %>% fixTestFilePath %>% loadFile %>% rdflib::rdf_query(sparql) %>% return
 }
}

getInstances <- function(classIri, endpoint){
  sparql <- paste0('SELECT ?cpInstance WHERE { ?cpInstance a <',classIri,'>}')
  SPARQL_query(endpoint, sparql)
}

getClasses <- function(voidFile = NULL, voidEndpoint = NULL, voidGraph = NULL){
  # only one of voidFile and voidEndpoint; favour voidEndpoint
  if(is.null(voidEndpoint)){
    voidGraph <- NULL
  }
 sparql <- 'PREFIX sh:<http://www.w3.org/ns/shacl#>
PREFIX sd:<http://www.w3.org/ns/sparql-service-description#>
PREFIX void:<http://rdfs.org/ns/void#>
PREFIX void_ext:<http://ldf.fi/void-ext#>
SELECT DISTINCT ?voidName  ?classIri '
 if(!is.null(voidGraph)){
   sparql <- paste0(sparql, 'FROM <', voidGraph, '> ')
 }
 sparql <- paste0(sparql, '
where {
  ?voidName void:class ?classIri .
  ?voidName void:propertyPartition ?pp1 .
  ?pp1 void:property ?propIri .
  ?pp1 void:triples ?triples .
  {
    ?pp1 void_ext:datatypePartition ?cp2 .
    ?cp2 void_ext:datatype ?datatypeTo .
  }
  } ')

 if(!is.null(voidEndpoint)){
   voidEndpoint %>% SPARQL_query(sparql) %>% return
 } else {
   voidFile %>% fixTestFilePath %>% loadFile %>% rdflib::rdf_query(sparql) %>% return
 }
}

getDescriptions <- function(filters = list('class' = NULL, 'property' = NULL), endpoint){
  sapply(names(filters), function(x){
    sparql <- paste0(
      'PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
       PREFIX void: <http://rdfs.org/ns/void#>
       PREFIX skos: <http://www.w3.org/2004/02/skos/core#>
       SELECT DISTINCT
       ?entity
      (COALESCE( ?desc_1, ?desc_2, ?desc_3, "-") AS ?description )
      WHERE{
      ?any void:', x, ' ?entity ')
    if(!is.null(filters[[x]])) {
      charFilter <- paste(unique(filters[[x]]), collapse='> <')
      sparql <- paste0(sparql , '
                       VALUES ?entity { <', charFilter, '> }')
    }
    sparql <- paste0(sparql , '
    OPTIONAL{ ?entity rdfs:comment ?desc_1 }
    OPTIONAL{ ?entity rdfs:label ?desc_2 }
    OPTIONAL{ ?entity skos:prefLabel ?desc_3 }
    }')
    SPARQL_query(endpoint, sparql, use.POST = TRUE)
  }, simplify = FALSE)


}

loadFile <- function(fname){
  rdflib::rdf_parse(fname)
}

expandDF <- function(df, sep='~~'){
  # "explode" the cardinality of df by creating one line per separated value
  apply(df, 1, strsplit, sep) %>% lapply(expand.grid) %>% Reduce(rbind,.) %>% tibble()
}


fixTestFilePath <- function(path){
  if(dir.exists('inst')){
      return(paste0('inst/', path))
  } else {
    return(path)
  }
}

