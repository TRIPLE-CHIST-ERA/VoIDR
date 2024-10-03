

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

getDescriptions <- function(type = c('class', 'property'), endpoint){
  sapply(type, function(x){
    sparql <- paste0(
      'PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
       PREFIX void: <http://rdfs.org/ns/void#>
       SELECT DISTINCT
       ?entity
       ?label
      (COALESCE( ?desc_1, ?desc_2) AS ?description )
      WHERE{
      ?any void:', type, ' ?entity .
      ?entity rdfs:comment ?desc_1 .
      ?entity rdfs:label ?desc_2
    }')
    SPARQL_query(endpoint, sparql)
  }, simplify = FALSE)

#  BIND( CONCAT( "Description of ", REPLACE( STR( ?property ), ".+[#/](\\w+$)", "$1" )) AS ?desc_2 )

}

loadFile <- function(fname){
  rdflib::rdf_parse(fname)
}

expandDF <- function(df, sep='~~'){
  # "explode" the cardinality of df by creating one line per separated value
  apply(df, 1, strsplit, sep) %>% lapply(expand.grid) %>% Reduce(rbind,.) %>% tibble()
}



