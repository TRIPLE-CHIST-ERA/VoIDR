
getClasses_old <- function(voidFile = NULL, voidEndpoint = NULL, voidGraph = NULL){
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
    #cat(sparql)
    SPARQL_query(endpoint, sparql, use.POST = TRUE)
  }, simplify = FALSE)


}


expandDF <- function(df, sep='~~'){
  # "explode" the cardinality of df by creating one line per separated value
  apply(df, 1, strsplit, sep) %>% lapply(expand.grid) %>% Reduce(rbind,.) %>% tibble()
}



getClasses <- function( voidEndpoint = NULL, voidGraph = NULL){
  # only one of voidFile and voidEndpoint; favour voidEndpoint


  sparqlSuffix <-   "{
    SELECT
    ?property
    ( CONCAT( MAX( ?subject_cardinality ), '..', MAX( ?object_cardinality )) AS ?cardinalities )
    WHERE{
      {
        SELECT DISTINCT
        ?graph
        ?property
        ?distinct_subjects
        ?triples
        ?distinct_objects
        ( IF( ?distinct_subjects = ?triples, '1', 'n' ) AS ?subject_cardinality )
        ( IF( ?distinct_objects  = ?triples, '1', 'n' ) AS ?object_cardinality  )
        WHERE{ "
  if(!is.null(voidGraph)){
    sparqlSuffix <- paste0(sparqlSuffix, "\n", "BIND (<", voidGraph, "> AS ?graph)")
  }
  sparqlSuffix <- paste0(sparqlSuffix, "
         ?graph a sd:Graph ;
          void:propertyPartition ?property_partition .
          ?property_partition
          void:property         ?property          ;
          void:distinctSubjects ?distinct_subjects ;
          void:triples          ?triples           ;
          void:distinctObjects  ?distinct_objects  .
        }
      }
    }
    GROUP BY ?property

  }
}")
statements <- list()

  tempSparql <-  'PREFIX sh:<http://www.w3.org/ns/shacl#>
PREFIX sd:<http://www.w3.org/ns/sparql-service-description#>
PREFIX void:<http://rdfs.org/ns/void#>
PREFIX void_ext:<http://ldf.fi/void-ext#>
SELECT DISTINCT  ?classFrom  ?property ?datatypeTo ?cardinalities '

  statements$dataSparql <- paste0(tempSparql, "
where {

  ?cp1 void:class ?classFrom .
  ?cp1 void:propertyPartition ?pp1 .
  ?pp1 void:property ?property .
  ?pp1 void:triples ?triples .
  {
    ?pp1 void_ext:datatypePartition ?cp2 .
    ?cp2 void_ext:datatype ?datatypeTo .
  }", sparqlSuffix)


 statements$objectSparql <- paste0(tempSparql,  "
where {
  ?cp1 void:class ?classFrom .
  ?cp1 void:propertyPartition ?pp1 .
  ?pp1 void:property ?property .
  ?pp1 void:triples ?triples .
  {
    ?pp1 void:classPartition ?cp2 .
    ?cp2 void:class ?classTo .
    ?graph void:classPartition ?cp3 .
    ?cp3 void:class ?classTo .
  } ", sparqlSuffix)


  sapply(names(statements), function(sparql){
    #cat(statements[[sparql]])
    if(!is.null(voidEndpoint)){
      voidEndpoint %>% SPARQL_query(statements[[sparql]])
    } else {
      voidFile %>% fixTestFilePath %>% loadFile %>% rdflib::rdf_query(sparql)
    }
  }, simplify = FALSE)
}


getMethods <- function(clsName, propList){
  dt <- propList$dataSparql
  dt <- dt[dt$classFrom == clsName,]
  cl <- propList$objectSparql
  cl <- cl[cl$classFrom == clsName,]
  list(dataProperties = list(unique = dt[dt$cardinalities %in% c('1..1', '1..n'), c('property', 'datatypeTo')], nonunique = dt[dt$cardinalities %in% c('n..1', 'n..n'), c('property', 'datatypeTo')] ),
       objectProperties = list(unique = cl[cl$cardinalities %in% c('1..1', '1..n'), 'property'], nonunique = cl[cl$cardinalities %in% c('n..1', 'n..n'), 'property'] )
  )

}
