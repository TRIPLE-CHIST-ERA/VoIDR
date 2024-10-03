

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
   return(SPARQL_query(voidEndpoint, sparql))
 } else {
   voidFile %>% fixTestFilePath %>% loadFile %>% rdflib::rdf_query(sparql) %>% return
 }
}

getInstances <- function(classIri, endpoint){
  sparql <- paste0('SELECT ?cpInstance WHERE { ?cpInstance a <',classIri,'>}')
  SPARQL_query(endpoint, sparql)
}

getClasses <- function(rdfObj){
# sparql <- 'PREFIX sh:<http://www.w3.org/ns/shacl#>
#  PREFIX sd:<http://www.w3.org/ns/sparql-service-description#>
#  PREFIX void:<http://rdfs.org/ns/void#>
#  PREFIX void_ext:<http://ldf.fi/void-ext#>
#  SELECT  ?cp1 ?classIri
#  WHERE {
#    ?graphIri sd:graph ?graph .
#    ?s sd:graph ?graph .
#    ?graph void:classPartition ?cp1 .
#    ?cp1 void:class ?classIri.
#  }'

 sparql <- paste0('PREFIX sh:<http://www.w3.org/ns/shacl#>
PREFIX sd:<http://www.w3.org/ns/sparql-service-description#>
PREFIX void:<http://rdfs.org/ns/void#>
PREFIX void_ext:<http://ldf.fi/void-ext#>
SELECT DISTINCT ?voidName,  ?classIri
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

 rdflib::rdf_query(rdfObj, sparql)
}

loadFile <- function(fname){
  rdflib::rdf_parse(fname)
}
getEntity <- function(voidName, classIri, voidObj, endpoint){

  mets <- getMethods(voidName, voidObj)
  propFilter <- paste(unique(mets$propIri), collapse='> <')# some methods appear multiple times (different classTo), hence the unique
  primaryColName <- sub('.*[/|#]','',classIri)
#  sparql <-  paste0('SELECT *
#                  WHERE {
#                    ?',primaryColName,' a <',classIri,'> .
#                    ?',primaryColName,' ?p ?value
#                    FILTER(?p IN (<', propFilter, '>))
#                  }')

  sparql <-  paste0('SELECT *
                  WHERE {
                    ?',primaryColName,' a <',classIri,'> .
                     VALUES ?p { <', propFilter, '> }
                    ?',primaryColName,' ?p ?value
                  }')


#  sparql <-  paste0('SELECT *
#                  WHERE {
#  {
#    select ?P
#    WHERE{
#      VAULES ?p { <', propFilter, '> }
#    }
#  }
#    ?',primaryColName,' a <',classIri,'> .
#                    ?',primaryColName,' ?p ?value
# }
  #### utilise VALUES
  cat(sparql)
  #sparql <-  paste0('SELECT *
  #                WHERE {
  #                  ?cpInstance a <',classIri,'> .
  #                  ?cpInstance ?p ?value
  #                  FILTER(?p IN (<', propFilter, '>))
  #                }')
  long_df <- SPARQL_query(endpoint, sparql)
  if(is.null(long_df)){
    return(NULL)
  }
  wide_df <- tidyr::pivot_wider(long_df, id_cols= 1, names_from = 'p', values_from= 'value', values_fn = function(x)paste(x, collapse= '~~'))
  colnames(wide_df) <- sapply(colnames(wide_df), function(x) sub('.*[/|#]','',x))
  return(wide_df)
}

expandDF <- function(df, sep='~~'){
  # "explode" the cardinality of df by creating one line per separated value
  apply(df, 1, strsplit, sep) %>% lapply(expand.grid) %>% Reduce(rbind,.) %>% tibble()
}



