getMethods <- function(className, rdfObj){
  sparql <-  paste0('PREFIX sh:<http://www.w3.org/ns/shacl#>
PREFIX sd:<http://www.w3.org/ns/sparql-service-description#>
PREFIX void:<http://rdfs.org/ns/void#>
PREFIX void_ext:<http://ldf.fi/void-ext#>
SELECT DISTINCT  ?propIri ?datatypeTo ?classTo ?classFrom
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

  rdflib::rdf_query(rdfObj,sparql)

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

 rdflib::rdf_query(rdfObj, sparql)
}

loadFile <- function(fname){
  rdflib::rdf_parse(fname)
}


