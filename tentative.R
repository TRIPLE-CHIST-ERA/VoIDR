library(rdflib)
setwd('/mnt/shareddisk/chist-era/')
rdfobj <- rdf_parse('rhea.ttl', format = 'turtle')

getAll <- 'PREFIX sh:<http://www.w3.org/ns/shacl#> 
PREFIX sd:<http://www.w3.org/ns/sparql-service-description#> 
PREFIX void:<http://rdfs.org/ns/void#>
PREFIX void_ext:<http://ldf.fi/void-ext#>
SELECT ?s ?p ?o
WHERE {?s ?p ?o}'
alldf <- rdf_query(rdfobj,getAll)

getClasses <- 'PREFIX sh:<http://www.w3.org/ns/shacl#> 
PREFIX sd:<http://www.w3.org/ns/sparql-service-description#> 
PREFIX void:<http://rdfs.org/ns/void#>
PREFIX void_ext:<http://ldf.fi/void-ext#>
SELECT  ?cp1 ?classIri
WHERE {
  ?graphIri sd:graph ?graph .
  ?s sd:graph ?graph .
  ?graph void:classPartition ?cp1 .
  ?cp1 void:class ?classIri.
}'
rhea <- rdf_query(rdfobj,getClasses)
unique(rhea$cp1)
attach(rhea)
rhea[grepl("#chebi!inverseOf",s),]
cname <- rhea$cp1[1]
cls <- rhea$classIri[1]
# for each in rhea$cp1

getMethods = paste0('PREFIX sh:<http://www.w3.org/ns/shacl#> 
PREFIX sd:<http://www.w3.org/ns/sparql-service-description#> 
PREFIX void:<http://rdfs.org/ns/void#>
PREFIX void_ext:<http://ldf.fi/void-ext#>
select ?propIri ?datatypeTo ?classTo ?classFrom
where {
  bind (<', cname, '> as ?cp1) 
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


m1  <- rdf_query(rdfobj,getMethods)
getAllInstances <- paste0('SELECT ?cpInstance WHERE { ?cpInstance a <',cls,'>}')
inst  <- SPARQL_query('https://sparql.rhea-db.org/sparql', getAllInstances)

#getSelectMethodForMyFirstObject <- 'PREFIX sh:<http://www.w3.org/ns/shacl#> 
#PREFIX sd:<http://www.w3.org/ns/sparql-service-description#> 
#PREFIX void:<http://rdfs.org/ns/void#>
#PREFIX void_ext:<http://ldf.fi/void-ext#>
#SELECT ?select {
#  BIND(<",propIri,"> AS ?propIri)
#  BIND(<",?cpInstance,"> as ?member)
#  ?member ?propIri ?select .
#}


getObjectList <- paste0('
SELECT ?classMembers 
WHERE {?classMembers a <"', cls, '">},
ORDER BY ?classMembers')


class SPARQLSelect:
	"""From class partion """
 
	"""the identifier of the instance e.g. https://sparql.rhea-db.org/.well-known/sparql-examples/1"""
	id="https://sparql.rhea-db.org/.well-known/sparql-examples/1"
	def __init__(self, id):
		self.id=id
 
	"""A static method that retrieves all instance of the class SPARQLSelect"
	instances(): 
		asIterator("select ?classMembers where {?classMembers a sh:SPARQLSelect} order by ?classMembers", (cm) -> new SPARQLSelect(cm))
 
	"""For a given that is in this object retrieve the values which are mathced by ?id shacl:select ?value"""
	def select:
		execute("select ?value where (<"+id+"> <http://www.w3.org/ns/m1#> ?value")
 
	def comment:
		execute("select ?value where (<"+id+"> <http://www.w3.org/1999/02/22-rdf-syntax-ns#comment> ?value}")
 
 
def asIterator(String sparqlSelect, Lambda createNewObject):
	return new Iterator(){
		boolean hasNext(){
}
 
		Object next() {
			return createNewObject.apply(result.next())
		}
	}
 


"select ?value where { <https://sparql.rhea-db.org/.well-known/sparql-example/1> <http://www.w3.org/1999/02/22-rdf-syntax-ns#comment> ?value}"