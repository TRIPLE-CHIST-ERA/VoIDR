PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX skos: <http://www.w3.org/2004/02/skos/core#>

PREFIX void: <http://rdfs.org/ns/void#>
SELECT DISTINCT
?property
?label # short no space +=. column name
( COALESCE( ?desc_1, ?desc_3, ?desc_2, "" ) AS ?description )
WHERE{
  SERVICE <https://sparql.uniprot.org/sparql> {
    ?any void:property ?property
    OPTIONAL{ ?property rdfs:comment ?desc_1 }
    OPTIONAL{ ?property rdfs:label ?desc_3 }
    OPTIONAL{ ?property skos:prefLabel ?desc_3 }
    BIND( CONCAT( "Description of ", REPLACE( STR( ?property ), ".+[#/](\\w+$)", "$1" )) AS ?desc_2 )
  }
}
