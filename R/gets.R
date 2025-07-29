
get_classes_old <- function(
  void_file = NULL,
  void_endpoint = NULL,
  void_graph = NULL
) {
  # Only one of voidFile and voidEndpoint; favour voidEndpoint
  if (is.null(void_endpoint)) {
    void_graph <- NULL
  }
  sparql <- "PREFIX sh:<http://www.w3.org/ns/shacl#>
PREFIX sd:<http://www.w3.org/ns/sparql-service-description#>
PREFIX void:<http://rdfs.org/ns/void#>
PREFIX void_ext:<http://ldf.fi/void-ext#>
SELECT DISTINCT ?voidName  ?classIri "

  if (!is.null(void_graph)) {
    sparql <- paste0(sparql, "FROM <", void_graph, "> ")
  }
  sparql <- paste0(
    sparql,
    "WHERE {
    ?voidName void:class ?classIri .
    ?voidName void:propertyPartition ?pp1 .
    ?pp1 void:property ?propIri .
    ?pp1 void:triples ?triples .
    {
      ?pp1 void_ext:datatypePartition ?cp2 .
      ?cp2 void_ext:datatype ?datatypeTo .
    }
    } "
  )

  if (!is.null(void_endpoint)) {
    void_endpoint %>%
      sparql_query(sparql) %>%
      return
  } else {
    void_file %>%
      fix_test_file_path %>%
      loadFile %>%
      rdflib::rdf_query(sparql) %>%
      return
  }
}

get_descriptions <- function(
  filters = list("class" = NULL, "property" = NULL),
  endpoint
) {
  sapply(
    names(filters),
    function(x) {
      #    sparql <- paste0(
      #      'PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
      #       PREFIX void: <http://rdfs.org/ns/void#>
      #       PREFIX skos: <http://www.w3.org/2004/02/skos/core#>
      #       SELECT DISTINCT
      #       ?entity
      #      (COALESCE( ?desc_1, ?desc_2, ?desc_3, "-") AS ?description )
      #      WHERE{
      #      ?any void:', x, ' ?entity ')
      #    if(!is.null(filters[[x]])) {
      #      charFilter <- paste(unique(filters[[x]]), collapse='> <')
      #      sparql <- paste0(sparql , '
      #                       VALUES ?entity { <', charFilter, '> }')
      #    }
      #    sparql <- paste0(sparql , '
      #    OPTIONAL{ ?entity rdfs:comment ?desc_1 }
      #    OPTIONAL{ ?entity rdfs:label ?desc_2 }
      #    OPTIONAL{ ?entity skos:prefLabel ?desc_3 }
      #    }')
      sparql <- paste0(
        "PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
        PREFIX void: <http://rdfs.org/ns/void#>
        PREFIX skos: <http://www.w3.org/2004/02/skos/core#>
        SELECT
        ?entity
        (MIN(COALESCE( ?desc_1, ?desc_2, ?desc_3, "-") ) AS ?description)
        WHERE{
        ?any void:", x, " ?entity ")

      if (!is.null(filters[[x]])) {
        char_filter <- paste(unique(filters[[x]]), collapse="> <")
        sparql <- paste0(sparql, "VALUES ?entity { <", char_filter, "> }")
      }
      sparql <- paste0(
        sparql,
        "OPTIONAL{ ?entity rdfs:comment ?desc_1 }
        OPTIONAL{ ?entity rdfs:label ?desc_2 }
        OPTIONAL{ ?entity skos:prefLabel ?desc_3 }
        } GROUP BY ?entity"
      )

      #cat(sparql)
      sparql_query(endpoint, sparql, use_post = TRUE)
    },
    simplify = FALSE
  )
}


expand_df <- function(df, sep = "~~") {
  # "explode" the cardinality of df by creating one line per separated value
  apply(df, 1, strsplit, sep) %>%
    lapply(expand.grid) %>%
    Reduce(rbind, .) %>%
    tibble::tibble()
}


# only one of voidFile and voidEndpoint; favour voidEndpoint
get_classes <- function(void_endpoint = NULL, void_graph = NULL) {
  sparql_suffix <- "{
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

  if (!is.null(void_graph)) {
    sparql_suffix <- paste0(
      sparql_suffix,
      "\n",
      "BIND (<", void_graph, "> AS ?graph)"
    )
  }

  sparql_suffix <- paste0(
    sparql_suffix,
    "
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
    }"
  )

  tmp_sparql <- "PREFIX sh:<http://www.w3.org/ns/shacl#>
PREFIX sd:<http://www.w3.org/ns/sparql-service-description#>
PREFIX void:<http://rdfs.org/ns/void#>
PREFIX void_ext:<http://ldf.fi/void-ext#>
SELECT DISTINCT  ?classFrom  ?property ?datatypeTo ?cardinalities "

  statements <- list()
  statements$dataSparql <- paste0(
    tmp_sparql,
    "WHERE {
      ?cp1 void:class ?classFrom .
      ?cp1 void:propertyPartition ?pp1 .
      ?pp1 void:property ?property .
      ?pp1 void:triples ?triples .
      {
        ?pp1 void_ext:datatypePartition ?cp2 .
        ?cp2 void_ext:datatype ?datatypeTo .
      }",
    sparql_suffix
  )
  statements$objectSparql <- paste0(
    tmp_sparql,
    "WHERE {
      ?cp1 void:class ?classFrom .
      ?cp1 void:propertyPartition ?pp1 .
      ?pp1 void:property ?property .
      ?pp1 void:triples ?triples .
      {
        ?pp1 void:classPartition ?cp2 .
        ?cp2 void:class ?classTo .
        ?graph void:classPartition ?cp3 .
        ?cp3 void:class ?classTo .
      } ",
    sparql_suffix
  )

  sapply(
    names(statements),
    function(item_name) {
      #cat(statements[[item_name]])
      if (!is.null(void_endpoint)) {
        sparql_query(endpoint = void_endpoint, statements[[item_name]])
      } else {
        # NOT IMPLEMENTED CORRECTLY YET.
        #rdflib::rdf_query(loadFile(fix_test_file_path(void_file)), item_name)

        void_file %>%
          fix_test_file_path %>%
          loadFile %>%
          rdflib::rdf_query(item_name)
      }
    },
    simplify = FALSE
  )
}

get_methods <- function(cls_name, prop_list) {
  dt <- prop_list$dataSparql
  dt <- dt[dt$classFrom == cls_name, ]
  cl <- prop_list$objectSparql
  cl <- cl[cl$classFrom == cls_name, ]
  list(
    dataProperties = list(
      unique = dt[
        dt$cardinalities %in% c("1..1", "1..n"),
        c("property", "datatypeTo")
      ],
      nonunique = dt[
        dt$cardinalities %in% c("n..1", "n..n"),
        c("property", "datatypeTo")
      ]
    ),
    objectProperties = list(
      unique = cl[cl$cardinalities %in% c("1..1", "1..n"), "property"],
      nonunique = cl[cl$cardinalities %in% c("n..1", "n..n"), "property"]
    )
  )

}
