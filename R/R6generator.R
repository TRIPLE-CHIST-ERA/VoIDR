.getClasses <- function(endpoint){
  dataSparql <- 'PREFIX sh:<http://www.w3.org/ns/shacl#>
PREFIX sd:<http://www.w3.org/ns/sparql-service-description#>
PREFIX void:<http://rdfs.org/ns/void#>
PREFIX void_ext:<http://ldf.fi/void-ext#>
SELECT DISTINCT   ?classFrom  ?property ?datatypeTo ?cardinalities
where {
  ?cp1 void:class ?classFrom .
  ?cp1 void:propertyPartition ?pp1 .
  ?pp1 void:property ?property .
  ?pp1 void:triples ?triples .
  {
    ?pp1 void_ext:datatypePartition ?cp2 .
    ?cp2 void_ext:datatype ?datatypeTo .
  }

  {
    SELECT
    ?property
    ( CONCAT( MAX( ?subject_cardinality ), "..", MAX( ?object_cardinality )) AS ?cardinalities )
    WHERE{
      {
        SELECT DISTINCT
        ?graph
        ?property
        ?distinct_subjects
        ?triples
        ?distinct_objects
        ( IF( ?distinct_subjects = ?triples, "1", "n" ) AS ?subject_cardinality )
        ( IF( ?distinct_objects  = ?triples, "1", "n" ) AS ?object_cardinality  )
        WHERE{
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
} ORDER BY ?classFrom  ?property ?cardinalities'
  #endpoint <- 'http://localhost:7200/repositories/beatles'
  classDF <- SPARQL_query(endpoint, dataSparql)

}

xxx <- R6Class("Person",
                  public = list(
                    name = NULL,
                    age = NULL,

                    initialize = function(name = NA, age = NA) {
                      self$name <- name
                      self$age <- age
                    },

                    say_hello = function() {
                      cat("Hello, my name is", self$name, "and I am", self$age, "years old.\n")
                    }
                  )
)


Employee <- R6Class("Employee",
                    inherit = xxx,
                    public = list(
                      job_title = NULL,

                      initialize = function(name, age, job_title) {
                        # Call to parent constructor
                        super$initialize(name, age)
                        self$job_title <- job_title
                      },

                      introduce = function() {
                        cat("I am", self$name, ", I work as a", self$job_title, ".\n")
                      }
                    )
)

generateClass <- function(className, dataProps){
  cls <- R6Class(className,
                 uniqueData = function(dataProperties = dataProps, rowLimit = NULL){

                 },
                 private = list(
                      dt,
                      sparql =
                 ),
                 initialize = function(){
                   private$dt <- data.frame(nrow = 0, ncol = length(dataProps))
                   colnames(dt) <- dataProps
                 }

                 )
}


endpoint <- 'http://localhost:7200/repositories/beatles'
