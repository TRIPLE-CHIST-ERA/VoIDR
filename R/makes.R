makeOneFunction <- function(className,classIri,  rdfObj, endpoint){
  print(className)
  props <- getMethods(unclass(className), rdfObj)
  propFilter <- paste(unique(props$propIri), collapse='> <')
  shortName <- sub('.*[/|#]','',classIri)
  #
  func <- paste0("function(properties = c(", paste(props$propIri,collapse=', '),"){
    sparql <-  paste0('SELECT *
                  WHERE {
                    ?',",shortName, ",' a <',", classIri, ",'> .
                     VALUES ?p { <',", propFilter,", '> }
                    ?',",shortName, ",' ?p ?value
                  }')
    long_df <- SPARQL_query('",endpoint,"', sparql)
    if(is.null(long_df)){
      return(NULL)
    }
    wide_df <- tidyr::pivot_wider(long_df, id_cols= 1, names_from = 'p', values_from= 'value', values_fn = function(x)paste(x, collapse= '~~'))
    colnames(wide_df) <- sapply(colnames(wide_df), function(x) sub('.*[/|#]','',x))
    return(wide_df)

  }")
}
library(igraph)
