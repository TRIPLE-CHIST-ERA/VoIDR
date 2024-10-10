getSong <-
function(properties = c("writer", "name", "length", "description")){
    propDict <- list()
    propDict[c("writer", "name", "length", "description")] <- c("http://stardog.com/tutorial/writer", "http://stardog.com/tutorial/name", "http://stardog.com/tutorial/length", "http://stardog.com/tutorial/description")
    propFilter <- paste(unique(propDict[properties]), collapse='> <')
    sparql <-  paste0('SELECT *
                  WHERE {
                    ?Song a <',"http://stardog.com/tutorial/Song",'> .
                     VALUES ?p { <', propFilter, '> }
                    ?Song ?p ?value
                  }')
    long_df <- SPARQL_query('http://localhost:7200/repositories/beatles', sparql)
    if(is.null(long_df)){
      return(NULL)
    }
    wide_df <- tidyr::pivot_wider(long_df, id_cols= 1, names_from = 'p', values_from= 'value', values_fn = function(x)paste(x, collapse= '~~'))
    colnames(wide_df) <- sapply(colnames(wide_df), function(x) sub('.*[/|#]','',x))
    return(wide_df)

  }
