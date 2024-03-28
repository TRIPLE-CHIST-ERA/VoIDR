library( dplyr );
library( purrr );
library( jsonlite );
library( httr ); 
library( stringi );

get_SPARQL_prefixes <- function( ns ){
    return(
        paste(
            paste0( 'PREFIX ', unlist( ns[,1] ), ': <', unlist( ns[,2] ), '>'),
            collapse = "\n"
        )
    );
}

#' @title Run a SPARQL query
#' 
#' @description SPARQL_query() run a SPARQL query, either SELECT, CONSTRUCT or DESCRIBE 
#' and return the results as a tibble. Returned column names are the same as SPARQL 
#' variables. Detection of column types relies on R built-in methods, not RDF data types.
#' 
#' In the HTTP request, the "application/sparql-results+json" MIME type is used,
#' which is supported by most SPARQL endpoints. 
#' 
#' @param endpoint   URL of SPARQL endpoint.
#' @param query      SPARQL query as a string.
#' @param ns         Optional data frame which two first columns are taken for short and long versions of base IRIs.
#' @param list       Additional parameter/value pair to pass with the HTTP request.
#' @param use.POST   Boolean to switch http protocol (default to GET). The main benefit of POST is to allow for larger input query.
#' @param add.prefix Boolean to add PREFIX declarations from ns to the SPARQL query.
#' @param iri.style  One of "long", "short" , "html" or "mdlink", to encode returned IRIs.
#' @param na.value   Value to replace empty fields.
#' @param echo       Boolean value to echo the SPARQL query before execution.
#' @returns          A tibble with the query results or NULL if the query returns nothing.
#' @seealso          SPARQL_ask() SPARQL_update() 
#' @examples
#' \donotrun{
#' SPARQL_query( 
#'     'https://query.wikidata.org/sparql', '
#'     PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
#'     SELECT ?message
#'     WHERE{
#'         wd:Q131303 rdfs:label ?message 
#'         FILTER( LANG( ?message ) = 'en' )
#'     }' )
#' }
SPARQL_query <- function( 
        endpoint, 
        query, 
        ns           = tribble( 
			~short, ~long,
			'rdf',  'http://www.w3.org/1999/02/22-rdf-syntax-ns#',
			'rdfs', 'http://www.w3.org/2000/01/rdf-schema#'
		),
        param        = list(),
		use.POST     = F,
		add.prefix   = F,
        iri.style    = "short",
        na.value     = NA,
        echo         = F
    ){
    if( add.prefix ){
        param$query = paste( get_SPARQL_prefixes( ns ), query, sep="\n" );
    } else {
        param$query = query;
    }
    if( echo ){
        cat( paste0( query, "\n" ))
    }
    start_time <- Sys.time()
    if( use.POST ){
        r <- POST( 
            endpoint,
            add_headers( Accept = "application/sparql-results+json" ),
            body = param, 
            encode = "form"
        );
    } else { # use GET
        url <- paste0( 
            endpoint,
            "?",
            paste0(
                sapply( seq( 1, length( param )), function(i) {
                    paste0(
                        names( param )[i],
                        '=',
                        URLencode( param[[i]], reserved = T ),
                        '&'
                    )
                }), 
                collapse = ""
            )
        );
        print(url)
        r <- GET( 
            url, 
            add_headers( Accept = "application/sparql-results+json" )
        );
    }
    if( r$status_code != 200 ){
        print( r )
        stop( paste( r$header, sep="\n", collapse="\n" ))
    }
    l <- content( r, type="application/json" );
    end_time <- Sys.time()
    message( paste( "Query time:", end_time - start_time, "s" ))
    if( length( l$results$bindings ) == 0 ){
        return( NULL )
    }
    # replace missing values in JSON response with explicit NAs
    buf <- lapply( 
        unlist( l$head ), 
        function( name ){
            sapply( 
                l$results$bindings,
                function( b ){
                    return(
                        ifelse( 
                            is.null( b[[name]] ),
                            NA,
                            b[[name]]$value
                        )
                    )
                }
                , simplify = T 
            )
        }
    )
    t <- as_tibble( setNames( buf, unlist( l$head ))) %>% 
        type.convert( na.strings=c(""),  as.is=T ); # call built-in type conversion of R
    if( ! is.na( na.value )){
        t <- t %>% replace( is.na(.), na.value );
    }
    # TODO: fix NAs  
    if( iri.style == "short" ){
        return( t %>% 
            mutate( 
                across( 
                    where( is_character ), 
                    ~ stri_replace_all_regex( 
                        unlist( . ), 
                        pattern       = unlist( ns[,2] ), 
                        replacement   = paste0( unlist( ns[,1] ), ':' ),
                        vectorize_all = F
                    )
                )
            )
        )
    } else if( iri.style == "mdlink" ){
        return( t %>% 
            mutate( 
                across( 
                    where( is_character ), 
                    ~ stri_replace_all_regex( 
                        unlist( . ), 
                        pattern     = paste0( '(', unlist( ns[,2] ), ')(\\S+)' ),
                        replacement = paste0( '[', unlist( ns[,1] ), ':$2]($1$2)' ),
                        vectorize_all = F    
                    )
                )
            )
        )
    } else if( iri.style == "html" ){
        return( t %>% 
            mutate( 
                across( 
                    where( is_character ), 
                    ~ stri_replace_all_regex( 
                        unlist( . ), 
                        pattern     = paste0( '(', unlist( ns[,2] ), ')(\\S+)' ),
                        replacement = paste0( '<a href="', unlist( ns[,2] ),'$2">', unlist( ns[,1] ), ':$2</a>' ),
                        vectorize_all = F    
                    )
                )
            )
        )
    }
    return( t );
}

SPARQL_update <- function(){
    stop( "not yet implemented" );    
}

SPARQL_ask <- function(){
    stop( "not yet implemented" );    
}



