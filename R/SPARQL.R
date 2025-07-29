library(dplyr)
library(purrr)
library(jsonlite)
library(httr)
library(stringi)

# Converts a 2-column tribble into a SPARQL PREFIX string.
as_sparql_prefix <- function(prefixes) {
  paste(
    paste0("PREFIX ", unlist(prefixes[, 1]), ": <", unlist(prefixes[, 2]), ">"),
    collapse = "\n"
  )
}

#' @title Run a SPARQL query
#'
#' @description run a SPARQL query, either SELECT, CONSTRUCT or DESCRIBE and
#' return the results as a tibble. Returned column names are the same as SPARQL
#' variables. Detection of column types relies on R built-in methods, not RDF
#' data types.
#'
#' In the HTTP request, the "application/sparql-results+json" MIME type is
#' used, which is supported by most SPARQL endpoints.
#'
#' @param endpoint   URL of SPARQL endpoint.
#' @param query      SPARQL query as a string.
#' @param ns         Optional data frame whose first two columns are taken for
#'                   short and long versions of base IRIs.
#' @param param      Additional parameter/value pair to pass with the HTTP
#'                   request.
#'                   Example: some endpoints accept a "timeout" argument, which
#'                   could be passed via this argument.
#' @param use_post   Boolean to switch http protocol (default to GET). The main
#'                   benefit of POST is to allow for larger input query.
#' @param add.prefix Boolean to add PREFIX declarations from ns to the SPARQL
#'                   query.
#' @param iri.style  One of "long", "short" , "html" or "mdlink" (markdown
#'                   link), to encode returned IRIs.
#' @param na_value   Value with which to replace empty fields.
#' @param echo       Boolean value to echo the SPARQL query before execution.
#'
#' @returns          A tibble with the query results or NULL if the query
#'                   returns nothing.
#' @seealso          SPARQL_ask() SPARQL_update()
#'
#' @examples
#' \donotrun{
#' sparql_query(
#'     'https://query.wikidata.org/sparql', '
#'     PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
#'     SELECT ?message
#'     WHERE{
#'         wd:Q131303 rdfs:label ?message
#'         FILTER( LANG( ?message ) = 'en' )
#'     }' )
#' }
sparql_query <- function(
  endpoint,
  query,
  ns = tibble::tribble(
    ~short, ~long,
    "rdf",  "http://www.w3.org/1999/02/22-rdf-syntax-ns#",
    "rdfs", "http://www.w3.org/2000/01/rdf-schema#"
  ),
  param      = list(),
  use_post   = FALSE,
  add_prefix = FALSE,
  iri_style  = "short",
  na_value   = NA,
  echo       = FALSE
) {
  # Prepend PREFIXes to the SPARQL query.
  if (add_prefix) {
    param$query <- paste(as_sparql_prefix(ns), query, sep = "\n\n")
  } else {
    param$query <- query
  }
  if (echo) {
    cat(paste0(param$query, "\n"))
  }

  # Submit SPARQL query to endpoint.
  start_time <- Sys.time()
  if (use_post) {
    response <- httr::POST(
      endpoint,
      httr::add_headers(Accept = "application/sparql-results+json"),
      body = param,
      encode = "form"
    )
  } else { # use GET
    url <- paste0(
      endpoint,
      "?",
      paste0(
        sapply(seq(1, length(param)), function(i) {
          paste0(
            names(param)[i],
            "=",
            URLencode(param[[i]], reserved = TRUE),
            "&"
          )
        }),
        collapse = ""
      )
    )
    # print(url)
    response <- httr::GET(
      url,
      httr::add_headers(Accept = "application/sparql-results+json")
    )
  }
  if (response$status_code != 200) {
    print(response)
    stop(paste(response$header, sep = "\n", collapse = "\n"))
  }
  query_result <- tryCatch(
    httr::content(response, type = "application/json"),
    error = function(e) content(response, type = "text/html")
  )
  message(paste("Query time:", Sys.time() - start_time, "s"))

  if (length(query_result$results$bindings) == 0) {
    return(NULL)
  }

  # Parse the JSON response of the query, and replace missing values with
  # explicit NAs.
  buf <- lapply(
    unlist(query_result$head),
    function(name) {
      sapply(
        query_result$results$bindings,
        function(b) {
          ifelse(
            is.null(b[[name]]),
            NA,
            b[[name]]$value
          )
        },
        simplify = TRUE
      )
    }
  )
  t <- tibble::as_tibble(setNames(buf, unlist(query_result$head))) |>
    # call built-in type conversion of R
    type.convert(na.strings = c(""), as.is = TRUE)

  if (!is.na(na_value)) {
    t <- t %>% replace(is.na(.), na_value)
  }

  # TODO: fix NAs
  if (iri_style == "short") {
    return(t %>%
      dplyr::mutate(
        dplyr::across(
          dplyr::where(rlang::is_character),
          ~ stringi::stri_replace_all_regex(
            unlist(.),
            pattern       = unlist(ns[, 2]),
            replacement   = paste0(unlist(ns[, 1]), ":"),
            vectorize_all = FALSE
          )
        )
      )
    )
  } else if (iri_style == "mdlink") {
    return(t %>%
      dplyr::mutate(
        dplyr::across(
          dplyr::where(rlang::is_character),
          ~ stringi::stri_replace_all_regex(
            unlist(.),
            pattern       = paste0("(", unlist(ns[, 2]), ")(\\S+)"),
            replacement   = paste0("[", unlist(ns[, 1]), ":$2]($1$2)"),
            vectorize_all = FALSE
          )
        )
      )
    )
  } else if (iri_style == "html") {
    return(t %>%
      dplyr::mutate(
        dplyr::across(
          dplyr::where(rlang::is_character),
          ~ stringi::stri_replace_all_regex(
            unlist(.),
            pattern     = paste0("(", unlist(ns[, 2]), ")(\\S+)"),
            replacement = paste0(
              '<a href="',
              unlist(ns[, 2]),
              '$2">',
              unlist(ns[, 1]),
              ":$2</a>"
            ),
            vectorize_all = FALSE
          )
        )
      )
    )
  }
  return(t)
}

sparql_update <- function() {
  stop("not yet implemented")
}

sparql_ask <- function() {
  stop("not yet implemented")
}
