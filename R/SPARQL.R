
DEFAULT_PREFIXES <- tibble::tribble(
  ~short, ~long,
  "rdf",  "http://www.w3.org/1999/02/22-rdf-syntax-ns#",
  "rdfs", "http://www.w3.org/2000/01/rdf-schema#"
)

#' Returns a string with the formatted elapsed time.
#' @keywords internal
elapsed_time <- function(start_time, end_time) {
  elapsed_seconds <- round(as.numeric(end_time - start_time, units = "secs"))
  h <- elapsed_seconds %/% 3600
  m <- (elapsed_seconds %% 3600) %/% 60
  s <- elapsed_seconds %% 60
  sprintf("%02d:%02d:%02d", h, m, s)
}

#' Converts a 2-column tibble into a SPARQL PREFIX string.
#'
#' @param prefixes  2-column tibble containing the long and short form of
#'                  the SPARQL prefixes.
#'
#' @keywords internal
as_sparql_prefix <- function(prefixes) {
  paste(
    paste0("PREFIX ", unlist(prefixes[, 1]), ": <", unlist(prefixes[, 2]), ">"),
    collapse = "\n"
  )
}

#' Checks whether the `x` list object has the expected structure of a standard
#' SPARQL query response in JSON format.
#'
#' @keywords internal
is_valid_rdf_term <- function(x) {
  is.list(x) && !is.null(x$type) && !is.null(x$value)
}

is_valid_query_result <- function(x) {
  is.list(x) &&
    all(c("head", "results") %in% names(x) &&
    "bindings" %in% names(x$results))
}

#' Convert an individual RDF term to an R string.
#'
#' @param value    must be of type `list` with `type` and `value` fields,
#'                 or `NULL`.
#' @param na_value Value returned when `value` is NULL or an empty string.
#'
#' @keywords internal
rdf_term_to_string <- function(value, na_value = NA) {
  # This function assumes that the input `value` has the correct structure:
  # a list with "type" and "value" fields.
  if (is.null(value) || value$value == "") {
    return(na_value)
  }
  switch(
    value$type,
    "uri" = paste0("<", value$value, ">"),
    "literal" = value$value,
    "bnode" = paste0("_:", value$value),
    rlang::abort(paste("Unknown RDF type in JSON response:", value$type))
  )
}

#' Parse a `query_result` object - a nested list derived from the JSON returned
#' by a SPARQL query - and return it as an R tibble.
#'
#' @param query_result  Object (list) to parse.
#' @param na_value      Value with which to replace empty/missing fields.
#'
#' @keywords internal
query_result_to_tibble <- function(query_result, na_value = NA) {

  if (!is_valid_query_result(query_result)) {
    rlang::abort(
      paste0(
        "Input value '", query_result, "' is not a valid SPARQL query result"
      )
    )
  }

  # Parse the "query_result" nested list object.
  # For each record ("table row") returned by the request, check whether some
  # fields ("table columns") are missing, which indicates a "NA" value.
  variable_names <- unlist(query_result$head)
  lapply(
    variable_names,
    function(column_name) {
      sapply(
        query_result$results$bindings,
        function(x) rdf_term_to_string(x[[column_name]], na_value)
      )
    }
  ) |>
    # Convert the parsed list to an R tibble.
    rlang::set_names(variable_names) |>
    tibble::as_tibble() |>
    # Call the built-in type conversion of R.
    type.convert(na.strings = c(""), as.is = TRUE)

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
#' @param endpoint     URL of SPARQL endpoint.
#' @param query        SPARQL query as a string.
#' @param prefixes     Optional data frame whose first two columns are taken
#'                     for short and long versions of base IRIs.
#' @param http_params  Additional parameter/value pair to pass with the HTTP
#'                     request.
#'                     Example: some endpoints accept a "timeout" argument.
#'                     This could be passed via this argument.
#' @param use_post     Boolean to switch http protocol (default to GET). The
#'                     main benefit of POST is to allow for larger input query.
#' @param add_prefixes Boolean to add PREFIX declarations passed to the
#'                     `prefixes` argument to the SPARQL query.
#' @param iri_style    One of "long", "short" , "html" or "mdlink" (markdown
#'                     link), to encode returned IRIs.
#' @param na_value     Value with which to replace empty fields.
#' @param echo         Boolean value to echo the SPARQL query before execution.
#'
#' @returns            A tibble with the query results or NULL if the query
#'                     returns nothing.
#' @seealso            SPARQL_ask() SPARQL_update()
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
  prefixes = tibble::tribble(
    ~short, ~long,
    "rdf",  "http://www.w3.org/1999/02/22-rdf-syntax-ns#",
    "rdfs", "http://www.w3.org/2000/01/rdf-schema#"
  ),
  http_params  = list(),
  use_post     = FALSE,
  add_prefixes = FALSE,
  iri_style    = "short",
  na_value     = NA,
  echo         = FALSE
) {
  # Prepend PREFIXes to the SPARQL query.
  if (add_prefixes) {
    http_params$query <- paste(as_sparql_prefix(prefixes), query, sep = "\n\n")
  } else {
    http_params$query <- query
  }
  if (echo) {
    cat(paste0(http_params$query, "\n"))
  }

  # Submit SPARQL query to endpoint.
  start_time <- Sys.time()
  if (use_post) {
    # Submit query via a POST request.
    response <- httr::POST(
      endpoint,
      httr::add_headers(Accept = "application/sparql-results+json"),
      body = http_params,
      encode = "form"
    )
  } else {
    # Submit query via a GET request.
    url <- paste0(
      endpoint,
      "?",
      paste0(
        sapply(seq(1, length(http_params)), function(i) {
          paste0(
            names(http_params)[i],
            "=",
            URLencode(http_params[[i]], reserved = TRUE),
            "&"
          )
        }),
        collapse = ""
      )
    )
    response <- httr::GET(
      url,
      httr::add_headers(Accept = "application/sparql-results+json")
    )
  }

  # Make sure the HTTP request completed successfully, otherwise report the
  # error and exit function.
  if (response$status_code != 200) {
    print(response)
    stop(paste(response$header, sep = "\n", collapse = "\n"))
  }
  query_result <- tryCatch(
    httr::content(response, type = "application/json"),
    error = function(e) httr::content(response, type = "text/html")
  )
  message(paste("Query time:", elapsed_time(start_time, end_time = Sys.time())))

  # Convert the HTTP query response into a tibble. If the query returned
  # no results, exit function.
  if (length(query_result$results$bindings) == 0) {
    return(NULL)
  }
  query_result_tibble <- query_result_to_tibble(query_result)

  # Adapt IRI style to the format requested by the user.
  # Note: applying the regexp using dplyr is much faster than trying to apply
  # it when converting individual RDF terms.
  if (iri_style == "long") {
    return(query_result_tibble)
  }
  short_forms <- unlist(prefixes[, 1])
  long_forms <- unlist(prefixes[, 2])
  pattern <- paste0("<(", long_forms, ")(\\S+)>")
  replacement <- switch(
    iri_style,
    "short" = paste0(short_forms, ":$2"),
    "mdlink" = paste0("[", short_forms, ":$2]($1$2)"),
    "html" = paste0('<a href="', long_forms, '$2">', short_forms, ":$2</a>"),
    rlang::abort(paste("Unsupported iri_style:", iri_style))
  )
  query_result_tibble |>
    dplyr::mutate(
      dplyr::across(
        dplyr::where(rlang::is_character),
        function(x) {
          stringi::stri_replace_all_regex(
            unlist(x),
            pattern = pattern,
            replacement = replacement,
            vectorize_all = FALSE
          )
        }
      )
    )
}

sparql_update <- function() {
  stop("not yet implemented")
}

sparql_ask <- function() {
  stop("not yet implemented")
}

# To make CONSTRUCT requests.
sparql_construct <- function() {
  stop("not yet implemented")
}


# To make DESCRIBE requests.
sparql_describe <- function() {
  stop("not yet implemented")
}
