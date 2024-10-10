\name{getSong}
\alias{getSong}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
A Capitalized Title (ideally limited to 65 characters)
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
getSong(properties = c("writer", "name", "length", "description"))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{properties}{
%%     ~~Describe \code{properties} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or standard data sets, see data().

## The function is currently defined as
function (properties = c("writer", "name", "length", "description")) 
{
    propDict <- list()
    propDict[c("writer", "name", "length", "description")] <- c("http://stardog.com/tutorial/writer", 
        "http://stardog.com/tutorial/name", "http://stardog.com/tutorial/length", 
        "http://stardog.com/tutorial/description")
    propFilter <- paste(unique(propDict[properties]), collapse = "> <")
    sparql <- paste0("SELECT *\n                  WHERE {\n                    ?Song a <", 
        "http://stardog.com/tutorial/Song", "> .\n                     VALUES ?p { <", 
        propFilter, "> }\n                    ?Song ?p ?value\n                  }")
    long_df <- SPARQL_query("http://localhost:7200/repositories/beatles", 
        sparql)
    if (is.null(long_df)) {
        return(NULL)
    }
    wide_df <- tidyr::pivot_wider(long_df, id_cols = 1, names_from = "p", 
        values_from = "value", values_fn = function(x) paste(x, 
            collapse = "~~"))
    colnames(wide_df) <- sapply(colnames(wide_df), function(x) sub(".*[/|#]", 
        "", x))
    return(wide_df)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory (show via RShowDoc("KEYWORDS")):
% \keyword{ ~kwd1 }
% \keyword{ ~kwd2 }
% Use only one keyword per line.
% For non-standard keywords, use \concept instead of \keyword:
% \concept{ ~cpt1 }
% \concept{ ~cpt2 }
% Use only one concept per line.
