shorten <- function(string){
  sub( '.+/', '', string)
}
createClass <- function(cname, props){
  cname <- shorten(cname)
  props <- lapply(props, shorten)
  assign(cname,
         R6::R6Class(cname,
                     public = props
                     ),
         envir = .GlobalEnv
        )
}
