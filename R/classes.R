shorten <- function(string) {
  sub(".+/", "", string)
}

create_class <- function(class_name, props) {
  cname <- shorten(class_name)
  props <- lapply(props, shorten)
  assign(cname, R6::R6Class(cname, public = props), envir = .GlobalEnv)
}
