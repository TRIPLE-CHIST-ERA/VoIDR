makeOneFunction <- function(className,classIri, endpoint, voidFile = NULL, voidEndpoint = NULL, voidGraph = NULL){
  props <- getMethods(unclass(className), voidFile, voidEndpoint , voidGraph )
 # propFilter <- paste(unique(props$propIri), collapse='> <')
  shortName <- sub('(.*)[/|#]','',classIri) %>% make.names %>% sub('\\.+', '_',.)

  #TODO: keep prefixes

  #root <- sub('(.*)[/|#].*','\\1',clasIri)
  shortProps <-unique(sub('(.*)[/|#]','', props$propIri))

#  propDict <- list()
#  propDict[unique(shortProps)] <- unique(props$propIri)

  func <- paste0("function(properties = c(\"", paste(shortProps,collapse='", "'),"\")){
    propDict <- list()
    propDict[c(\"",paste(shortProps,collapse='", "'),"\")] <- c(\"", paste(unique(props$propIri),collapse='", "'),"\")
    propFilter <- paste(propDict[properties], collapse='> <')
    sparql <-  paste0('SELECT *
                  WHERE {
                    ?",shortName, " a <',\"", classIri, "\",'> .
                     VALUES ?p { <', propFilter, '> }
                    ?",shortName, " ?p ?value
                  }')
    long_df <- SPARQL_query('",endpoint,"', sparql)
    if(is.null(long_df)){
      return(NULL)
    }
    wide_df <- tidyr::pivot_wider(long_df, id_cols= 1, names_from = 'p', values_from= 'value', values_fn = function(x)paste(x, collapse= '~~'))
    colnames(wide_df) <- sapply(colnames(wide_df), function(x) sub('.*[/|#]','',x))
    return(wide_df)

  }")
  ret <- list()
  ret[[shortName]] <- func
  return(ret)
}

makePackage <- function(packageName, endpoint, voidFile = NULL, voidEndpoint = NULL, voidGraph = NULL, authors = 'person("Iulian", "Dragan", email = "iulian.dragan@sib.swiss", role = c("aut", "cre"))', license = NULL, dest_path = '.'){
  cls <- getClasses(voidFile,voidEndpoint, voidGraph)
  funcs <- apply(cls,1, function(x){
    makeOneFunction(x[1], x[2], endpoint, voidFile, voidEndpoint, voidGraph)
  }) %>% unlist(recursive = FALSE)
  # restart every time:
  unlink(paste0(tempdir(), '/', packageName), recursive = TRUE)
  myDir <- tempdir()
  names(funcs) <- paste0('get_',names(funcs)) %>% make.names
  lapply(names(funcs), function(n){
    funcText <- paste0(n, ' <- ', funcs[[n]])
    cat(funcText, file = paste0(myDir,'/',n, '.R'))
  })

  package.skeleton(name = packageName, path = myDir, code_files = paste0(myDir, '/', names(funcs), '.R'))
  # get the sources for SPARQL_query and expandDF
  lapply(c('expandDF', 'SPARQL_query'), function(fname){
    fsource <- capture.output(print(get(fname, envir = as.environment('package:VoIDR'))))
    fsource[1] <- paste0(fname, ' <- ',fsource[1])
    # without the lines starting with "<" (meta package rubbish)
    cat(fsource[grep('^<', fsource, invert = TRUE)], file = paste0(myDir,'/', packageName,'/R/',fname,'.R'), sep ="\n")
  })
  # DESCRIPTION
  desc <- readLines(fixTestFilePath('./extdata/DESCRIPTION'))
  desc[1] <- paste0(desc[1],' ', packageName)
  desc[5] <- paste0(desc[5],' ', Sys.Date())
  desc[6] <- paste0('Authors@R: ', authors)
  desc[7] <- sub('<endpoint>', endpoint, desc[7])
  if(!is.null(license)){
    desc[9] <- paste0(desc[9],' ', license)
  }
  cat(desc, file = paste0(myDir,'/', packageName,'/DESCRIPTION'), sep ="\n")
  # NAMESPACE
  cat('export("expandDF")', file = paste0(myDir,'/', packageName,'/NAMESPACE'), append = TRUE)
  unlink(paste0(myDir,'/', packageName, '/Read-and-delete-me'))
  paste0(myDir,'/', packageName) %>% devtools::build(path = dest_path) %>% return
}


##TODO DESCRIPTION AND NAMESPACE

