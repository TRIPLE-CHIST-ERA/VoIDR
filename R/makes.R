makeOneFunction <- function(className,classIri,  rdfObj, endpoint){
  props <- getMethods(unclass(className), rdfObj)
 # propFilter <- paste(unique(props$propIri), collapse='> <')
  shortName <- sub('(.*)[/|#]','',classIri)

  #root <- sub('(.*)[/|#].*','\\1',clasIri)
  shortProps <-unique(sub('(.*)[/|#]','', props$propIri))

#  propDict <- list()
#  propDict[unique(shortProps)] <- unique(props$propIri)

  func <- paste0("function(properties = c(\"", paste(shortProps,collapse='", "'),"\")){
    propDict <- list()
    propDict[c(\"",paste(shortProps,collapse='", "'),"\")] <- c(\"", paste(unique(props$propIri),collapse='", "'),"\")
    propFilter <- paste(unique(propDict[properties]), collapse='> <')
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

makePackage <- function(packageName, rdfFile, endpoint){
  rdfObj <- loadFile(fixTestFilePath(rdfFile))
  cls <- getClasses(rdfObj)
  funcs <- apply(cls,1, function(x){
    makeOneFunction(x[1], x[2], rdfObj, endpoint)
  }) %>% unlist(recursive = FALSE)
  # restart every time:
  unlink(paste0(tempdir(), '/', packageName), recursive = TRUE)
  myDir <- tempdir()
  names(funcs) <- paste0('get_',names(funcs))
  lapply(names(funcs), function(n){
    funcText <- paste0(n, ' <- ', funcs[[n]])
    cat(funcText, file = paste0(myDir,'/',n, '.R'))
  })
  suppressWarnings(package.skeleton(name = packageName, path = myDir, code_files = paste0(myDir, '/', names(funcs), '.R')))
  # get the sources for SPARQL_query and expandDF
  lapply(c('expandDF', 'SPARQL_query'), function(fname){
    fsource <- capture.output(print(get(fname, envir = as.environment('package:gomr'))))
    fsource[1] <- paste0(fname, ' <- ',fsource[1])
    # without the last line
    cat(fsource[-length(fsource)], file=paste0(myDir,'/', packageName,'/R/',fname,'.R'), sep ="\n")
  })
  ##TODO DESCRIPTION AND NAMESPACE
  return(TRUE)
}


##TODO DESCRIPTION AND NAMESPACE

