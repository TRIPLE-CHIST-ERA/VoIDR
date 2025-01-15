makeOneFunction <- function(className,classIri, endpoint, voidFile = NULL, voidEndpoint = NULL, voidGraph = NULL){
  props <- getMethods(unclass(className), voidFile, voidEndpoint , voidGraph )
 # propFilter <- paste(unique(props$propIri), collapse='> <')
  shortName <- sub('(.*)[/|#]','',classIri) %>% make.names %>% sub('\\.+', '_',.)

  #TODO: keep prefixes

  #root <- sub('(.*)[/|#].*','\\1',clasIri)
  shortProps <-unique(sub('(.*)[/|#]','', props$propIri))

#  propDict <- list()
#  propDict[unique(shortProps)] <- unique(props$propIri)

  func <- paste0("function(properties = c(\"", paste(shortProps,collapse='", "'),"\"), limit = 1000){
    propDict <- list()
    propDict[c(\"",paste(shortProps,collapse='", "'),"\")] <- c(\"", paste(unique(props$propIri),collapse='", "'),"\")
    propFilter <- paste(propDict[properties], collapse='> <')
    sparql <-  paste0('SELECT *
                  WHERE {
                    ?",shortName, " a <',\"", classIri, "\",'> .
                     VALUES ?p { <', propFilter, '> }
                    ?",shortName, " ?p ?value
                  }')
    if(!is.null(limit)){
      sparql <- paste0(sparql, ' LIMIT ', as.integer(limit))
    }
    long_df <- SPARQL_query('",endpoint,"', sparql)
    if(is.null(long_df)){
      return(NULL)
    }
    wide_df <- tidyr::pivot_wider(long_df, id_cols= 1, names_from = 'p', values_from= 'value', values_fn = function(x)paste(x, collapse= '~~'))
    colnames(wide_df) <- sapply(colnames(wide_df), function(x) sub('.*[/|#]','',x))
    return(wide_df)

  }")
  doc <- getDescriptions(filter = list(class = classIri, property = paste(unique(props$propIri), collapse='> <')), endpoint)
  funcDoc <- doc$class$description
  propDoc <- doc$property
  doc$property$entity <- sub('(.*)[/|#]','',doc$property$entity)
  ret <- list()
  ret[[shortName]] <- list(func = func, funcDoc = funcDoc, propDoc = propDoc)
  return(ret)
}

makePackage <- function(packageName, endpoint, voidFile = NULL, voidEndpoint = NULL, voidGraph = NULL, authors = 'person("Iulian", "Dragan", email = "iulian.dragan@sib.swiss", role = c("aut", "cre"))', license = NULL, dest_path = '.'){
  cls <- getClasses2(voidFile,voidEndpoint, voidGraph)
  clsNames <- unique(c(cls$literalSparql$classFrom, cls$iriSparql$classFrom))
  funcs <- lapply(clsNames, function(x){
 #   makeOneFunction(x[1], x[2], endpoint, voidFile, voidEndpoint, voidGraph)
    makeOneFunction2(x, endpoint, voidEndpoint, cls)
  }) %>% unlist(recursive = FALSE)
  assign('funcs', funcs, envir = .GlobalEnv)
  # restart every time:
  unlink(paste0(tempdir(), '/', packageName), recursive = TRUE)
  myDir <- tempdir()
  names(funcs) <- paste0('get_',names(funcs)) %>% make.names
  lapply(names(funcs), function(n){
    print(n)
    fileName <- paste0(myDir,'/',n, '.R')
    cat(paste0("#' @title ", n, "\n"), file = fileName)
    if(is.null(funcs[[n]]$funcDoc)){
      funcs[[n]]$funcDoc <- n
    }
    cat(paste0("#' @description ", gsub("\n+", " ",funcs[[n]]$funcDoc), "\n"), file = fileName, append = TRUE)
    cat("#' @param properties a character vector, which properties of this class should be loaded. Properties will become columns of the result set.\n", file = fileName, append = TRUE)
    if(!is.null(funcs[[n]]$propDoc)){
      funcs[[n]]$propDoc$entity  <- sub('(.*)[/|#]','',funcs[[n]]$propDoc$entity ) # shorten
      apply(funcs[[n]]$propDoc, 1,  function(this.prop){
        cat(paste0("#'  * ",this.prop['entity'], " -- ", gsub("\n+", " ", this.prop['description']), "\n"), file = fileName, append = TRUE)
      })
      cat(paste0("#' @md\n"), file = fileName, append = TRUE)
    }
    cat("#' @param limit a numeric, how many triples to fetch, default 1000. If null, all the triples will be fetched.\n", file = fileName, append = TRUE)
    funcText <- paste0(n, ' <- ', funcs[[n]]$func)
    cat(funcText, file = fileName, append = TRUE)
  })
  package.skeleton(name = packageName, path = myDir, code_files = paste0(myDir, '/', names(funcs), '.R'))
  # get the sources for SPARQL_query and expandDF
  lapply(c('makeSparql', 'SPARQL_query'), function(fname){
    fsource <- capture.output(print(get(fname, envir = as.environment('package:VoIDR'))))
    fsource[1] <- paste0(fname, ' <- ',fsource[1])
    # without the lines starting with "<" (meta package rubbish)
    cat(fsource[grep('^<', fsource, invert = TRUE)], file = paste0(myDir,'/', packageName,'/R/',fname,'.R'), sep ="\n")
  })
  # DESCRIPTION
  #desc <- readLines(fixTestFilePath('./extdata/DESCRIPTION'))
  desc <- readLines(system.file('extdata', 'DESCRIPTION', package='VoIDR'))
  desc[1] <- paste0(desc[1],' ', packageName)
  desc[5] <- paste0(desc[5],' ', Sys.Date())
  desc[6] <- paste0('Authors@R: ', authors)
  desc[7] <- sub('<endpoint>', endpoint, desc[7])
  if(!is.null(license)){
    desc[9] <- paste0(desc[9],' ', license)
  }
  cat(desc, file = paste0(myDir,'/', packageName,'/DESCRIPTION'), sep ="\n")
  # NAMESPACE
 # cat('export("expandDF")', file = paste0(myDir,'/', packageName,'/NAMESPACE'), append = TRUE)
  unlink(paste0(myDir,'/', packageName, '/Read-and-delete-me'))
  unlink(paste0(myDir,'/', packageName, '/man/*'))
  devtools::document(pkg = paste0(myDir,'/', packageName))
  paste0(myDir,'/', packageName) %>% devtools::build(path = dest_path) %>% return

}




makeOneFunction2 <- function(className, endpoint, voidEndpoint, classList){

  props <- getMethods2(unclass(className), classList )
  # propFilter <- paste(unique(props$propIri), collapse='> <')
  shortName <- sub('(.*)[/|#]','',className) %>% make.names %>% sub('\\.+', '_',.)

  #TODO: keep prefixes

  #root <- sub('(.*)[/|#].*','\\1',clasIri)
  isEmpty <- function(x) length(x) == 0
  longProps <- sapply(props, function(x) sapply(x, function(y)unique(y$property), simplify = FALSE), simplify = FALSE) %>%
                sapply(function(l) l[!sapply(l, isEmpty)], simplify = FALSE)  %>% `[`(!sapply(., isEmpty))



  # shortProps <-sapply(props, function(x) sapply(x, function(y)unique(sub('(.*)[/|#]','', y$property)), simplify = FALSE), simplify = FALSE)
  shortProps <-sapply(longProps, function(x) sapply(x, function(y)sub('(.*)[/|#]','', y), simplify = FALSE), simplify = FALSE)

  propDict <- list()
  propDict[unlist(shortProps, use.names = FALSE)] <- unlist(longProps, use.names = FALSE)


 #new_l <-  sapply(sp, function(l) l[!sapply(l, function(x) length(x) ==0)])
 #sp %>% sapply(function(l) l[!sapply(l, isEmpty)]) %>% `[`(!sapply(., isEmpty))
 #isEmpty <- function(x) length(x) == 0
 #shortProps <- shortProps %>% sapply(function(l) l[!sapply(l, isEmpty)]) %>% `[`(!sapply(., isEmpty)) %>% list
 argProps <- paste(list(shortProps), collapse = ", ")

 #return(argProps)
  #  propDict <- list()
  #  propDict[unique(shortProps)] <- unique(props$propIri)

  #func <- paste0("function(properties = c(\"", paste(shortProps,collapse='", "'),"\"), limit = 1000){
#  func <- paste0("function(properties = ", argProps,  ", limit = 1000){
#   iriProps <- ",  paste(list(longProps), collapse = ", "), "
#   sapply(names(properties), function(t){
#    propType = properties[[t]]
#    sapply(names(propType), function(card){
#      propCard <- propType[[card]]
#      propDict <- list()
#      propDict[propCard] <- iriProps[[t]][[card]]
#      propFilter <- paste(propDict[propCard], collapse='> <')
#      sparql <- makeSparql(propFilter,'", shortName, "', '", className, "', limit)
#      long_df <- SPARQL_query('",endpoint,"', sparql)
#      if(is.null(long_df)){
#       return(NULL)
#     }
#    wide_df <- tidyr::pivot_wider(long_df, id_cols= 1, names_from = 'p', values_from= 'value', values_fn = function(x)paste(x, collapse= '~~'))
#    colnames(wide_df) <- sapply(colnames(wide_df), function(x) sub('.*[/|#]','',x))
#    return(wide_df)
#    }, simplify = FALSE)
 #  }, simplify = FALSE)

#  }")

  func <- paste0("function(properties = ", argProps,  ", limit = 1000, only.complete.cases = FALSE){
   propDict <- ",  paste(list(propDict), collapse = ", "), "
   isEmpty <- function(x) length(x) == 0
   flatProps <- unlist(properties, use.names = FALSE)

   sparql <- makeSparql(propDict[flatProps],'", shortName, "', '", className, "', limit, only.complete.cases)
      long_df <- SPARQL_query('",endpoint,"', sparql)
      if(is.null(long_df)){
       return(NULL)
     }
    return(long_df)

  }")


 #funcdoc <-  sapply(names(shortProps), function(t){
#    propType = shortProps[[t]]
#    sapply(names(propType), function(card){
#      propCard <- longProps[[t]][[card]]
#      getDescriptions(filter = list(class = className, property = paste(propCard, collapse='> <')), voidEndpoint)
#    }, simplify = FALSE)}, simplify = FALSE)


  flatProps <- sapply(longProps, function(x) unname(sapply(x, unname, simplify = FALSE)), simplify = FALSE) %>% unname %>% unlist

  doc <-getDescriptions(filter = list(class = className, property = flatProps), voidEndpoint)



 # doc <- getDescriptions(filter = list(class = className, property = paste(longProps, collapse='> <')), endpoint)
  funcDoc <- doc$class$description
  propDoc <- doc$property
  doc$property$entity <- sub('(.*)[/|#]','',doc$property$entity)
  ret <- list()
  ret[[shortName]] <- list(func = func, funcDoc = funcDoc, propDoc = propDoc)
  return(ret)
}


makeSparql_old <- function( propFilter, shortName, longName, limit = NULL){
  sparql <- paste0('SELECT *
                  WHERE {
                    ?',shortName, ' a <', longName, '> .
                     VALUES ?p { <', propFilter, '> }
                    ?',shortName, ' ?p ?value
                  }')
  if(!is.null(limit)){
    sparql <- paste0(sparql, ' LIMIT ', as.integer(limit))
  }
  return(sparql)
}

makeSparql <- function( propFilter, shortName, longName, limit = NULL, only.complete.cases = FALSE){
  if(only.complete.cases){
    linePrefix = ''
  } else {
    linePrefix = 'OPTIONAL'
  }
  selectLimitClause <- paste0("{SELECT ?", shortName, "\n WHERE { ?", shortName, " a <", longName, ">}\n")
  if(!is.null(limit)){
    selectLimitClause <- paste0(selectLimitClause, ' LIMIT ', format(limit, scientific = FALSE), "\n")
  }
  selectLimitClause <- paste0(selectLimitClause, "\n }")
  joinClause <- Reduce(function(x,y) {
    ret <- paste0(x, linePrefix, '{ ?', shortName, ' <',  propFilter[[y]], '> ')
    if(!grepl(':', ret)){
      ret <- paste0(ret, '?')
    }
    paste0(ret, y , "}\n" )
  }, names(propFilter), init = '')


  sparql <- paste0("SELECT *\n WHERE {\n", selectLimitClause, joinClause, "\n}")

  return(sparql)
}
