
makePackage <- function(packageName, endpoint, voidEndpoint = NULL, voidGraph = NULL, authors = 'person("Iulian", "Dragan", email = "iulian.dragan@sib.swiss", role = c("aut", "cre"))', license = NULL, dest_path = '.'){
  cls <- getClasses(voidEndpoint, voidGraph)
  clsNames <- unique(c(cls$dataSparql$classFrom, cls$objectSparql$classFrom))
  funcs <- lapply(clsNames, function(x){
    makeOneFunction(x, endpoint, voidEndpoint, cls)
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
    cat("#' @param only.complete.cases a logical, fetch only rows where all the specified properties have a value? If FALSE (the default) NAs are allowed in the output.\n", file = fileName, append = TRUE)
    funcText <- paste0(n, ' <- ', funcs[[n]]$func)
    cat(funcText, file = fileName, append = TRUE)
  })
  #print(packageName)
  package.skeleton(name = packageName, path = myDir, code_files = paste0(myDir, '/', names(funcs), '.R'))
  # get the sources for SPARQL_query and expandDF
  lapply(c('makeSparql', 'SPARQL_query', 'isEmpty' ), function(fname){
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
  #cat('\nexport("print.sparql_string")', file = paste0(myDir,'/', packageName,'/NAMESPACE'), append = TRUE)
  unlink(paste0(myDir,'/', packageName, '/Read-and-delete-me'))
  unlink(paste0(myDir,'/', packageName, '/man/*'))
  devtools::document(pkg = paste0(myDir,'/', packageName))
  paste0(myDir,'/', packageName) %>% devtools::build(path = dest_path) %>% return

}




makeOneFunction <- function(className, endpoint, voidEndpoint, classList){

  props <- getMethods(unclass(className), classList )

  shortName <- sub('(.*)[/|#]','',className) %>% make.names %>% sub('\\.+', '_',.)

  #TODO: keep prefixes


  longProps <- sapply(props, function(x) sapply(x, function(y)unique(y$property), simplify = FALSE), simplify = FALSE) %>%
                sapply(function(l) l[!sapply(l, isEmpty)], simplify = FALSE)  %>% `[`(!sapply(., isEmpty))



  shortProps <-sapply(longProps, function(x) sapply(x, function(y) gsub(':','_', (sub('(.*)[/|#]','', y))), simplify = FALSE), simplify = FALSE)

  propDict <- list()
  propDict[unlist(shortProps, use.names = FALSE)] <- unlist(longProps, use.names = FALSE)


 argProps <- paste(list(shortProps), collapse = ", ")


 list(dataProperties = list(nonunique = c("mnemonic", "obsolete", "commonName", "otherName", "partOfLineage", "scientificName", "synonym")),
      objectProperties = list(unique = c("strain", "narrowerTransitive"), nonunique = c("replaces", "rdfs:subClassOf", "replacedBy", "host", "depiction")))

  func <- paste0("function(properties = ", argProps,  ", limit = 1000, only.complete.cases = FALSE){
   propDict <- ",  paste(list(propDict), collapse = ", "), "
   isEmpty <- function(x) length(x) == 0
   flatProps <- unlist(properties, use.names = FALSE)
   returnPattern <- ", argProps, "
   sparql <- makeSparql(propDict[flatProps],'", shortName, "', '", className, "', limit, only.complete.cases)
    retDf <- SPARQL_query('",endpoint,"', sparql)
    retCols <- colnames(retDf)
    ret <- sapply(returnPattern, function(propType){
      out <- sapply(propType, function(propCard){
      actualCols <- intersect(propCard, retCols)
      if(length(actualCols) == 0){
        return(NULL)
      }
      retChunk <- retDf[,c('", shortName, "',actualCols)]
      retChunk[!duplicated(retChunk),]
      }, simplify = FALSE)
      return(out[!sapply(out, is.null)])
    }, simplify = FALSE)
    ret$sparql <- sparql
    class(ret$sparql) = 'sparql_string'
    f <- function(x) cat(x)
    assign('print.sparql_string', f, envir = .GlobalEnv)

    return(ret[!sapply(ret,isEmpty)])
  }")


  flatProps <- sapply(longProps, function(x) unname(sapply(x, unname, simplify = FALSE)), simplify = FALSE) %>% unname %>% unlist

  #doc <-getDescriptions(filter = list(class = className, property = flatProps), voidEndpoint)
  doc <-getDescriptions(filter = list(class = className, property = flatProps), endpoint)

  funcDoc <- doc$class$description
  propDoc <- doc$property
  doc$property$entity <- sub('(.*)[/|#]','',doc$property$entity)
  ret <- list()
  ret[[shortName]] <- list(func = func, funcDoc = funcDoc, propDoc = propDoc)
  return(ret)
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
    paste0(x, linePrefix, '{ ?', shortName, ' <',  propFilter[[y]], '> ?', y,"}\n" )
  }, names(propFilter), init = '')


  sparql <- paste0("SELECT *\n WHERE {\n", selectLimitClause, joinClause, "\n}")

  return(sparql)
}

isEmpty <- function(x) length(x) == 0
#print.sparql_string <- function(x) cat(x)
