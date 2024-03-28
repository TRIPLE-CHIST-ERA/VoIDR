fixTestFilePath <- function(path){
  if(dir.exists('inst')){
      return(paste0('inst/', path))
  } else {
    return(path)
  }
}
