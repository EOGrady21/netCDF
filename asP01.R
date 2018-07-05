####GF3 2 P01####

#' GF# to P01
#' 
#' Use this function to map gf3 codes to P01 codes for exporting to netCDF
#'
#' @param gf3 a gf3 standard code paramater
#'
#' @return a matching P01 value with units and standard name (if applicable)
#' @export
#'
#' @examples
as.P01 <- function(gf3){
  gf32p01 <- read.csv('c:/Users/ChisholmE/Documents/sample files/GF3 Code Map.csv', header = TRUE)
  
 
  
  line <- grep(gf32p01$GF3.code, pattern = gf3)
  
  if (length(line) == 0){
    yn <- list()
    for (i in 1:length(gf32p01$GF3.code)){
      yn[[i]] <- grep( pattern = gf32p01$GF3.code[[i]], x = gf3, value = TRUE)
      if(length(yn[[i]] != 0)){
        line <- i
      }
    }
    
  }
  if (length(line) == 0){
    warning(paste(gf3, 'not recognized in list of GF3 codes!'))
    stop()
  }
  
  gf3 <- list(gf3 = gf3)
  gf3$P01 <- as.character(gf32p01$P01.code[[line]])
  gf3$P01name <-as.character(gf32p01$P01..preferred.name[[line]])
  gf3$P06 <- as.character(gf32p01$P06.unit.code[[line]])
  gf3$P06name <- as.character(gf32p01$P06.unit.name[[line]])
  gf3$units <- as.character(gf32p01$units[[line]])
  gf3$std <- as.character(gf32p01$standard_name[[line]])
  
  return(gf3)
}