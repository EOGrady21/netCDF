##nc update v5
#edit CMAG variables to P01 standard


#check which files contain CMAG

files <- list.files('D:/madcp/')
lst <- list()
for ( i in 1:length(files)){
  nc <- nc_open(paste0('D:/madcp/', files[i]))
  l <- grep(names(nc$var), pattern = 'CMAG')
  if (length(l) != 0){
    lst[i] <- files[i]
  }
  nc_close(nc)
}

#start replacing
#CMAG_01 == CMAGZZ01

#rm(list = ls())

st <- Sys.time()
for (i in 1:length(files)){
  
   input <- paste0("D:/madcp/" , files[i])
  # output <- paste0(files[i])
  # final <- paste0(files[i])
  # 
   
   ####calls nco executable and edits netCDF####
   system2(command = 'C:/nco/ncrename.exe ',  args = eval(parse(
     text = paste0("' -v CMAG_01,CMAGZZ01 -v CMAG_02,CMAGZZ02 -v CMAG_03,CMAGZZ03 -v CMAG_04,CMAGZZ04 ", input, "'")
   )))
   ####    ####
   print(paste("CMAG names updated for", files[i]))
   
   print(paste("File", i, "of", length(files), "processed."))
}
et <- Sys.time()
et-st


