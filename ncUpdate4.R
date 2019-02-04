##nc update v4
#remove _FillValue
#remove ELTMEP01


rm(list = ls())

files <- list.files('C:/nco/', pattern = "*.nc")
st <- Sys.time()
for (i in 1:length(files)){
  
   input <- paste0("C:/nco/" , files[i])
  # output <- paste0(files[i])
  # final <- paste0(files[i])
  # 
   system2(command = 'C:/nco/ncatted.exe ',  args = eval(parse(
     text = paste0("' -a _FillValue,global,d,, -O ", input, " ", input, "'")
   )))
   print(paste("_FillValue removed for", files[i]))
   
   system2(command = 'C:/nco/ncks.exe',  args = eval(parse(
     text = paste0("' -x -v ELTMEP01 --overwrite ", input, " ", input, "'")
   )))
   print(paste("File", i, "of", length(files), "processed."))
}
et <- Sys.time()
et-st


