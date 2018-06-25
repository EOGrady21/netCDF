####readodfmctd####

filename <- 'mctd/MCTD_HUD2015006_1897_11688_1800.ODF'

read.odf.mctd <- function(filename){
  
  obj <- read.odf(filename, header = 'list')
  
  mctd <- as.ctd(
    salinity = obj@data$salinity,
    temperature = obj@data$temperature,
    pressure = obj@data$pressure,
    conductivity = obj@data$conductivity,
    time = obj@data$time,
    
    units = obj@metadata$units,
    flags = obj@metadata$flags, 
    type = 'Moored'
  )
  
  mctd <-
    oceSetData(
      mctd,
      'sigmaTheta',
      obj@data$sigmaTheta,
      unit = obj@metadata$units$sigmaTheta,
      originalName = obj@metadata$dataNamesOriginal$sigmaTheta,
      note = NULL
    )
  mctd <-
    oceSetData(
      mctd,
      'potentialTemp',
      obj@data$theta,
      unit = obj@metadata$units$theta,
      originalName = obj@metadata$dataNamesOriginal$theta,
      note = NULL
    )
  mctd <-
    oceSetData(
      mctd,
      'oxygen',
      obj@data$oxygen,
      unit = obj@metadata$units$oxygen,
      originalName = obj@metadata$dataNamesOriginal,
      note = NULL
    )
  
  
  for (m in names(obj@metadata)) {
    if (m != 'units' & m != 'flags' & m != 'dataNamesOriginal') {
      mctd <- oceSetMetadata(mctd, m, obj[[m]], note = NULL)
    }
  }
  
  return(mctd)
  
}