####MCM oce object####

library(oce)

filename <- 'mcm/MCM_HUD2013008_1844_602_3600.ODF'

read.odf.cm <- function(filename ){

obj <- read.oce(filename)

mcm <- as.cm(
  time = obj@data$time,
  u = obj@data$directionTrue,     #is this correct??
  v = obj@data$speedHorizontal,
  pressure = obj@data$pressure,
  temperature = obj@data$temperature,
  salinity = obj@data$salinity,
  longitude = obj@metadata$initialLongitude,
  latitude = obj@metadata$initialLatitude
)


for (m in names(mcm@metadata)) {
  if (m != 'units' & m != 'flags' & m != 'dataNamesOriginal') {
    adp <- oceSetMetadata(adp, m, mcm[[m]], note = NULL)
  }
}

return(mcm)

}

