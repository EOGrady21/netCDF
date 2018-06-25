####MCM oce object####

library(oce)

filename <- 'mcm/MCM_HUD2013008_1844_602_3600.ODF'

#' MCM odf 2 oce
#'
#' @param filename odf file containing Moored current metre (MCM) data
#'
#' @return an oce object, cm - class containing all the data from the odf file
#' @export
#'
#' @examples
read.odf.cm <- function(filename ){

obj <- read.oce(filename)

speed <- obj[['speedHorizontal']]
direction <- obj[['directionTrue']]

deg2rad <- function(angle){
  angle * pi/180
}

u <- speed*cos(deg2rad(direction))
v <- speed*sin(deg2rad(direction))

mcm <- as.cm(
  time = obj@data$time,
   u = u,     #is this correct??
   v = v,
  pressure = obj@data$pressure,
  temperature = obj@data$temperature,
  salinity = obj@data$salinity,
  longitude = obj@metadata$initialLongitude,
  latitude = obj@metadata$initialLatitude
)


for (m in names(mcm@metadata)) {
  if (m != 'units' & m != 'flags' & m != 'dataNamesOriginal') {
    obj <- oceSetMetadata(obj, m, mcm[[m]], note = NULL)
  }
}

return(mcm)

}

