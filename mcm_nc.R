#####mcm netCDF####

#' Current Meter netCDF template
#'
#' @param obj an odf object from oce which contains mcm data
#' @param metadata a csv file following the standard template which includes all necessary metadata 
#' @param filename the desired name for the netCDF file produced
#'
#' @return netCDF file with variables temperature, slainity, pressure, current
#'   direction, current speed, time, time string, latitude, longitude, station
#' @export
#'
#' @examples
mcm_nc <- function(obj, metadata, filename = NULL){
  require(oce)
  require(ncdf4)
  
  
  variable_1 <- 'temperature'
  var1 <- obj@metadata$dataNamesOriginal[[variable_1]]
  units1 <- obj@metadata$units[[variable_1]]$scale
  P01_VAR1 <- 'SDN:P01::TEMPPR01'
  P01_name_var1 <- 'Temperature of the water body'
  P06_var1 <- 'SDN:P06::UPAA'
  P06_name_var1 <- 'Degrees Celsius'
  std_variable_1 <- 'Temperature'
  var1max <- 100
  var1min <- -100
  
  
  variable_2 <- 'salinity'
  var2 <- obj@metadata$dataNamesOriginal[[variable_2]]
  units2 <- obj@metadata$units[[variable_2]]$scale
  P01_VAR2 <- 'SDN:P01::PSLTZZ01'
  P01_name_var2 <- 'Practical salinity of the water body'
  P06_var2 <- 'SDN:P06::UUUU'
  P06_name_var2 <- 'Dimensionless'
  std_variable_2 <- 'sea_water_practical_salinity'
  var2max <- 50
  var2min <- 0
  
  variable_3 <- 'pressure'
  var3 <- obj@metadata$dataNamesOriginal[[variable_3]]
  units3 <- obj@metadata$units[[variable_3]]$scale
  P01_VAR3 <- 'SDN:P01::PRESPR01'
  P01_name_var3 <- 'Pressure (spatial co-ordinate) exerted by the water body by profiling pressure sensor and corrected to read zero at sea level'
  P06_var3 <- 'SDN:P06::UPDB'
  P06_name_var3 <- 'Decibars'
  std_variable_3 <- 'sea_water_pressure'
  var3max <- -1000
  var3min <- 1000
  
  variable_4 <- 'directionTrue'
  var4 <- obj@metadata$dataNamesOriginal[[variable_4]]
  units4 <- obj@metadata$units[[variable_4]]$scale
  P01_VAR4 <- 'SDN:P01::LCDAZZ01'
  P01_name_var4 <- 'Current direction in the water body'
  P06_var4 <- 'SDN:P06::UAAA'
  P06_name_var4 <- 'Degrees'
  std_variable_4 <- 
  var4max <- 1000
  var4min <- -1000
  
  
  variable_5 <- 'speedHorizontal'
  var5 <- obj@metadata$dataNamesOriginal[[variable_5]]
  units5 <- obj@metadata$units[[variable_5]]$scale
  P01_VAR5 <- 'SDN:P01::LCSAZZ01'
  P01_name_var5 <- 'Current speed (Eulerian) in the water body'
  P06_var5 <- 'SDN:P06::ULAA'
  P06_name_var5 <- 'Metres per second'
  std_variable_5 <- 
  var5max <- 1000
  var5min <- -1000
  
  
  #FILENAME
  if(missing(filename)){
  filename <- paste("MCM", obj[['cruiseNumber']], obj[['eventNumber']], obj[['eventQualifier']], obj[['samplingInterval']], sep = '_')
  }
  ncpath <- "./"
  ncfname <- paste(ncpath, filename, ".nc", sep = "")
  
  #DIMENSIONS
  timedim <- ncdim_def("time", "seconds since 1970-01-01T00:00:00Z", as.double(obj[['time']]))
  stationdim <- ncdim_def("station", "counts", as.numeric(obj[['station']]))
  londim <- ncdim_def("lon", "degrees_east" , as.double(obj[['longitude']]))
  latdim <- ncdim_def("lat", "degrees_north", as.double(obj[['latitude']]))
  dimnchar <- ncdim_def('nchar', '', 1:23, create_dimvar = FALSE)
  
  #FILLVALUE
  FillValue <- 1e35
  
  #VARIABLES
  
  
  dlname <- 'lon'
  lon_def <- ncvar_def(longname= "longitude", units = 'degrees_east', dim = stationdim, name = dlname, prec = 'double')
  
  dlname <- 'lat'
  lat_def <- ncvar_def( longname = 'latitude', units = 'degrees_north', dim =  stationdim, name = dlname, prec = 'double')
  
  dlname <- "time_02"
  t_def <- ncvar_def("ELTMEP01", "seconds since 1970-01-01T00:00:00Z", list( stationdim, timedim), FillValue, dlname, prec = "double")
  
  dlname <- "time_string"
  ts_def <- ncvar_def("DTUT8601", units = "",dim =  list( dimnchar, timedim), missval = NULL, name =  dlname, prec = "char")
  
  dlname <- variable_1
  v1_def <- ncvar_def(var1, units1, list(timedim, stationdim), FillValue, dlname, prec = 'double')
  
  dlname <- variable_2
  v2_def <- ncvar_def(var2, units2, list(timedim, stationdim), FillValue, dlname, prec = 'double')
  
  dlname <- variable_3
  v3_def <- ncvar_def(var3, units3, list(timedim, stationdim), FillValue, dlname, prec = 'double')
  
  dlname <- variable_4
  v4_def <- ncvar_def(var4, units4, list(timedim, stationdim), FillValue, dlname, prec = 'double')
  
  dlname <- variable_5
  v5_def <- ncvar_def(var5, units5, list(timedim, stationdim), FillValue, dlname, prec = 'double')
  
 
  
  #####write out definitions to new nc file####
  ncout <- nc_create(ncfname, list( t_def, lon_def, lat_def, ts_def, v1_def, v2_def, v3_def, v4_def, v5_def), force_v4 = TRUE)
  
  
  ncvar_put(ncout, ts_def, obj[['time']])
  ncvar_put(ncout, t_def, as.POSIXct(obj[['time']], tz = 'UTC', origin = '1970-01-01 00:00:00'))
  ncvar_put(ncout, lon_def, obj[['longitude']])
  ncvar_put(ncout, lat_def, obj[['latitude']])
  ncvar_put(ncout, v1_def, obj[[variable_1]])
  ncvar_put(ncout, v2_def, obj[[variable_2]])
  ncvar_put(ncout, v3_def, obj[[variable_3]])
  ncvar_put(ncout, v4_def, obj[[variable_4]])
  ncvar_put(ncout, v5_def, obj[[variable_5]])

  
  
  ####metadata####
  ncatt_put(ncout, 'station', 'longitude', obj[['longitude']])
  ncatt_put(ncout, 'station', 'latitiude', obj[['latitude']])
  ncatt_put(ncout, 'station', 'standard_name', 'platform_name')
  ncatt_put(ncout, 'station', 'cf_role', 'timeseries_id')
  ncatt_put(ncout, 'time' , 'calendar', 'gregorian')
  ncatt_put(ncout, 'time_string', 'note', 'time values as ISO8601 string, YY-MM-DD hh:mm:ss')
  ncatt_put(ncout, 'time_string', 'time_zone', 'UTC')
  
  ####global####
  #might be different based on instrument
  
  #FROM CSV
  # ncatt_put(ncout, 0, 'keywords', 'Oceans > Ocean Circulation > Ocean Currents')
  # ncatt_put(ncout, 0, 'acknowledgment', obj[['acknowledgement']] )
  # ncatt_put(ncout, 0, 'comment', obj[['comment']])
  # ncatt_put(ncout, 0, 'cruise_description', obj[['cruise_description']])
  # ncatt_put(ncout, 0, 'date_created', date())
  # ncatt_put(ncout, 0, 'keywords_vocabulary', 'GCMD Science Keywords')
  # ncatt_put(ncout, 0, 'standard_name_vocabulary', obj[['standard_name_vocabulary']])
  # ncatt_put(ncout, 0, 'title', obj[['title']])
  # ncatt_put(ncout, 0, 'summary', obj[['summary']])
  # ncatt_put(ncout, 0, "naming_authority", obj[['naming_authority']])
  # ncatt_put(ncout, 0, "sea_name", obj[['sea_name']])
  # ncatt_put(ncout, 0, "publisher_name", obj[['publisher_name']])
  # ncatt_put(ncout, 0, "publisher_email", obj[['publisher_email']])
  # ncatt_put(ncout, 0, "program", obj[['description']])
  # ncatt_put(ncout, 0, "project", obj[['project']])
  # ncatt_put(ncout, 0, "featureType", obj[['featureType']])
  # ncatt_put(ncout, 0, "source", obj[['source']]) 

  
  #FROM ODF
  ncatt_put(ncout, 0, 'inst_type', obj[['type']])
  ncatt_put(ncout, 0, 'model', obj[['model']])
  ncatt_put(ncout, 0, 'sampling_interval', obj[['samplingInterval']])
  ncatt_put(ncout, 0, 'country_code', obj[['countryInstituteCode']])
  ncatt_put(ncout, 0, 'cruise_number', obj[['cruiseNumber']])
  ncatt_put(ncout, 0, "mooring_number", obj[['station']])
  ncatt_put(ncout, 0, "time_coverage_duration", (tail(obj[['time']], n = 1) - obj[['time']][[1]]))
  ncatt_put(ncout, 0, "time_coverage_duration_units", "days")
  ncatt_put(ncout, 0, "cdm_data_type", "station")
  ncatt_put(ncout, 0, "serial_number", obj[['serialNumber']])
  ncatt_put(ncout, 0, "data_type", 'MCM')
  ncatt_put(ncout, 0, "longitude", obj[['longitude']])
  ncatt_put(ncout, 0, "latitude", obj[['latitude']])
  ncatt_put(ncout, 0, "platform", obj[['cruise']])
  ncatt_put(ncout, 0, "sounding", obj[['sounding']])
  ncatt_put(ncout, 0, "chief_scientist", obj[['scientist']])
  ncatt_put(ncout, 0, "water_depth", obj[['waterDepth']])
  ncatt_put(ncout, 0, "cruise_name", obj[['cruise']])
  
  #ncatt_put(ncout, 0, "data_subtype", obj[['data_subtype']])?
  #ncatt_put(ncout, 0, "coord_system", obj[['coord_system']])?
  
  #in odf header
  # ncatt_put(ncout, 0, "processing_history", obj[['processing_history']])
  # ncatt_put(ncout, 0, "firmware_version", obj[['firmware_version']])
  # ncatt_put(ncout, 0, "history", obj[['history']])
  # ncatt_put(ncout, 0, "experiment", obj[['experiment']])
  # 
  
  ####variables####
  #sensor type, sensor depth and serial number for each variable
  
  ncatt_put(ncout, var1, "sensor_type", obj[['model']])
  ncatt_put(ncout, var1, "sensor_depth", obj[['depthMin']])
  ncatt_put(ncout, var1, "serial_number", obj[['serialNumber']])
  ncatt_put(ncout, var2, "sensor_type", obj[['model']])
  ncatt_put(ncout, var2, "sensor_depth", obj[['depthMin']])
  ncatt_put(ncout, var2, "serial_number", obj[['serialNumber']])
  ncatt_put(ncout, var3, "sensor_type", obj[['model']])
  ncatt_put(ncout, var3, "sensor_depth", obj[['depthMin']])
  ncatt_put(ncout, var3, "serial_number", obj[['serialNumber']])
  ncatt_put(ncout, var4, "sensor_type", obj[['model']])
  ncatt_put(ncout, var4, "sensor_depth", obj[['depthMin']])
  ncatt_put(ncout, var4, "serial_number", obj[['serialNumber']])
  ncatt_put(ncout, var5, "sensor_type", obj[['model']])
  ncatt_put(ncout, var5, "sensor_depth", obj[['depthMin']])
  ncatt_put(ncout, var5, "serial_number", obj[['serialNumber']])

  
  
  #generic names
  ncatt_put(ncout, var1, "generic_name", variable_1)
  ncatt_put(ncout, var2, "generic_name", variable_2)
  ncatt_put(ncout, var3, "generic_name", variable_3)
  ncatt_put(ncout, var4, "generic_name", variable_4)       
  ncatt_put(ncout, var5, "generic_name", variable_5)

  
  ####CF conventions & BODC standards####
  
 
  
  
  ncatt_put(ncout, 0, 'Conventions', 'CF-1.7')
  ncatt_put(ncout, 0, "creator_type", "person")
  
  ncatt_put(ncout, 0, "time_coverage_start", obj[['time']][1])
  ncatt_put(ncout, 0, "time_coverage_end", tail(obj[['time']], n= 1))
  ncatt_put(ncout, 0, "geospatial_lat_min", obj[['latitude']])
  ncatt_put(ncout, 0, "geospatial_lat_max", obj[['latitude']])
  ncatt_put(ncout, 0, "geospatial_lat_units", "degrees_north")
  ncatt_put(ncout, 0, "geospatial_lon_min", obj[['longitude']])
  ncatt_put(ncout, 0, "geospatial_lon_max", obj[['longitude']])
  ncatt_put(ncout, 0, "geospatial_lon_units", "degrees_east")
  ncatt_put(ncout, 0, "geospatial_vertical_max", obj[['depthMax']])
  ncatt_put(ncout, 0, "geospatial_vertical_min", obj[['depthMin']])
  ncatt_put(ncout, 0, "geospatial_vertical_units", "metres")
  ncatt_put(ncout, 0, "geospatial_vertical_positive", 'down')
  ncatt_put(ncout,0, "_FillValue", "1e35")
  ncatt_put(ncout, 0, "date_modified", date())
  ncatt_put(ncout, 0, "institution", obj[['institute']])
  
  
  ####BODC P01 names####
  ncatt_put(ncout, "ELTMEP01", "sdn_parameter_urn", "SDN:P01::ELTMEP01")
  ncatt_put(ncout, "lon", "sdn_parameter_urn", "SDN:P01::ALONZZ01")
  ncatt_put(ncout, "lat", "sdn_parameter_urn", "SDN:P01::ALATZZ01")
  ncatt_put(ncout, "time_string", "sdn_parameter_urn", "SDN:P01::DTUT8601")
  ncatt_put(ncout, var1, "sdn_parameter_urn", P01_VAR1)
  ncatt_put(ncout, var2, "sdn_parameter_urn", P01_VAR2)
  ncatt_put(ncout, var3, "sdn_parameter_urn", P01_VAR3)
  ncatt_put(ncout, var4, "sdn_parameter_urn", P01_VAR4)
  ncatt_put(ncout, var5, "sdn_parameter_urn", P01_VAR5)

  
  
  ncatt_put(ncout, var1, "sdn_parameter_name", P01_name_var1)
  ncatt_put(ncout, var2, "sdn_parameter_name", P01_name_var2)
  ncatt_put(ncout, var3, "sdn_parameter_name", P01_name_var3)
  ncatt_put(ncout, var4, "sdn_parameter_name", P01_name_var4)
  ncatt_put(ncout, var5, "sdn_parameter_name", P01_name_var5)

  
  ncatt_put(ncout, "lon", "sdn_parameter_name", "Longitude east")
  ncatt_put(ncout, "lat", "sdn_parameter_name", "Latitude north")
  ncatt_put(ncout, 'ELTMEP01', "sdn_parameter_name", "Elapsed time (since 1970-01-01T00:00:00Z)")
  ncatt_put(ncout, 'time_string', "sdn_parameter_name", "String corresponding to format 'YYYY-MM-DDThh:mm:ss.sssZ' or other valid ISO8601 string")
  
  
  ncatt_put(ncout, var1, "sdn_uom_urn", P06_var1)
  ncatt_put(ncout, var2, "sdn_uom_urn", P06_var2)
  ncatt_put(ncout, var3, "sdn_uom_urn", P06_var3)
  ncatt_put(ncout, var4, "sdn_uom_urn", P06_var4)
  ncatt_put(ncout, var5, "sdn_uom_urn", P06_var5)

  
  ncatt_put(ncout, "lon", "sdn_uom_urn", "SDN:P06::DEGE")
  ncatt_put(ncout, "lat", "sdn_uom_urn", "SDN:P06:DEGN")
  ncatt_put(ncout, "ELTMEP01", "sdn_uom_urn", "SDN:P06::UTBB")
  ncatt_put(ncout, "time_string", "sdn_uom_urn", "SDN:P06::TISO")
  
  ncatt_put(ncout, var1, "sdn_uom_name", P06_name_var1)
  ncatt_put(ncout, var2, "sdn_uom_name", P06_name_var2)
  ncatt_put(ncout, var3, "sdn_uom_name", P06_name_var3)
  ncatt_put(ncout, var4, "sdn_uom_name", P06_name_var4)
  ncatt_put(ncout, var5, "sdn_uom_name", P06_name_var5)

  
  ncatt_put(ncout, "lon", "sdn_uom_name", "Degrees east")
  ncatt_put(ncout, "lat", "sdn_uom_name", "Degrees north")
  ncatt_put(ncout, "ELTMEP01", "sdn_uom_name", "Seconds")
  ncatt_put(ncout, "time_string", "sdn_uom_name", "ISO8601")
  
  #####CF standard names####
  ncatt_put(ncout, "ELTMEP01", "standard_name", "time")
  ncatt_put(ncout, "lat", "standard_name", "latitude")
  ncatt_put(ncout, "lon", "standard_name", "longitude")
  
  ncatt_put(ncout, var1, "standard_name", std_variable_1)
  ncatt_put(ncout, var2, "standard_name", std_variable_2)
  ncatt_put(ncout, var3, "standard_name", std_variable_3)
  ncatt_put(ncout, var4, "standard_name", std_variable_4)
  ncatt_put(ncout, var5, "standard_name", std_variable_5)

  
  
  ####data max and min####
  ncatt_put(ncout, var1, "data_max", max(obj[[variable_1]], na.rm = TRUE))
  ncatt_put(ncout, var1, "data_min", min(obj[[variable_1]], na.rm = TRUE))
  ncatt_put(ncout, var1, "valid_max", var1max)
  ncatt_put(ncout, var1, "valid_min", var1min)
  
  ncatt_put(ncout, var2, "data_max", max(obj[[variable_2]], na.rm = TRUE))
  ncatt_put(ncout, var2, "data_min", min(obj[[variable_2]], na.rm = TRUE))
  ncatt_put(ncout, var2, "valid_max", var2max)
  ncatt_put(ncout, var2, "valid_min", var2min)
  
  ncatt_put(ncout, var3, "data_max", max(obj[[variable_3]], na.rm = TRUE))
  ncatt_put(ncout, var3, "data_min", min(obj[[variable_3]], na.rm = TRUE))
  ncatt_put(ncout, var3, "valid_max", var3max)
  ncatt_put(ncout, var3, "valid_min", var3min)
  
  ncatt_put(ncout, var4, "data_max", max(obj[[variable_4]], na.rm = TRUE))
  ncatt_put(ncout, var4, "data_min", min(obj[[variable_4]], na.rm = TRUE))
  ncatt_put(ncout, var4, "valid_max", var4max)
  ncatt_put(ncout, var4, "valid_min", var4min)
  
  ncatt_put(ncout, var5, "data_max", max(obj[[variable_5]], na.rm = TRUE))
  ncatt_put(ncout, var5, "data_min", min(obj[[variable_5]], na.rm = TRUE))
  ncatt_put(ncout, var5, "valid_max", var5max)
  ncatt_put(ncout, var5, "valid_min", var5min)
  

  
  #metadata from spreadsheet
  
  if (!missing(metadata)) {
    metad <- read.csv(metadata, header = TRUE)
    
    mn <- as.character(metad[,1])
    mv <- as.character(metad[,2])
    
    
    md <- as.list(mv)
    names(md) <- mn
    
    for (m in seq_along(md)) {
      ncatt_put(ncout, 0, names(md)[m], md[[m]])
    }
  }
  
  ####nc close####
  nc_close(ncout)
  
  
 
  
}
  
  