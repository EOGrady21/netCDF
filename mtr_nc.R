####temperature recorder netCDF####


obj <- read.odf('C:/Users/ChisholmE/Documents/sample files/mtr/MTR_HUD2015030_1898_10546422_1800.ODF', header = 'list')
metadata <- ('C:/Users/ChisholmE/Documents/sample files/metadata/MTR_SAMPLE_METADATA.csv')


#' Moored temperature recorder netCDF template
#'
#' @param obj an odf object from oce which contains temperature data
#' @param metadata a csv file following the standard template which includes all
#'   necessary metadata
#' @param filename the desired name for the netCDF file produced, if left NULL
#'   the default will conform to BIO naming conventions
#'   
#' @return netCDF file with variables, temperature, time, time_String, station,
#'   latitude, longitude
#' @export
#'
#' @examples

mtr_nc <- function(obj, metadata, filename = NULL){
  
  require(oce)
  require(ncdf4)
  
  v <- names(obj@data)
  var <- obj@metadata$dataNamesOriginal
  
  for ( i in 1:length(var)){
    var[[i]] <- as.P01(var[[i]])
  }
  i <- 1
  
  for ( vv in var ){
    
    eval(parse(text = paste0("variable_", i, "<- '" , v[[i]], "'")))
    eval(parse(text= paste0("var",i," <-'", vv$gf3,"'")))
    eval(parse(text = paste0("units", i, " <-'", vv$units, "'")))
    eval(parse(text = paste0('P01_VAR', i," <- paste0('SDN:P01::', vv$P01)" )))
    eval(parse(text = paste0('P01_name_var', i," <-'" , vv$P01name , "'")))
    eval(parse(text = paste0('P06_var', i, "<-'" , vv$P06 , "'")))
    eval(parse(text = paste0('P06_name_var', i,  "<- '" , vv$P06name , "'")))
    eval(parse(text = paste0('var', i, 'max <-', -10000)))
    eval(parse(text = paste0('var', i, 'min <-' , 10000)))
    eval(parse(text = paste0("std_variable_", i, " <- '", vv$std, "'")))
    
    #check if variable also has quality flag
    if (v[[i]] %in% names(obj[['flags']])) {
      eval(parse(text = paste0("var", i, "_QC <- '", vv$gf3, "_QC'")))
      eval(parse(text = paste0("variable", i , "_QC <- 'quality flag for " , v[[i]], "'")))
    }
    i <- i+1
    
    
  }
  
  
  # variable_1 <- 'temperature'
  # var1 <- obj@metadata$dataNamesOriginal[[variable_1]]
  # units1 <- 'Degrees Celsius'
  # P01_VAR1 <- 'SDN:P01::TEMPPR01'
  # P01_name_var1 <- 'Temperature of the water body'
  # P06_var1 <- 'SDN:P06::UPAA'
  # P06_name_var1 <- 'Degrees Celsius'
  # std_variable_1 <- NULL
  # var1max <- 100
  # var1min <- -100
  
  
  #FILENAME
  if(missing(filename)){
    filename <- paste("MTR", '_', obj[['cruiseNumber']], '_',  obj[['eventNumber']],'_',  obj[['eventQualifier']],'_', obj[['samplingInterval']], '-', obj[['depthMin']], sep = '')
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
  
  
  
  #####write out definitions to new nc file####
  ncout <- nc_create(ncfname, list( t_def, lon_def, lat_def, ts_def, v1_def ), force_v4 = TRUE)
  
  
  ncvar_put(ncout, ts_def, obj[['time']])
  ncvar_put(ncout, t_def, as.POSIXct(obj[['time']], tz = 'UTC', origin = '1970-01-01 00:00:00'))
  ncvar_put(ncout, lon_def, obj[['longitude']])
  ncvar_put(ncout, lat_def, obj[['latitude']])
  ncvar_put(ncout, v1_def, obj[[variable_1]])
  
  
  ####metadata####
  ncatt_put(ncout, 'station', 'longitude', obj[['longitude']])
  ncatt_put(ncout, 'station', 'latitiude', obj[['latitude']])
  ncatt_put(ncout, 'station', 'standard_name', 'platform_name')
  ncatt_put(ncout, 'station', 'cf_role', 'timeseries_id')
  ncatt_put(ncout, 'time' , 'calendar', 'gregorian')
  ncatt_put(ncout, 'time_string', 'note', 'time values as ISO8601 string, YY-MM-DD hh:mm:ss')
  ncatt_put(ncout, 'time_string', 'time_zone', 'UTC')
  
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
  ncatt_put(ncout, 0, "data_type", 'MTR')
  ncatt_put(ncout, 0, "longitude", obj[['longitude']])
  ncatt_put(ncout, 0, "latitude", obj[['latitude']])
  ncatt_put(ncout, 0, "platform", obj[['cruise']])
  ncatt_put(ncout, 0, "sounding", obj[['sounding']])
  ncatt_put(ncout, 0, "chief_scientist", obj[['scientist']])
  ncatt_put(ncout, 0, "water_depth", obj[['waterDepth']])
  ncatt_put(ncout, 0, "cruise_name", obj[['cruise']])
  
  ####variables####
  
  ncatt_put(ncout, var1, 'reference_scale', 'IPTS-68')
  
  #sensor type, sensor depth and serial number for each variable
  
  ncatt_put(ncout, var1, "sensor_type", obj[['model']])
  ncatt_put(ncout, var1, "sensor_depth", obj[['depthMin']])
  ncatt_put(ncout, var1, "serial_number", obj[['serialNumber']])
  
  
  #generic names
  ncatt_put(ncout, var1, "generic_name", variable_1)
  
  
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
  
  
  ncatt_put(ncout, var1, "sdn_parameter_name", P01_name_var1)
  
  ncatt_put(ncout, "lon", "sdn_parameter_name", "Longitude east")
  ncatt_put(ncout, "lat", "sdn_parameter_name", "Latitude north")
  ncatt_put(ncout, 'ELTMEP01', "sdn_parameter_name", "Elapsed time (since 1970-01-01T00:00:00Z)")
  ncatt_put(ncout, 'time_string', "sdn_parameter_name", "String corresponding to format 'YYYY-MM-DDThh:mm:ss.sssZ' or other valid ISO8601 string")
  
  
  ncatt_put(ncout, var1, "sdn_uom_urn", P06_var1)
  
  ncatt_put(ncout, "lon", "sdn_uom_urn", "SDN:P06::DEGE")
  ncatt_put(ncout, "lat", "sdn_uom_urn", "SDN:P06:DEGN")
  ncatt_put(ncout, "ELTMEP01", "sdn_uom_urn", "SDN:P06::UTBB")
  ncatt_put(ncout, "time_string", "sdn_uom_urn", "SDN:P06::TISO")
  
  
  ncatt_put(ncout, var1, "sdn_uom_name", P06_name_var1)
  
  ncatt_put(ncout, "lon", "sdn_uom_name", "Degrees east")
  ncatt_put(ncout, "lat", "sdn_uom_name", "Degrees north")
  ncatt_put(ncout, "ELTMEP01", "sdn_uom_name", "Seconds")
  ncatt_put(ncout, "time_string", "sdn_uom_name", "ISO8601")
  
  
  #####CF standard names####
  ncatt_put(ncout, "ELTMEP01", "standard_name", "time")
  ncatt_put(ncout, "lat", "standard_name", "latitude")
  ncatt_put(ncout, "lon", "standard_name", "longitude")
  
  ncatt_put(ncout, var1, "standard_name", std_variable_1)
  
  
  ####data max and min####
  ncatt_put(ncout, var1, "data_max", max(obj[[variable_1]], na.rm = TRUE))
  ncatt_put(ncout, var1, "data_min", min(obj[[variable_1]], na.rm = TRUE))
  ncatt_put(ncout, var1, "valid_max", var1max)
  ncatt_put(ncout, var1, "valid_min", var1min)
  
  
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

  