####profile ctd netCDF template####



obj <- read.odf('C:/Users/ChisholmE/Documents/sample files/ctd/CTD_BCD2017666_01_01_DN.ODF', header = 'list')
upcast <- read.odf('C:/Users/ChisholmE/Documents/sample files/ctd/CTD_BCD2017666_01_01_UP.ODF', header = 'list')
metadata <- ('C:/Users/ChisholmE/Documents/sample files/metadata/CTD_SAMPLE_METADATA.csv')

source('asP01.R')

#'  CTD netCDF template
#'
#' @param obj an odf object from oce which contains mctd data
#' @param metadata a csv file following the standard template which includes all
#'   necessary metadata
#' @param filename the desired name for the netCDF file produced, if left NULL
#'   the default will conform to BIO naming conventions
#' 
#'   
#' @return netCDF file with variables temperature, conductivity, pressure, sigma
#'   theta, oxygen, salinity, station, latitude, longitude, oxygen voltage,
#'   fluoresence, par, scan, scan2 and QC flag variables for each data variable
#'   
#' @export
#'
#' @examples
#' 

ctd_nc <- function(obj, upcast = NULL, metadata, filename = NULL){
  
 
  #input varaibles automatically from obj@data
  
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
    if(!is.null(vv$std)){
      eval(parse(text = paste0("std_variable_", i, " <- '", vv$std, "'")))
    }else{
      eval(parse(text = paste0("std_variable_", i, " <- NULL")))
    }
    #check if variable also has quality flag
   if (v[[i]] %in% names(obj[['flags']])) {
    eval(parse(text = paste0("var", i, "_QC <- '", vv$gf3, "_QC'")))
    eval(parse(text = paste0("variable", i , "_QC <- 'quality flag for " , v[[i]], "'")))
   }
    i <- i+1
    
    
  }
  numvar <- length(var)
  if (numvar >12){
    warning("Maximum of 12 variables exceeded! Not all data has been exported!")
  }
  numflag <- length(var) #FIXME: change so that function can handle more flags than variables
  
  
   ####combine up and down cast####
   if (!is.null(upcast)) {
     if (obj[['startTime']] == upcast[['startTime']]) {
       #if (names(upcast@data) == names(obj@data)) {
         for (l in 1:length(v)) {
           for (i in 1: length(upcast@data[[1]])){
             eval(parse(text = paste0("obj[['", v[[l]], "']][length(obj[['", v[[l]], "']]) + 1] <- upcast[['", v[[l]], "']][", i, "]")))
 #add flags
              eval(parse(text = paste0("obj@metadata$flags[['", v[[l]], "']][length(obj@metadata$flags[['", v[[l]], "']]) +1] <- upcast@metadata$flags[['", v[[l]], "']][", i, "]")))
           }
         }
       # } else{
       #   warning('UPCAST VARIABLES DO NOT MATCH DOWNCAST!')
       # }
       
     } else{
       warning('UPCAST START TIME DOES NOT MATCH DOWNCAST START TIME! DOUBLE CHECK FILES!')
       stop()
     }
   }
   
  ####filename####
  if(missing(filename)){
    filename <- paste("CTD", obj[['cruiseNumber']], obj[['eventNumber']], obj[['eventQualifier']], 'DN', sep = '_')
  }
  ncpath <- "./"
  ncfname <- paste(ncpath, filename, ".nc", sep = "")
  
  ####setting dimensions and definitions####
  
 
  presdim <- ncdim_def("pressure", "decibars", as.double(obj[['pressure']]))
  stationdim <- ncdim_def("station", "counts", as.numeric(obj[['station']]))
  londim <- ncdim_def("lon", "degrees_east" , as.double(obj[['longitude']]))
  latdim <- ncdim_def("lat", "degrees_north", as.double(obj[['latitude']]))
  dimnchar <- ncdim_def('nchar', '', 1:23, create_dimvar = FALSE)
  
  #set fill value
  FillValue <- 1e35
  #####define variables####

  dlname <- 'lon'
  lon_def <- ncvar_def(longname= "longitude", units = 'degrees_east', dim = stationdim, name = dlname, prec = 'double')
  
  dlname <- 'lat'
  lat_def <- ncvar_def( longname = 'latitude', units = 'degrees_north', dim =  stationdim, name = dlname, prec = 'double')
  
  dlname <- variable_1
  v1_def <- ncvar_def(var1, units1, list(presdim, stationdim), FillValue, dlname, prec = 'double')
  
  
  if (numvar >1){
  dlname <- variable_2
  v2_def <- ncvar_def(var2, units2, list(presdim, stationdim), FillValue, dlname, prec = 'double')
  
  
  if (numvar >2){
  dlname <- variable_3
  v3_def <- ncvar_def(var3, units3, list(presdim, stationdim), FillValue, dlname, prec = 'double')
  
  
  if (numvar >3){
  dlname <- variable_4
  v4_def <- ncvar_def(var4, units4, list(presdim, stationdim), FillValue, dlname, prec = 'double')
  
 
  if (numvar >4){
  dlname <- variable_5
  v5_def <- ncvar_def(var5, units5, list(presdim, stationdim), FillValue, dlname, prec = 'double')
  
  
  if (numvar >5){
  dlname <- variable_6
  v6_def <- ncvar_def(var6, units6, list(presdim, stationdim), FillValue, dlname, prec = 'double')
  
  
  if (numvar >6){
  dlname <- variable_7
  v7_def <- ncvar_def(var7, units7, list(presdim, stationdim), FillValue, dlname, prec = 'double')
  
  
  if (numvar >7){
  dlname <- variable_8
  v8_def <- ncvar_def(var8, units8, list(presdim, stationdim), FillValue, dlname, prec = 'double')
  
  
  if (numvar > 8 ){
  dlname <- variable_9
  v9_def <- ncvar_def(var9, units9, list(presdim, stationdim), FillValue, dlname, prec = 'double')
  
  
  if (numvar > 9 ){
  dlname <- variable_10
  v10_def <- ncvar_def(var10, units10, list(presdim, stationdim), FillValue, dlname, prec = 'double')
  
  
  if (numvar >10){
  dlname <- variable_11
  v11_def <- ncvar_def(var11, units11, list(presdim, stationdim), FillValue, dlname, prec = 'double')
  
  if (numvar >11){
    dlname <- variable_12
    v12_def <- ncvar_def(var12, units12, list(presdim, stationdim), FillValue, dlname, prec = 'double')
    
    
  }
  
  }
  }
  }
  }
  }
  }
  }
  }
  }
  }
  
  
  dlname <- variable1_QC
  v1qc_def <- ncvar_def(var1_QC, units = '', list(presdim, stationdim), missval = 0, dlname, prec = 'integer')
  
  if (numflag > 1){
  dlname <- variable2_QC
  v2qc_def <- ncvar_def(var2_QC, units = '', list(presdim, stationdim), missval = 0, dlname, prec = 'integer')
  
  if (numflag >2){
  dlname <- variable3_QC
  v3qc_def <- ncvar_def(var3_QC, units = '', list(presdim, stationdim), missval = 0, dlname, prec = 'integer')
  
  if (numflag >3){
  dlname <- variable4_QC
  v4qc_def <- ncvar_def(var4_QC, units = '', list(presdim, stationdim), missval = 0, dlname, prec = 'integer')
  
  if (numflag >4){
  dlname <- variable5_QC
  v5qc_def <- ncvar_def(var5_QC, units = '', list(presdim, stationdim), missval = 0, dlname, prec = 'integer')
  
  if (numflag >5){
  dlname <- variable6_QC
  v6qc_def <- ncvar_def(var6_QC, units = '', list(presdim, stationdim), missval = 0, dlname, prec = 'integer')
  
  if (numflag >6){
  dlname <- variable7_QC
  v7qc_def <- ncvar_def(var7_QC, units = '', list(presdim, stationdim), missval = 0, dlname, prec = 'integer')
  
  if (numflag >7){
  dlname <- variable8_QC
  v8qc_def <- ncvar_def(var8_QC, units = '', list(presdim, stationdim), missval = 0, dlname, prec = 'integer')
  
  if (numflag > 8){
  dlname <- variable9_QC
  v9qc_def <- ncvar_def(var9_QC, units = '', list(presdim, stationdim), missval = 0, dlname, prec = 'integer')
  
  if (numflag >9){
  dlname <- variable10_QC
  v10qc_def <- ncvar_def(var10_QC, units = '', list(presdim, stationdim), missval = 0, dlname, prec = 'integer')
  
  if (numflag >10){
  dlname <- variable11_QC
  v11qc_def <- ncvar_def(var11_QC, units = '', list(presdim, stationdim), missval = 0, dlname, prec = 'integer')
  
  if (numflag >11){
    dlname <- variable12_QC
    v12qc_def <- ncvar_def(var12_QC, units = '', list(presdim, stationdim), missval = 0, dlname, prec = 'integer')
  }
  }
  }
  }
  }
  }
  }
  }
  }
  }
  }
  
  
  #####write out definitions to new nc file####
defs <- grep(ls(), pattern = '_def', value = TRUE)
  dd <- NULL
  for ( i in 1:length(defs)){
    eval(parse(text = paste0("dd[[i]] <- ", defs[[i]])))
  }
  ncout <-
    nc_create(
      ncfname,
      
        dd
      ,
      force_v4 = TRUE
    )
  
  
  ncvar_put(ncout, lon_def, obj[['longitude']])
  ncvar_put(ncout, lat_def, obj[['latitude']])
  
  ncvar_put(ncout, v1_def, obj[[variable_1]])
  ncvar_put(ncout, v2_def, obj[[variable_2]])
  ncvar_put(ncout, v3_def, obj[[variable_3]])
  ncvar_put(ncout, v4_def, obj[[variable_4]])
  ncvar_put(ncout, v5_def, obj[[variable_5]])
  ncvar_put(ncout, v6_def, obj[[variable_6]])
  ncvar_put(ncout, v7_def, obj[[variable_7]])
  ncvar_put(ncout, v8_def, obj[[variable_8]])
  ncvar_put(ncout, v9_def, obj[[variable_9]])
  ncvar_put(ncout, v10_def, obj[[variable_10]])
  ncvar_put(ncout, v11_def, obj[[variable_11]])
  
  ncvar_put(ncout, v1qc_def, obj@metadata$flags[[variable_1]] )
  ncvar_put(ncout, v2qc_def, obj@metadata$flags[[variable_2]] )
  ncvar_put(ncout, v3qc_def, obj@metadata$flags[[variable_3]] )
  ncvar_put(ncout, v4qc_def, obj@metadata$flags[[variable_4]] )
  ncvar_put(ncout, v5qc_def, obj@metadata$flags[[variable_5]] )
  ncvar_put(ncout, v6qc_def, obj@metadata$flags[[variable_6]] )
  ncvar_put(ncout, v7qc_def, obj@metadata$flags[[variable_7]] )
  ncvar_put(ncout, v8qc_def, obj@metadata$flags[[variable_8]] )
  ncvar_put(ncout, v9qc_def, obj@metadata$flags[[variable_9]] )
  ncvar_put(ncout, v10qc_def, obj@metadata$flags[[variable_10]] )
  ncvar_put(ncout, v11qc_def, obj@metadata$flags[[variable_11]] )
  
  
  
  ####metadata####
  ####metadata####
  ncatt_put(ncout, 'station', 'longitude', obj[['longitude']])
  ncatt_put(ncout, 'station', 'latitiude', obj[['latitude']])
  ncatt_put(ncout, 'station', 'standard_name', 'platform_name')
  ncatt_put(ncout, 'station', 'cf_role', 'profile_id')
 
  
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
  ncatt_put(ncout, 0, 'country_code', obj[['countryInstituteCode']])
  ncatt_put(ncout, 0, 'cruise_number', obj[['cruiseNumber']])
  ncatt_put(ncout, 0, "mooring_number", obj[['station']])
  ncatt_put(ncout, 0, "cdm_data_type", "profile")
  ncatt_put(ncout, 0, "serial_number", obj[['serialNumber']])
  ncatt_put(ncout, 0, "data_type", 'CTD')
  ncatt_put(ncout, 0, "longitude", obj[['longitude']])
  ncatt_put(ncout, 0, "latitude", obj[['latitude']])
  ncatt_put(ncout, 0, "platform", obj[['cruise']])
  ncatt_put(ncout, 0, "sounding", obj[['sounding']])
  ncatt_put(ncout, 0, "chief_scientist", obj[['scientist']])
  ncatt_put(ncout, 0, "water_depth", obj[['waterDepth']])
  ncatt_put(ncout, 0, "cruise_name", obj[['cruise']])
  ncatt_put(ncout, 0, "cruise_description", obj@metadata$header$CRUISE_HEADER$CRUISE_DESCRIPTION)
  ncatt_put(ncout, 0, "program", obj@metadata$header$CRUISE_HEADER$CRUISE_DESCRIPTION)
  
  
  #ncatt_put(ncout, 0, "data_subtype", obj[['data_subtype']])?
  #ncatt_put(ncout, 0, "coord_system", obj[['coord_system']])?
  
  #in odf header
  # ncatt_put(ncout, 0, "processing_history", obj[['processing_history']])
  # ncatt_put(ncout, 0, "firmware_version", obj[['firmware_version']])
  # ncatt_put(ncout, 0, "history", obj[['history']])
  # ncatt_put(ncout, 0, "experiment", obj[['experiment']])
  # 
  
  
  ####variables####
  
  ###ancillary variables
  
  ncatt_put(ncout, var1, 'ancillary_variables', var1_QC)
  ncatt_put(ncout, var2, 'ancillary_variables', var2_QC)
  ncatt_put(ncout, var3, 'ancillary_variables', var3_QC)
  ncatt_put(ncout, var4, 'ancillary_variables', var4_QC)
  ncatt_put(ncout, var5, 'ancillary_variables', var5_QC)
  ncatt_put(ncout, var6, 'ancillary_variables', var6_QC)
  ncatt_put(ncout, var7, 'ancillary_variables', var7_QC)
  ncatt_put(ncout, var8, 'ancillary_variables', var8_QC)
  ncatt_put(ncout, var9, 'ancillary_variables', var9_QC)
  ncatt_put(ncout, var10, 'ancillary_variables', var10_QC)
  ncatt_put(ncout, var11, 'ancillary_variables', var11_QC)
  
  
  ##reference scales
  
  ncatt_put(ncout, var1, 'reference_scale', 'IPTS-68')
  
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
  ncatt_put(ncout, var6, "sensor_type", obj[['model']])
  ncatt_put(ncout, var6, "sensor_depth", obj[['depthMin']])
  ncatt_put(ncout, var6, "serial_number", obj[['serialNumber']])
  ncatt_put(ncout, var7, "sensor_type", obj[['model']])
  ncatt_put(ncout, var7, "sensor_depth", obj[['depthMin']])
  ncatt_put(ncout, var7, "serial_number", obj[['serialNumber']])
  ncatt_put(ncout, var8, "sensor_type", obj[['model']])
  ncatt_put(ncout, var8, "sensor_depth", obj[['depthMin']])
  ncatt_put(ncout, var8, "serial_number", obj[['serialNumber']])
  ncatt_put(ncout, var9, "sensor_type", obj[['model']])
  ncatt_put(ncout, var9, "sensor_depth", obj[['depthMin']])
  ncatt_put(ncout, var9, "serial_number", obj[['serialNumber']])
  ncatt_put(ncout, var10, "sensor_type", obj[['model']])
  ncatt_put(ncout, var10, "sensor_depth", obj[['depthMin']])
  ncatt_put(ncout, var10, "serial_number", obj[['serialNumber']])
  ncatt_put(ncout, var11, "sensor_type", obj[['model']])
  ncatt_put(ncout, var11, "sensor_depth", obj[['depthMin']])
  ncatt_put(ncout, var11, "serial_number", obj[['serialNumber']])
  
  
  
  
  #generic names
  ncatt_put(ncout, var1, "generic_name", variable_1)
  ncatt_put(ncout, var2, "generic_name", variable_2)
  ncatt_put(ncout, var3, "generic_name", variable_3)
  ncatt_put(ncout, var4, "generic_name", variable_4)       
  ncatt_put(ncout, var5, "generic_name", variable_5)
  ncatt_put(ncout, var6, "generic_name", variable_6)
  ncatt_put(ncout, var7, "generic_name", variable_7)
  ncatt_put(ncout, var8, "generic_name", variable_8)
  ncatt_put(ncout, var9, "generic_name", variable_9)
  ncatt_put(ncout, var10, "generic_name", variable_10)
  ncatt_put(ncout, var11, "generic_name", variable_11)
  
  
  
  ####CF conventions & BODC standards####
  ncatt_put(ncout, 0, 'Conventions', 'CF-1.7')
  ncatt_put(ncout, 0, "creator_type", "person")
  ncatt_put(ncout, 0, "time_coverage_start", as.character(as.POSIXct(obj[['startTime']])))
  ncatt_put(ncout, 0, "time_coverage_end", as.character(as.POSIXct(obj[['startTime']])))
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
  
  #new requirements # in csv
  # ncatt_put(ncout, 0, "sea_name", obj[['sea_name']])
  # ncatt_put(ncout, 0, "processing_history", obj[['processing_history']])
  # ncatt_put(ncout, 0, "source", obj[['source']]) 
  # ncatt_put(ncout, 0, "publisher_name", obj[['publisher_name']])
  # ncatt_put(ncout, 0, "publisher_email", obj[['publisher_email']])
  
  
  
  ####BODC P01 names####
  ncatt_put(ncout, var1, "sdn_parameter_urn", P01_VAR1)
  ncatt_put(ncout, var2, "sdn_parameter_urn", P01_VAR2)
  ncatt_put(ncout, var3, "sdn_parameter_urn", P01_VAR3)
  ncatt_put(ncout, var4, "sdn_parameter_urn", P01_VAR4)
  ncatt_put(ncout, var5, "sdn_parameter_urn", P01_VAR5)
  ncatt_put(ncout, var6, "sdn_parameter_urn", P01_VAR6)
  ncatt_put(ncout, var7, "sdn_parameter_urn", P01_VAR7)
  ncatt_put(ncout, var8, "sdn_parameter_urn", P01_VAR8)
  ncatt_put(ncout, var9, "sdn_parameter_urn", P01_VAR9)
  ncatt_put(ncout, var10, "sdn_parameter_urn", P01_VAR10)
  ncatt_put(ncout, var11, "sdn_parameter_urn", P01_VAR11)
  
  ncatt_put(ncout, "lon", "sdn_parameter_urn", "SDN:P01::ALONZZ01")
  ncatt_put(ncout, "lat", "sdn_parameter_urn", "SDN:P01::ALATZZ01")
  
  
  ncatt_put(ncout, var1, "sdn_parameter_name", P01_name_var1)
  ncatt_put(ncout, var2, "sdn_parameter_name", P01_name_var2)
  ncatt_put(ncout, var3, "sdn_parameter_name", P01_name_var3)
  ncatt_put(ncout, var4, "sdn_parameter_name", P01_name_var4)
  ncatt_put(ncout, var5, "sdn_parameter_name", P01_name_var5)
  ncatt_put(ncout, var6, "sdn_parameter_name", P01_name_var6)
  ncatt_put(ncout, var7, "sdn_parameter_name", P01_name_var7)
  ncatt_put(ncout, var8, "sdn_parameter_name", P01_name_var8)
  ncatt_put(ncout, var9, "sdn_parameter_name", P01_name_var9)
  ncatt_put(ncout, var10, "sdn_parameter_name", P01_name_var10)
  ncatt_put(ncout, var11, "sdn_parameter_name", P01_name_var11)
  
  ncatt_put(ncout, "lon", "sdn_parameter_name", "Longitude east")
  ncatt_put(ncout, "lat", "sdn_parameter_name", "Latitude north")
  
  
  
  ncatt_put(ncout, var1, "sdn_uom_urn", P06_var1)
  ncatt_put(ncout, var2, "sdn_uom_urn", P06_var2)
  ncatt_put(ncout, var3, "sdn_uom_urn", P06_var3)
  ncatt_put(ncout, var4, "sdn_uom_urn", P06_var4)
  ncatt_put(ncout, var5, "sdn_uom_urn", P06_var5)
  ncatt_put(ncout, var6, "sdn_uom_urn", P06_var6)
  ncatt_put(ncout, var7, "sdn_uom_urn", P06_var7)
  ncatt_put(ncout, var8, "sdn_uom_urn", P06_var8)
  ncatt_put(ncout, var9 , "sdn_uom_urn", P06_var9)
  ncatt_put(ncout, var10, "sdn_uom_urn", P06_var10)
  ncatt_put(ncout, var11, "sdn_uom_urn", P06_var11)
  
  ncatt_put(ncout, "lon", "sdn_uom_urn", "SDN:P06::DEGE")
  ncatt_put(ncout, "lat", "sdn_uom_urn", "SDN:P06:DEGN")
  
  ncatt_put(ncout, var1, "sdn_uom_name", P06_name_var1)
  ncatt_put(ncout, var2, "sdn_uom_name", P06_name_var2)
  ncatt_put(ncout, var3, "sdn_uom_name", P06_name_var3)
  ncatt_put(ncout, var4, "sdn_uom_name", P06_name_var4)
  ncatt_put(ncout, var5, "sdn_uom_name", P06_name_var5)
  ncatt_put(ncout, var6, "sdn_uom_name", P06_name_var6)
  ncatt_put(ncout, var7, "sdn_uom_name", P06_name_var7)
  ncatt_put(ncout, var8, "sdn_uom_name", P06_name_var8)
  ncatt_put(ncout, var9, "sdn_uom_name", P06_name_var9)
  ncatt_put(ncout, var10, "sdn_uom_name", P06_name_var10)
  ncatt_put(ncout, var11, "sdn_uom_name", P06_name_var11)
  
  
  ncatt_put(ncout, "lon", "sdn_uom_name", "Degrees east")
  ncatt_put(ncout, "lat", "sdn_uom_name", "Degrees north")
  
  
  #####CF standard names####

  ncatt_put(ncout, "lat", "standard_name", "latitude")
  ncatt_put(ncout, "lon", "standard_name", "longitude")
  
  ncatt_put(ncout, var1, "standard_name", std_variable_1)
  ncatt_put(ncout, var2, "standard_name", std_variable_2)
  ncatt_put(ncout, var3, "standard_name", std_variable_3)
  ncatt_put(ncout, var4, "standard_name", std_variable_4)
  ncatt_put(ncout, var5, "standard_name", std_variable_5)
  ncatt_put(ncout, var6, "standard_name", std_variable_6)
  ncatt_put(ncout, var7, "standard_name", std_variable_7)
  ncatt_put(ncout, var8, "standard_name", std_variable_8)
  ncatt_put(ncout, var9, "standard_name", std_variable_9)
  ncatt_put(ncout, var10, "standard_name", std_variable_10)
  ncatt_put(ncout, var11, "standard_name", std_variable_11)
  
  
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
  
  ncatt_put(ncout, var6, "data_max", max(obj[[variable_6]], na.rm = TRUE))
  ncatt_put(ncout, var6, "data_min", min(obj[[variable_6]], na.rm = TRUE))
  ncatt_put(ncout, var6, "valid_max", var6max)
  ncatt_put(ncout, var6, "valid_min", var6min)
  
  ncatt_put(ncout, var7, "data_max", max(obj[[variable_7]], na.rm = TRUE))
  ncatt_put(ncout, var7, "data_min", min(obj[[variable_7]], na.rm = TRUE))
  ncatt_put(ncout, var7, "valid_max", var7max)
  ncatt_put(ncout, var7, "valid_min", var7min)
  
  ncatt_put(ncout, var8, "data_max", max(obj[[variable_8]], na.rm = TRUE))
  ncatt_put(ncout, var8, "data_min", min(obj[[variable_8]], na.rm = TRUE))
  ncatt_put(ncout, var8, "valid_max", var8max)
  ncatt_put(ncout, var8, "valid_min", var8min)
  
  ncatt_put(ncout, var9, "data_max", max(obj[[variable_9]], na.rm = TRUE))
  ncatt_put(ncout, var9, "data_min", min(obj[[variable_9]], na.rm = TRUE))
  ncatt_put(ncout, var9, "valid_max", var9max)
  ncatt_put(ncout, var9, "valid_min", var9min)
  
  ncatt_put(ncout, var10, "data_max", max(obj[[variable_10]], na.rm = TRUE))
  ncatt_put(ncout, var10, "data_min", min(obj[[variable_10]], na.rm = TRUE))
  ncatt_put(ncout, var10, "valid_max", var10max)
  ncatt_put(ncout, var10, "valid_min", var10min)
  
  ncatt_put(ncout, var11, "data_max", max(obj[[variable_11]], na.rm = TRUE))
  ncatt_put(ncout, var11, "data_min", min(obj[[variable_11]], na.rm = TRUE))
  ncatt_put(ncout, var11, "valid_max", var11max)
  ncatt_put(ncout, var11, "valid_min", var11min)
  
  
  ####QUALITY CONTROL####
  
  ncatt_put(ncout, var1_QC, 'flag_values', c(0:9))
  ncatt_put(ncout, var1_QC, 'flag_meanings', 'none good probably_good probably_bad bad changed BD excess interpolated missing')
  ncatt_put(ncout, var1_QC, 'references', 'http://www.iode.org/mg22')
  
  ncatt_put(ncout, var2_QC, 'flag_values', c(0:9))
  ncatt_put(ncout, var2_QC, 'flag_meanings', 'none good probably_good probably_bad bad changed BD excess interpolated missing')
  ncatt_put(ncout, var2_QC, 'references', 'http://www.iode.org/mg22')
  
  ncatt_put(ncout, var3_QC, 'flag_values', c(0:9))
  ncatt_put(ncout, var3_QC, 'flag_meanings', 'none good probably_good probably_bad bad changed BD excess interpolated missing')
  ncatt_put(ncout, var3_QC, 'references', 'http://www.iode.org/mg22')
  
  ncatt_put(ncout, var4_QC, 'flag_values', c(0:9))
  ncatt_put(ncout, var4_QC, 'flag_meanings', 'none good probably_good probably_bad bad changed BD excess interpolated missing')
  ncatt_put(ncout, var4_QC, 'references', 'http://www.iode.org/mg22')
  
  ncatt_put(ncout, var5_QC, 'flag_values', c(0:9))
  ncatt_put(ncout, var5_QC, 'flag_meanings', 'none good probably_good probably_bad bad changed BD excess interpolated missing')
  ncatt_put(ncout, var5_QC, 'references', 'http://www.iode.org/mg22')
  
  ncatt_put(ncout, var6_QC, 'flag_values', c(0:9))
  ncatt_put(ncout, var6_QC, 'flag_meanings', 'none good probably_good probably_bad bad changed BD excess interpolated missing')
  ncatt_put(ncout, var6_QC, 'references', 'http://www.iode.org/mg22')
  
  ncatt_put(ncout, var7_QC, 'flag_values', c(0:9))
  ncatt_put(ncout, var7_QC, 'flag_meanings', 'none good probably_good probably_bad bad changed BD excess interpolated missing')
  ncatt_put(ncout, var7_QC, 'references', 'http://www.iode.org/mg22')
  
  ncatt_put(ncout, var8_QC, 'flag_values', c(0:9))
  ncatt_put(ncout, var8_QC, 'flag_meanings', 'none good probably_good probably_bad bad changed BD excess interpolated missing')
  ncatt_put(ncout, var8_QC, 'references', 'http://www.iode.org/mg22')
  
  ncatt_put(ncout, var9_QC, 'flag_values', c(0:9))
  ncatt_put(ncout, var9_QC, 'flag_meanings', 'none good probably_good probably_bad bad changed BD excess interpolated missing')
  ncatt_put(ncout, var9_QC, 'references', 'http://www.iode.org/mg22')
  
  ncatt_put(ncout, var10_QC, 'flag_values', c(0:9))
  ncatt_put(ncout, var10_QC, 'flag_meanings', 'none good probably_good probably_bad bad changed BD excess interpolated missing')
  ncatt_put(ncout, var10_QC, 'references', 'http://www.iode.org/mg22')
  
  ncatt_put(ncout, var11_QC, 'flag_values', c(0:9))
  ncatt_put(ncout, var11_QC, 'flag_meanings', 'none good probably_good probably_bad bad changed BD excess interpolated missing')
  ncatt_put(ncout, var11_QC, 'references', 'http://www.iode.org/mg22')
  
  
  #metadata from spreadsheet
  
  #avoid printing variables as metadata
 
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
  
  ####preserve ODF history header####
  if (!is.null(obj@metadata$header)){
    if (length(obj@metadata$header) != 0){
    head <- obj@metadata$header
    hi <- list(grep(names(head), pattern = "HISTORY"))
    if(length(hi[[1]] != 0)){
    hist <- NULL
    for ( i in 1:length(hi[[1]])){
      hist[[i]] <- unlist(head[[hi[[1]][i]]])
    }
    histo <- unlist(hist)
    histor <- NULL
    for (i in 1:length(histo)){
      histor[[i]] <- paste(names(histo)[[i]],":", histo[[i]])
    }
    
    history <- unlist(histor)
    
    for (i in 1:length(history)){
      ncatt_put(ncout, 0, paste0("ODF_HISTORY_", i), history[[i]])
    }
    }
      ec <- list(grep(names(head$EVENT_HEADER), pattern = 'EVENT_COMMENTS'))
      if (length(ec[[1]] != 0)){
      evc <- NULL
      for( i in 1:length(ec[[1]])){
        evc[[i]] <- unlist(head$EVENT_HEADER[[ec[[1]][i]]])
      }
      evec <- unlist(evc)
      evenc <- NULL
      for (i in 1:length(evec)){
        evenc[[i]] <- paste(names(evec)[[i]], ":", evec[[i]])
      }
      eventc <- unlist(evenc)
    for( i in 1:length(eventc)){
      ncatt_put(ncout, 0, paste0("EVENT_COMMENTS_", i), eventc[[i]])
    }
      }
    
    }
  }
  
  
  
  
  ####nc close####
 
    nc_close(ncout)
 
  
}