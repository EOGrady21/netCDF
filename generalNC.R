####General NC template


NCcreate <- function(obj, filename, metadata){
 
  #if(missing(name)){
  #  name <- paste('MADCP', adp[['experiment']], adp[['station']], adp[['serial_number']], adp[['delta_t_sec']], sep = '_')
  #}
  #file name and path
  ncpath <- "./"
  ncfname <- paste(ncpath, filename, ".nc", sep = "")
  
  
  ####setting dimensions and definitions####
  
  #create dimensions
  timedim <- ncdim_def("time", "seconds since 1970-01-01T00:00:00Z", as.double(obj[['time']]))    #time formatting FIX
  distdim <- ncdim_def("distance", "metres", as.double(obj[['distance']]))
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
  
  dlname <- variable_6
  v6_def <- ncvar_def(var6, units6, list(timedim, stationdim), FillValue, dlname, prec = 'double')
  
  dlname <- variable_7
  v7_def <- ncvar_def(var7, units7, list(timedim, stationdim), FillValue, dlname, prec = 'double')
  
  dlname <- variable_8
  v8_def <- ncvar_def(var8, units8, list(timedim, stationdim), FillValue, dlname, prec = 'double')
  
  dlname <- variable_9
  v9_def <- ncvar_def(var9, units9, list(timedim, stationdim), FillValue, dlname, prec = 'double')
  
  dlname <- variable_10
  v10_def <- ncvar_def(var10, units10, list(timedim, stationdim), FillValue, dlname, prec = 'double')
  
  dlname <- variable_11
  v11_def <- ncvar_def(var11, units11, list(timedim, stationdim), FillValue, dlname, prec = 'double')
  
  dlname <- variable_12
  v12_def <- ncvar_def(var12, units12, list(timedim, stationdim), FillValue, dlname, prec = 'double')
  
  
  #####write out definitions to new nc file####
  ncout <- nc_create(ncfname, list( t_def, lon_def, lat_def, ts_def, v1_def, v2_def, v3_def, v4_def, v5_def, v6_def, v7_def, v8_def, v9_def, v10_def, v11_def, v12_def), force_v4 = TRUE)
  ncvar_put(ncout, ts_def, obj[['time']])
  ncvar_put(ncout, t_def, as.POSIXct(obj[['time']], tz = 'UTC', origin = '1970-01-01 00:00:00'))
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
  ncvar_put(ncout, v12_def, obj[[variable_12]])
  
  
  
  ####metadata####
  ####metadata####
  ncatt_put(ncout, 'station', 'longitude', obj[['longitude']])
  ncatt_put(ncout, 'station', 'latitiude', obj[['latitude']])
  ncatt_put(ncout, 'station', 'standard_name', 'platform_name')
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
  ncatt_put(ncout, var1, "sensor_depth", obj[['sensor_depth']])
  ncatt_put(ncout, var1, "serial_number", obj[['serial_number']])
  ncatt_put(ncout, var2, "sensor_type", obj[['model']])
  ncatt_put(ncout, var2, "sensor_depth", obj[['sensor_depth']])
  ncatt_put(ncout, var2, "serial_number", obj[['serial_number']])
  ncatt_put(ncout, var3, "sensor_type", obj[['model']])
  ncatt_put(ncout, var3, "sensor_depth", obj[['sensor_depth']])
  ncatt_put(ncout, var3, "serial_number", obj[['serial_number']])
  ncatt_put(ncout, var4, "sensor_type", obj[['model']])
  ncatt_put(ncout, var4, "sensor_depth", obj[['sensor_depth']])
  ncatt_put(ncout, var4, "serial_number", obj[['serial_number']])
  ncatt_put(ncout, var5, "sensor_type", obj[['model']])
  ncatt_put(ncout, var5, "sensor_depth", obj[['sensor_depth']])
  ncatt_put(ncout, var5, "serial_number", obj[['serial_number']])
  ncatt_put(ncout, var6, "sensor_type", obj[['model']])
  ncatt_put(ncout, var6, "sensor_depth", obj[['sensor_depth']])
  ncatt_put(ncout, var6, "serial_number", obj[['serial_number']])
  ncatt_put(ncout, var7, "sensor_type", obj[['model']])
  ncatt_put(ncout, var7, "sensor_depth", obj[['sensor_depth']])
  ncatt_put(ncout, var7, "serial_number", obj[['serial_number']])
  ncatt_put(ncout, var8, "sensor_type", obj[['model']])
  ncatt_put(ncout, var8, "sensor_depth", obj[['sensor_depth']])
  ncatt_put(ncout, var8, "serial_number", obj[['serial_number']])
  ncatt_put(ncout, var9, "sensor_type", obj[['model']])
  ncatt_put(ncout, var9, "sensor_depth", obj[['sensor_depth']])
  ncatt_put(ncout, var9, "serial_number", obj[['serial_number']])
  ncatt_put(ncout, var10, "sensor_type", obj[['model']])
  ncatt_put(ncout, var10, "sensor_depth", obj[['sensor_depth']])
  ncatt_put(ncout, var10, "serial_number", obj[['serial_number']])
  ncatt_put(ncout, var11, "sensor_type", obj[['model']])
  ncatt_put(ncout, var11, "sensor_depth", obj[['sensor_depth']])
  ncatt_put(ncout, var11, "serial_number", obj[['serial_number']])
  ncatt_put(ncout, var12, "sensor_type", obj[['model']])
  ncatt_put(ncout, var12, "sensor_depth", obj[['sensor_depth']])
  ncatt_put(ncout, var12 , "serial_number", obj[['serial_number']])
  
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
  ncatt_put(ncout, var12, "generic_name", variable_12)
  
  ####CF conventions & BODC standards####
  ncatt_put(ncout, 0, 'Conventions', 'CF-1.7')
  ncatt_put(ncout, 0, "creator_type", "person")
  ncatt_put(ncout, 0, "program", obj[['description']])
  ncatt_put(ncout, 0, "time_coverage_start", obj[['time_coverage_start']])
  ncatt_put(ncout, 0, "time_coverage_end", obj[['time_coverage_end']])
  ncatt_put(ncout, 0, "geospatial_lat_min", obj[['latitude']])
  ncatt_put(ncout, 0, "geospatial_lat_max", obj[['latitude']])
  ncatt_put(ncout, 0, "geospatial_lat_units", "degrees_north")
  ncatt_put(ncout, 0, "geospatial_lon_min", obj[['longitude']])
  ncatt_put(ncout, 0, "geospatial_lon_max", obj[['longitude']])
  ncatt_put(ncout, 0, "geospatial_lon_units", "degrees_east")
  ncatt_put(ncout, 0, "geospatial_vertical_max", obj[['sensor_depth']] + min(adp[['distance']], na.rm = TRUE))
  ncatt_put(ncout, 0, "geospatial_vertical_min", obj[['sensor_depth']] + max(adp[['distance']], na.rm = TRUE))
  ncatt_put(ncout, 0, "geospatial_vertical_units", "metres")
  ncatt_put(ncout, 0, "geospatial_vertical_positive", 'down')
  ncatt_put(ncout, 0, "project", obj[['project']])
  ncatt_put(ncout,0, "_FillValue", "1e35")
  ncatt_put(ncout, 0, "featureType", obj[['featureType']])
  ncatt_put(ncout, 0, "date_modified", date())
  ncatt_put(ncout, 0, "institution", adp[['institution']])
  
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
  ncatt_put(ncout, var12, "sdn_parameter_urn", P01_VAR12)
  ncatt_put(ncout, "ELTMEP01", "sdn_parameter_urn", "SDN:P01::ELTMEP01")
  ncatt_put(ncout, "lon", "sdn_parameter_urn", "SDN:P01::ALONZZ01")
  ncatt_put(ncout, "lat", "sdn_parameter_urn", "SDN:P01::ALATZZ01")
  ncatt_put(ncout, "time_string", "sdn_parameter_urn", "SDN:P01::DTUT8601")
  
  
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
  ncatt_put(ncout, var12, "sdn_parameter_name", P01_name_var12)
  
  ncatt_put(ncout, "lon", "sdn_parameter_name", "Longitude east")
  ncatt_put(ncout, "lat", "sdn_parameter_name", "Latitude north")
  ncatt_put(ncout, 'ELTMEP01', "sdn_parameter_name", "Elapsed time (since 1970-01-01T00:00:00Z)")
  ncatt_put(ncout, 'time_string', "sdn_parameter_name", "String corresponding to format 'YYYY-MM-DDThh:mm:ss.sssZ' or other valid ISO8601 string")
  
  
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
  ncatt_put(ncout, var12, "sdn_uom_urn", P06_var12)
  
  ncatt_put(ncout, "lon", "sdn_uom_urn", "SDN:P06::DEGE")
  ncatt_put(ncout, "lat", "sdn_uom_urn", "SDN:P06:DEGN")
  ncatt_put(ncout, "ELTMEP01", "sdn_uom_urn", "SDN:P06::UTBB")
  ncatt_put(ncout, "time_string", "sdn_uom_urn", "SDN:P06::TISO")
  
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
  ncatt_put(ncout, var12, "sdn_uom_name", P06_name_var12)

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
  ncatt_put(ncout, var6, "standard_name", std_variable_6)
  ncatt_put(ncout, var7, "standard_name", std_variable_7)
  ncatt_put(ncout, var8, "standard_name", std_variable_8)
  ncatt_put(ncout, var9, "standard_name", std_variable_9)
  ncatt_put(ncout, var10, "standard_name", std_variable_10)
  ncatt_put(ncout, var11, "standard_name", std_variable_11)
  ncatt_put(ncout, var12, "standard_name", std_variable_12)
  
  
  ####data max and min####
  ncatt_put(ncout, var1, "data_max", max(obj[[var1]], na.rm = TRUE))
  ncatt_put(ncout, var1, "data_min", min(obj[[var2]], na.rm = TRUE))
  ncatt_put(ncout, var1, "valid_max", var1max)
  ncatt_put(ncout, var1, "valid_min", var1min)
  
  ncatt_put(ncout, var2, "data_max", max(obj[[var2]], na.rm = TRUE))
  ncatt_put(ncout, var2, "data_min", min(obj[[var2]], na.rm = TRUE))
  ncatt_put(ncout, var2, "valid_max", var2max)
  ncatt_put(ncout, var2, "valid_min", var2min)
  
  ncatt_put(ncout, var3, "data_max", max(obj[[var3]], na.rm = TRUE))
  ncatt_put(ncout, var3, "data_min", min(obj[[var3]], na.rm = TRUE))
  ncatt_put(ncout, var3, "valid_max", var3max)
  ncatt_put(ncout, var3, "valid_min", var3min)
  
  ncatt_put(ncout, var4, "data_max", max(obj[[var4]], na.rm = TRUE))
  ncatt_put(ncout, var4, "data_min", min(obj[[var4]], na.rm = TRUE))
  ncatt_put(ncout, var4, "valid_max", var4max)
  ncatt_put(ncout, var4, "valid_min", var4min)
  
  ncatt_put(ncout, var5, "data_max", max(obj[[var5]], na.rm = TRUE))
  ncatt_put(ncout, var5, "data_min", min(obj[[var5]], na.rm = TRUE))
  ncatt_put(ncout, var5, "valid_max", var5max)
  ncatt_put(ncout, var5, "valid_min", var5min)
  
  ncatt_put(ncout, var6, "data_max", max(obj[[var6]], na.rm = TRUE))
  ncatt_put(ncout, var6, "data_min", min(obj[[var6]], na.rm = TRUE))
  ncatt_put(ncout, var6, "valid_max", var6max)
  ncatt_put(ncout, var6, "valid_min", var6min)
  
  ncatt_put(ncout, var7, "data_max", max(obj[[var7]], na.rm = TRUE))
  ncatt_put(ncout, var7, "data_min", min(obj[[var7]], na.rm = TRUE))
  ncatt_put(ncout, var7, "valid_max", var7max)
  ncatt_put(ncout, var7, "valid_min", var7min)
  
  ncatt_put(ncout, var8, "data_max", max(obj[[var8]], na.rm = TRUE))
  ncatt_put(ncout, var8, "data_min", min(obj[[var8]], na.rm = TRUE))
  ncatt_put(ncout, var8, "valid_max", var8max)
  ncatt_put(ncout, var8, "valid_min", var8min)
  
  ncatt_put(ncout, var9, "data_max", max(obj[[var9]], na.rm = TRUE))
  ncatt_put(ncout, var9, "data_min", min(obj[[var9]], na.rm = TRUE))
  ncatt_put(ncout, var9, "valid_max", var9max)
  ncatt_put(ncout, var9, "valid_min", var9min)
  
  ncatt_put(ncout, var10, "data_max", max(obj[[var10]], na.rm = TRUE))
  ncatt_put(ncout, var10, "data_min", min(obj[[var10]], na.rm = TRUE))
  ncatt_put(ncout, var10, "valid_max", var10max)
  ncatt_put(ncout, var10, "valid_min", var10min)
  
  ncatt_put(ncout, var11, "data_max", max(obj[[var11]], na.rm = TRUE))
  ncatt_put(ncout, var11, "data_min", min(obj[[var11]], na.rm = TRUE))
  ncatt_put(ncout, var11, "valid_max", var11max)
  ncatt_put(ncout, var11, "valid_min", var11min)
  
  ncatt_put(ncout, var12, "data_max", max(obj[[var12]], na.rm = TRUE))
  ncatt_put(ncout, var12, "data_min", min(obj[[var12]], na.rm = TRUE))
  ncatt_put(ncout, var12, "valid_max", var12max)
  ncatt_put(ncout, var12, "valid_min", var12min)
  
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
