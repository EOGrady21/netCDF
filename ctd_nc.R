####profile ctd netCDF template####



# obj <- read.odf('C:/Users/ChisholmE/Documents/sample files/ctd/CTD_BCD2017666_01_01_DN.ODF', header = 'list')
# upcast <- read.odf('C:/Users/ChisholmE/Documents/sample files/ctd/CTD_BCD2017666_01_01_UP.ODF', header = 'list')
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
  
  # #remove SYTM from var list
  # tr <- grep(v, pattern = 'time')
  # v <- v[-tr]
  # vt <- grep(var, pattern = 'SYTM')
  # var <- var[-vt]
  
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
  if (numvar >23){
    warning("Maximum of 23 variables exceeded! Not all data has been exported!")
  }
  tf <-  v %in% names(obj[['flags']])
  numflag <- length(tf[tf == TRUE])

  
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
  
 
  scandim <- ncdim_def("scan", "counts", as.double(obj[['scan']]))
  timedim <- ncdim_def("time", "seconds since 1970-01-01T00:00:00Z", as.double(obj[['time']]))
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
  
  
  if (numvar >1){
    dlname <- variable_2
    v2_def <- ncvar_def(var2, units2, list(timedim, stationdim), FillValue, dlname, prec = 'double')
    
    
    if (numvar >2){
      dlname <- variable_3
      v3_def <- ncvar_def(var3, units3, list(timedim, stationdim), FillValue, dlname, prec = 'double')
      
      
      if (numvar >3){
        dlname <- variable_4
        v4_def <- ncvar_def(var4, units4, list(timedim, stationdim), FillValue, dlname, prec = 'double')
        
        
        if (numvar >4){
          dlname <- variable_5
          v5_def <- ncvar_def(var5, units5, list(timedim, stationdim), FillValue, dlname, prec = 'double')
          
          
          if (numvar >5){
            dlname <- variable_6
            v6_def <- ncvar_def(var6, units6, list(timedim, stationdim), FillValue, dlname, prec = 'double')
            
            
            if (numvar >6){
              dlname <- variable_7
              v7_def <- ncvar_def(var7, units7, list(timedim, stationdim), FillValue, dlname, prec = 'double')
              
              
              if (numvar >7){
                dlname <- variable_8
                v8_def <- ncvar_def(var8, units8, list(timedim, stationdim), FillValue, dlname, prec = 'double')
                
                
                if (numvar > 8 ){
                  dlname <- variable_9
                  v9_def <- ncvar_def(var9, units9, list(timedim, stationdim), FillValue, dlname, prec = 'double')
                  
                  
                  if (numvar > 9 ){
                    dlname <- variable_10
                    v10_def <- ncvar_def(var10, units10, list(timedim, stationdim), FillValue, dlname, prec = 'double')
                    
                    
                    if (numvar >10){
                      dlname <- variable_11
                      v11_def <- ncvar_def(var11, units11, list(timedim, stationdim), FillValue, dlname, prec = 'double')
                      
                      if (numvar >11){
                        dlname <- variable_12
                        v12_def <- ncvar_def(var12, units12, list(timedim, stationdim), FillValue, dlname, prec = 'double')
                        
                        if (numvar >12){
                          dlname <- variable_13
                          v13_def <- ncvar_def(var13, units13, list(timedim, stationdim), FillValue, dlname, prec = 'double')
                          
                          
                          if (numvar >13){
                            dlname <- variable_14
                            v14_def <- ncvar_def(var14, units14, list(timedim, stationdim), FillValue, dlname, prec = 'double')
                            
                            
                            if (numvar >14){
                              dlname <- variable_15
                              v15_def <- ncvar_def(var15, units15, list(timedim, stationdim), FillValue, dlname, prec = 'double')
                              
                              
                              if (numvar >15){
                                dlname <- variable_16
                                v16_def <- ncvar_def(var16, units16, list(timedim, stationdim), FillValue, dlname, prec = 'double')
                                
                                
                                if (numvar >16){
                                  dlname <- variable_17
                                  v17_def <- ncvar_def(var17, units17, list(timedim, stationdim), FillValue, dlname, prec = 'double')
                                  
                                  
                                  if (numvar >17){
                                    dlname <- variable_18
                                    v18_def <- ncvar_def(var18, units18, list(timedim, stationdim), FillValue, dlname, prec = 'double')
                                    
                                    
                                    if (numvar >18){
                                      dlname <- variable_19
                                      v19_def <- ncvar_def(var19, units19, list(timedim, stationdim), FillValue, dlname, prec = 'double')
                                      
                                      
                                      if (numvar > 19 ){
                                        dlname <- variable_20
                                        v20_def <- ncvar_def(var20, units20, list(timedim, stationdim), FillValue, dlname, prec = 'double')
                                        
                                        
                                        if (numvar > 20 ){
                                          dlname <- variable_21
                                          v21_def <- ncvar_def(var21, units21, list(timedim, stationdim), FillValue, dlname, prec = 'double')
                                          
                                          
                                          if (numvar >21){
                                            dlname <- variable_22
                                            v22_def <- ncvar_def(var22, units22, list(timedim, stationdim), FillValue, dlname, prec = 'double')
                                            
                                            if (numvar >22){
                                              dlname <- variable_23
                                              v23_def <- ncvar_def(var23, units23, list(timedim, stationdim), FillValue, dlname, prec = 'double')
                                              
                                              
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
  
  if (numflag > 0){
  dlname <- variable1_QC
  v1qc_def <- ncvar_def(var1_QC, units = '', list(timedim, stationdim), missval = 0, dlname, prec = 'integer')
  
  if (numflag > 1){
    dlname <- variable2_QC
    v2qc_def <- ncvar_def(var2_QC, units = '', list(timedim, stationdim), missval = 0, dlname, prec = 'integer')
    
    if (numflag >2){
      dlname <- variable3_QC
      v3qc_def <- ncvar_def(var3_QC, units = '', list(timedim, stationdim), missval = 0, dlname, prec = 'integer')
      
      if (numflag >3){
        dlname <- variable4_QC
        v4qc_def <- ncvar_def(var4_QC, units = '', list(timedim, stationdim), missval = 0, dlname, prec = 'integer')
        
        if (numflag >4){
          dlname <- variable5_QC
          v5qc_def <- ncvar_def(var5_QC, units = '', list(timedim, stationdim), missval = 0, dlname, prec = 'integer')
          
          if (numflag >5){
            dlname <- variable6_QC
            v6qc_def <- ncvar_def(var6_QC, units = '', list(timedim, stationdim), missval = 0, dlname, prec = 'integer')
            
            if (numflag >6){
              dlname <- variable7_QC
              v7qc_def <- ncvar_def(var7_QC, units = '', list(timedim, stationdim), missval = 0, dlname, prec = 'integer')
              
              if (numflag >7){
                dlname <- variable8_QC
                v8qc_def <- ncvar_def(var8_QC, units = '', list(timedim, stationdim), missval = 0, dlname, prec = 'integer')
                
                if (numflag > 8){
                  dlname <- variable9_QC
                  v9qc_def <- ncvar_def(var9_QC, units = '', list(timedim, stationdim), missval = 0, dlname, prec = 'integer')
                  
                  if (numflag >9){
                    dlname <- variable10_QC
                    v10qc_def <- ncvar_def(var10_QC, units = '', list(timedim, stationdim), missval = 0, dlname, prec = 'integer')
                    
                    if (numflag >10){
                      dlname <- variable11_QC
                      v11qc_def <- ncvar_def(var11_QC, units = '', list(timedim, stationdim), missval = 0, dlname, prec = 'integer')
                      
                      if (numflag >11){
                        dlname <- variable12_QC
                        v12qc_def <- ncvar_def(var12_QC, units = '', list(timedim, stationdim), missval = 0, dlname, prec = 'integer')
                        
                        
                        if (numflag > 12){
                          dlname <- variable13_QC
                          v13qc_def <- ncvar_def(var13_QC, units = '', list(timedim, stationdim), missval = 0, dlname, prec = 'integer')
                          
                          if (numflag >13){
                            dlname <- variable14_QC
                            v14qc_def <- ncvar_def(var14_QC, units = '', list(timedim, stationdim), missval = 0, dlname, prec = 'integer')
                            
                            if (numflag >14){
                              dlname <- variable15_QC
                              v15qc_def <- ncvar_def(var15_QC, units = '', list(timedim, stationdim), missval = 0, dlname, prec = 'integer')
                              
                              if (numflag >15){
                                dlname <- variable16_QC
                                v16qc_def <- ncvar_def(var16_QC, units = '', list(timedim, stationdim), missval = 0, dlname, prec = 'integer')
                                
                                if (numflag >16){
                                  dlname <- variable17_QC
                                  v17qc_def <- ncvar_def(var17_QC, units = '', list(timedim, stationdim), missval = 0, dlname, prec = 'integer')
                                  
                                  if (numflag >17){
                                    dlname <- variable18_QC
                                    v18qc_def <- ncvar_def(var18_QC, units = '', list(timedim, stationdim), missval = 0, dlname, prec = 'integer')
                                    
                                    if (numflag >18){
                                      dlname <- variable19_QC
                                      v19qc_def <- ncvar_def(var19_QC, units = '', list(timedim, stationdim), missval = 0, dlname, prec = 'integer')
                                      
                                      if (numflag > 19){
                                        dlname <- variable20_QC
                                        v20qc_def <- ncvar_def(var20_QC, units = '', list(timedim, stationdim), missval = 0, dlname, prec = 'integer')
                                        
                                        if (numflag >20){
                                          dlname <- variable21_QC
                                          v21qc_def <- ncvar_def(var21_QC, units = '', list(timedim, stationdim), missval = 0, dlname, prec = 'integer')
                                          
                                          if (numflag >21){
                                            dlname <- variable22_QC
                                            v22qc_def <- ncvar_def(var22_QC, units = '', list(timedim, stationdim), missval = 0, dlname, prec = 'integer')
                                            
                                            if (numflag >22){
                                              dlname <- variable23_QC
                                              v23qc_def <- ncvar_def(var23_QC, units = '', list(timedim, stationdim), missval = 0, dlname, prec = 'integer')
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
  ncvar_put(ncout, ts_def, obj[['time']])
  ncvar_put(ncout, t_def, as.POSIXct(obj[['time']], tz = 'UTC', origin = '1970-01-01 00:00:00'))
  
  ncvar_put(ncout, v1_def, obj@data[[variable_1]])
  if (numvar >1){
    ncvar_put(ncout, v2_def, obj@data[[variable_2]])
    if (numvar >2){
      ncvar_put(ncout, v3_def, obj@data[[variable_3]])
      if (numvar >3){
        ncvar_put(ncout, v4_def, obj@data[[variable_4]])
        if (numvar >4){
          ncvar_put(ncout, v5_def, obj@data[[variable_5]])
          if (numvar >5){
            ncvar_put(ncout, v6_def, obj@data[[variable_6]])
            if (numvar >6){
              ncvar_put(ncout, v7_def, obj@data[[variable_7]])
              if (numvar >7){
                ncvar_put(ncout, v8_def, obj@data[[variable_8]])
                if (numvar >8){
                  ncvar_put(ncout, v9_def, obj@data[[variable_9]])
                  if (numvar >9){
                    ncvar_put(ncout, v10_def, obj@data[[variable_10]])
                    if (numvar >10){
                      ncvar_put(ncout, v11_def, obj@data[[variable_11]])
                      if(numvar >11){
                        ncvar_put(ncout, v12_def, obj@data[[variable_12]])
                        if (numvar >12){
                          ncvar_put(ncout, v13_def, obj@data[[variable_13]])
                          if (numvar >13){
                            ncvar_put(ncout, v14_def, obj@data[[variable_14]])
                            if (numvar >14){
                              ncvar_put(ncout, v15_def, obj@data[[variable_15]])
                              if (numvar >15){
                                ncvar_put(ncout, v16_def, obj@data[[variable_16]])
                                if (numvar >16){
                                  ncvar_put(ncout, v17_def, obj@data[[variable_17]])
                                  if (numvar >17){
                                    ncvar_put(ncout, v18_def, obj@data[[variable_18]])
                                    if (numvar >18){
                                      ncvar_put(ncout, v19_def, obj@data[[variable_19]])
                                      if (numvar >19){
                                        ncvar_put(ncout, v20_def, obj@data[[variable_20]])
                                        if (numvar >20){
                                          ncvar_put(ncout, v21_def, obj@data[[variable_21]])
                                          if (numvar >21){
                                            ncvar_put(ncout, v22_def, obj@data[[variable_22]])
                                            if(numvar >22){
                                              ncvar_put(ncout, v23_def, obj@data[[variable_23]])
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
  
  
  if (numflag >0){
  ncvar_put(ncout, v1qc_def, obj@metadata$flags[[variable_1]] )
  if (numflag >1){
    ncvar_put(ncout, v2qc_def, obj@metadata$flags[[variable_2]] )
    if(numflag >2){
      ncvar_put(ncout, v3qc_def, obj@metadata$flags[[variable_3]] )
      if (numflag >3){
        ncvar_put(ncout, v4qc_def, obj@metadata$flags[[variable_4]] )
        if (numflag >4){
          ncvar_put(ncout, v5qc_def, obj@metadata$flags[[variable_5]] )
          if (numflag >5){
            ncvar_put(ncout, v6qc_def, obj@metadata$flags[[variable_6]] )
            if (numflag >6){
              ncvar_put(ncout, v7qc_def, obj@metadata$flags[[variable_7]] )
              if (numflag >7){
                ncvar_put(ncout, v8qc_def, obj@metadata$flags[[variable_8]] )
                if (numflag >8){
                  ncvar_put(ncout, v9qc_def, obj@metadata$flags[[variable_9]] )
                  if (numflag >9){
                    ncvar_put(ncout, v10qc_def, obj@metadata$flags[[variable_10]] )
                    if (numflag >10){
                      ncvar_put(ncout, v11qc_def, obj@metadata$flags[[variable_11]] )
                      if (numflag >11){
                        ncvar_put(ncout, v12qc_def, obj@metadata$flags[[variable_12]] )
                        if (numflag >12){
                          ncvar_put(ncout, v13qc_def, obj@metadata$flags[[variable_13]] )
                          if(numflag >13){
                            ncvar_put(ncout, v14qc_def, obj@metadata$flags[[variable_14]] )
                            if (numflag >14){
                              ncvar_put(ncout, v15qc_def, obj@metadata$flags[[variable_15]] )
                              if (numflag >15){
                                ncvar_put(ncout, v16qc_def, obj@metadata$flags[[variable_16]] )
                                if (numflag >16){
                                  ncvar_put(ncout, v17qc_def, obj@metadata$flags[[variable_17]] )
                                  if (numflag >17){
                                    ncvar_put(ncout, v18qc_def, obj@metadata$flags[[variable_18]] )
                                    if (numflag >18){
                                      ncvar_put(ncout, v19qc_def, obj@metadata$flags[[variable_19]] )
                                      if (numflag >19){
                                        ncvar_put(ncout, v20qc_def, obj@metadata$flags[[variable_20]] )
                                        if (numflag >20){
                                          ncvar_put(ncout, v21qc_def, obj@metadata$flags[[variable_21]] )
                                          if (numflag >21){
                                            ncvar_put(ncout, v22qc_def, obj@metadata$flags[[variable_22]] )
                                            if (numflag >22){
                                              ncvar_put(ncout, v23qc_def, obj@metadata$flags[[variable_23]] )
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
  }
  
  
  
  ####metadata####
  ####metadata####
  ncatt_put(ncout, 'station', 'longitude', obj[['longitude']])
  ncatt_put(ncout, 'station', 'latitiude', obj[['latitude']])
  ncatt_put(ncout, 'station', 'standard_name', 'platform_name')
  ncatt_put(ncout, 'station', 'cf_role', 'profile_id')
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
  if (numvar >0 & numflag >0){
    ncatt_put(ncout, var1, 'ancillary_variables', var1_QC)
    if (numvar >1 & numflag >1){
      ncatt_put(ncout, var2, 'ancillary_variables', var2_QC)
      if (numvar >2 & numflag >2){
        ncatt_put(ncout, var3, 'ancillary_variables', var3_QC)
        if (numvar >3 & numflag >3){
          ncatt_put(ncout, var4, 'ancillary_variables', var4_QC)
          if (numvar >4 & numflag >4){
            ncatt_put(ncout, var5, 'ancillary_variables', var5_QC)
            if (numvar >5 & numflag >5){
              ncatt_put(ncout, var6, 'ancillary_variables', var6_QC)
              if (numvar >6 & numflag >6){
                ncatt_put(ncout, var7, 'ancillary_variables', var7_QC)
                if (numvar >7 & numflag >7){
                  ncatt_put(ncout, var8, 'ancillary_variables', var8_QC)
                  if (numvar >8 & numflag >8){
                    ncatt_put(ncout, var9, 'ancillary_variables', var9_QC)
                    if (numvar >9 & numflag >9){
                      ncatt_put(ncout, var10, 'ancillary_variables', var10_QC)
                      if (numvar >10 & numflag >10){
                        ncatt_put(ncout, var11, 'ancillary_variables', var11_QC)
                        if (numvar >11 & numflag >11){
                          ncatt_put(ncout, var12, 'ancillary_variables', var12_QC)
                          if (numvar >12 & numflag >12){
                            ncatt_put(ncout, var13, 'ancillary_variables', var13_QC)
                            if (numvar >13 & numflag >13){
                              ncatt_put(ncout, var14, 'ancillary_variables', var14_QC)
                              if (numvar >14 & numflag >14){
                                ncatt_put(ncout, var15, 'ancillary_variables', var15_QC)
                                if (numvar >15 & numflag >15){
                                  ncatt_put(ncout, var16, 'ancillary_variables', var16_QC)
                                  if (numvar >16 & numflag >16){
                                    ncatt_put(ncout, var17, 'ancillary_variables', var17_QC)
                                    if (numvar >17 & numflag >17){
                                      ncatt_put(ncout, var18, 'ancillary_variables', var18_QC)
                                      if (numvar >18 & numflag >18){
                                        ncatt_put(ncout, var19, 'ancillary_variables', var19_QC)
                                        if (numvar >19 & numflag >19){
                                          ncatt_put(ncout, var20, 'ancillary_variables', var20_QC)
                                          if (numvar >20 & numflag >20){
                                            ncatt_put(ncout, var21, 'ancillary_variables', var21_QC)
                                            if (numvar >21 & numflag >21){
                                              ncatt_put(ncout, var22, 'ancillary_variables', var22_QC)
                                              if (numvar >22 & numflag >22){
                                                ncatt_put(ncout, var23, 'ancillary_variables', var23_QC)
                                                if (numvar >23 & numflag >23){
                                                  ncatt_put(ncout, var24, 'ancillary_variables', var24_QC)
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
    }
  }
  
  
  ##reference scales
  
  
  
  #sensor type, sensor depth and serial number for each variable
  ncatt_put(ncout, var1, "sensor_type", obj[['model']])
  ncatt_put(ncout, var1, "sensor_depth", obj[['depthMin']])
  ncatt_put(ncout, var1, "serial_number", obj[['serialNumber']])
  ncatt_put(ncout, var1, "generic_name", variable_1)
  ncatt_put(ncout, var1, "sdn_parameter_urn", P01_VAR1)
  ncatt_put(ncout, var1, "sdn_parameter_name", P01_name_var1)
  ncatt_put(ncout, var1, "sdn_uom_urn", P06_var1)
  ncatt_put(ncout, var1, "sdn_uom_name", P06_name_var1)
  if (!is.null(std_variable_1)){
    ncatt_put(ncout, var1, "standard_name", std_variable_1)
  }
  ncatt_put(ncout, var1, "data_max", max(obj[[variable_1]], na.rm = TRUE))
  ncatt_put(ncout, var1, "data_min", min(obj[[variable_1]], na.rm = TRUE))
  ncatt_put(ncout, var1, "valid_max", var1max)
  ncatt_put(ncout, var1, "valid_min", var1min)
  
  
  if (numvar > 1){
    ncatt_put(ncout, var2, "sensor_type", obj[['model']])
    ncatt_put(ncout, var2, "sensor_depth", obj[['depthMin']])
    ncatt_put(ncout, var2, "serial_number", obj[['serialNumber']])
    ncatt_put(ncout, var2, "generic_name", variable_2)
    ncatt_put(ncout, var2, "sdn_parameter_urn", P01_VAR2)
    ncatt_put(ncout, var2, "sdn_parameter_name", P01_name_var2)
    ncatt_put(ncout, var2, "sdn_uom_urn", P06_var2)
    ncatt_put(ncout, var2, "sdn_uom_name", P06_name_var2)
    if (!is.null(std_variable_2)){
      ncatt_put(ncout, var2, "standard_name", std_variable_2)
    }
    ncatt_put(ncout, var2, "data_max", max(obj[[variable_2]], na.rm = TRUE))
    ncatt_put(ncout, var2, "data_min", min(obj[[variable_2]], na.rm = TRUE))
    ncatt_put(ncout, var2, "valid_max", var2max)
    ncatt_put(ncout, var2, "valid_min", var2min)
    
    
    if (numvar >2){
      ncatt_put(ncout, var3, "sensor_type", obj[['model']])
      ncatt_put(ncout, var3, "sensor_depth", obj[['depthMin']])
      ncatt_put(ncout, var3, "serial_number", obj[['serialNumber']])
      ncatt_put(ncout, var3, "generic_name", variable_3)
      ncatt_put(ncout, var3, "sdn_parameter_urn", P01_VAR3)
      ncatt_put(ncout, var3, "sdn_parameter_name", P01_name_var3)
      ncatt_put(ncout, var3, "sdn_uom_urn", P06_var3)
      ncatt_put(ncout, var3, "sdn_uom_name", P06_name_var3)
      if (!is.null(std_variable_3)){
        ncatt_put(ncout, var3, "standard_name", std_variable_3)
      }
      ncatt_put(ncout, var3, "data_max", max(obj[[variable_3]], na.rm = TRUE))
      ncatt_put(ncout, var3, "data_min", min(obj[[variable_3]], na.rm = TRUE))
      ncatt_put(ncout, var3, "valid_max", var3max)
      ncatt_put(ncout, var3, "valid_min", var3min)
      
      
      if (numvar >3){
        ncatt_put(ncout, var4, "sensor_type", obj[['model']])
        ncatt_put(ncout, var4, "sensor_depth", obj[['depthMin']])
        ncatt_put(ncout, var4, "serial_number", obj[['serialNumber']])
        ncatt_put(ncout, var4, "generic_name", variable_4)   
        ncatt_put(ncout, var4, "sdn_parameter_urn", P01_VAR4)
        ncatt_put(ncout, var4, "sdn_parameter_name", P01_name_var4)
        ncatt_put(ncout, var4, "sdn_uom_urn", P06_var4)
        ncatt_put(ncout, var4, "sdn_uom_name", P06_name_var4)
        if (!is.null(std_variable_4)){
          ncatt_put(ncout, var4, "standard_name", std_variable_4)
        }
        ncatt_put(ncout, var4, "data_max", max(obj[[variable_4]], na.rm = TRUE))
        ncatt_put(ncout, var4, "data_min", min(obj[[variable_4]], na.rm = TRUE))
        ncatt_put(ncout, var4, "valid_max", var4max)
        ncatt_put(ncout, var4, "valid_min", var4min)
        
        
        if (numvar >4){
          ncatt_put(ncout, var5, "sensor_type", obj[['model']])
          ncatt_put(ncout, var5, "sensor_depth", obj[['depthMin']])
          ncatt_put(ncout, var5, "serial_number", obj[['serialNumber']])
          ncatt_put(ncout, var5, "generic_name", variable_5)
          ncatt_put(ncout, var5, "sdn_parameter_urn", P01_VAR5)
          ncatt_put(ncout, var5, "sdn_parameter_name", P01_name_var5)
          ncatt_put(ncout, var5, "sdn_uom_urn", P06_var5)
          ncatt_put(ncout, var5, "sdn_uom_name", P06_name_var5)
          if (!is.null(std_variable_5)){
            ncatt_put(ncout, var5, "standard_name", std_variable_5)
          }
          ncatt_put(ncout, var5, "data_max", max(obj[[variable_5]], na.rm = TRUE))
          ncatt_put(ncout, var5, "data_min", min(obj[[variable_5]], na.rm = TRUE))
          ncatt_put(ncout, var5, "valid_max", var5max)
          ncatt_put(ncout, var5, "valid_min", var5min)
          
          
          if (numvar >5){
            ncatt_put(ncout, var6, "sensor_type", obj[['model']])
            ncatt_put(ncout, var6, "sensor_depth", obj[['depthMin']])
            ncatt_put(ncout, var6, "serial_number", obj[['serialNumber']])
            ncatt_put(ncout, var6, "generic_name", variable_6)
            ncatt_put(ncout, var6, "sdn_parameter_urn", P01_VAR6)
            ncatt_put(ncout, var6, "sdn_parameter_name", P01_name_var6)
            ncatt_put(ncout, var6, "sdn_uom_urn", P06_var6)
            ncatt_put(ncout, var6, "sdn_uom_name", P06_name_var6)
            if (!is.null(std_variable_6)){
              ncatt_put(ncout, var6, "standard_name", std_variable_6)
            }
            ncatt_put(ncout, var6, "data_max", max(obj[[variable_6]], na.rm = TRUE))
            ncatt_put(ncout, var6, "data_min", min(obj[[variable_6]], na.rm = TRUE))
            ncatt_put(ncout, var6, "valid_max", var6max)
            ncatt_put(ncout, var6, "valid_min", var6min)
            
            
            if (numvar > 6){
              ncatt_put(ncout, var7, "sensor_type", obj[['model']])
              ncatt_put(ncout, var7, "sensor_depth", obj[['depthMin']])
              ncatt_put(ncout, var7, "serial_number", obj[['serialNumber']])
              ncatt_put(ncout, var7, "generic_name", variable_7)
              ncatt_put(ncout, var7, "sdn_parameter_urn", P01_VAR7)
              ncatt_put(ncout, var7, "sdn_parameter_name", P01_name_var7)
              ncatt_put(ncout, var7, "sdn_uom_urn", P06_var7)
              ncatt_put(ncout, var7, "sdn_uom_name", P06_name_var7)
              if (!is.null(std_variable_7)){
                ncatt_put(ncout, var7, "standard_name", std_variable_7)
              }
              ncatt_put(ncout, var7, "data_max", max(obj[[variable_7]], na.rm = TRUE))
              ncatt_put(ncout, var7, "data_min", min(obj[[variable_7]], na.rm = TRUE))
              ncatt_put(ncout, var7, "valid_max", var7max)
              ncatt_put(ncout, var7, "valid_min", var7min)
              
              
              if (numvar > 7){
                ncatt_put(ncout, var8, "sensor_type", obj[['model']])
                ncatt_put(ncout, var8, "sensor_depth", obj[['depthMin']])
                ncatt_put(ncout, var8, "serial_number", obj[['serialNumber']])
                ncatt_put(ncout, var8, "generic_name", variable_8)
                ncatt_put(ncout, var8, "sdn_parameter_urn", P01_VAR8)
                ncatt_put(ncout, var8, "sdn_parameter_name", P01_name_var8)
                ncatt_put(ncout, var8, "sdn_uom_urn", P06_var8)
                ncatt_put(ncout, var8, "sdn_uom_name", P06_name_var8)
                if (!is.null(std_variable_8)){
                  ncatt_put(ncout, var8, "standard_name", std_variable_8)
                }
                ncatt_put(ncout, var8, "data_max", max(obj[[variable_8]], na.rm = TRUE))
                ncatt_put(ncout, var8, "data_min", min(obj[[variable_8]], na.rm = TRUE))
                ncatt_put(ncout, var8, "valid_max", var8max)
                ncatt_put(ncout, var8, "valid_min", var8min)
                
                
                if (numvar > 8){
                  ncatt_put(ncout, var9, "sensor_type", obj[['model']])
                  ncatt_put(ncout, var9, "sensor_depth", obj[['depthMin']])
                  ncatt_put(ncout, var9, "serial_number", obj[['serialNumber']])
                  ncatt_put(ncout, var9, "generic_name", variable_9)
                  ncatt_put(ncout, var9, "sdn_parameter_urn", P01_VAR9)
                  ncatt_put(ncout, var9, "sdn_parameter_name", P01_name_var9)
                  ncatt_put(ncout, var9 , "sdn_uom_urn", P06_var9)
                  ncatt_put(ncout, var9, "sdn_uom_name", P06_name_var9)
                  if (!is.null(std_variable_9)){
                    ncatt_put(ncout, var9, "standard_name", std_variable_9)
                  }
                  ncatt_put(ncout, var9, "data_max", max(obj[[variable_9]], na.rm = TRUE))
                  ncatt_put(ncout, var9, "data_min", min(obj[[variable_9]], na.rm = TRUE))
                  ncatt_put(ncout, var9, "valid_max", var9max)
                  ncatt_put(ncout, var9, "valid_min", var9min)
                  
                  
                  if (numvar >9){
                    ncatt_put(ncout, var10, "sensor_type", obj[['model']])
                    ncatt_put(ncout, var10, "sensor_depth", obj[['depthMin']])
                    ncatt_put(ncout, var10, "serial_number", obj[['serialNumber']])
                    ncatt_put(ncout, var10, "generic_name", variable_10)
                    ncatt_put(ncout, var10, "sdn_parameter_urn", P01_VAR10)
                    ncatt_put(ncout, var10, "sdn_parameter_name", P01_name_var10)
                    ncatt_put(ncout, var10, "sdn_uom_urn", P06_var10)
                    ncatt_put(ncout, var10, "sdn_uom_name", P06_name_var10)
                    if (!is.null(std_variable_10)){
                      ncatt_put(ncout, var10, "standard_name", std_variable_10)
                    }
                    ncatt_put(ncout, var10, "data_max", max(obj[[variable_10]], na.rm = TRUE))
                    ncatt_put(ncout, var10, "data_min", min(obj[[variable_10]], na.rm = TRUE))
                    ncatt_put(ncout, var10, "valid_max", var10max)
                    ncatt_put(ncout, var10, "valid_min", var10min)
                    
                    
                    if (numvar >10){
                      ncatt_put(ncout, var11, "sensor_type", obj[['model']])
                      ncatt_put(ncout, var11, "sensor_depth", obj[['depthMin']])
                      ncatt_put(ncout, var11, "serial_number", obj[['serialNumber']])
                      ncatt_put(ncout, var11, "generic_name", variable_11)
                      ncatt_put(ncout, var11, "sdn_parameter_urn", P01_VAR11)
                      ncatt_put(ncout, var11, "sdn_parameter_name", P01_name_var11)
                      ncatt_put(ncout, var11, "sdn_uom_urn", P06_var11)
                      ncatt_put(ncout, var11, "sdn_uom_name", P06_name_var11)
                      if (!is.null(std_variable_11)){
                        ncatt_put(ncout, var11, "standard_name", std_variable_11)
                      }
                      ncatt_put(ncout, var11, "data_max", max(obj[[variable_11]], na.rm = TRUE))
                      ncatt_put(ncout, var11, "data_min", min(obj[[variable_11]], na.rm = TRUE))
                      ncatt_put(ncout, var11, "valid_max", var11max)
                      ncatt_put(ncout, var11, "valid_min", var11min)
                      
                      
                      if (numvar >11){
                        ncatt_put(ncout, var12, "sensor_type", obj[['model']])
                        ncatt_put(ncout, var12, "sensor_depth", obj[['depthMin']])
                        ncatt_put(ncout, var12 , "serial_number", obj[['serialNumber']])
                        ncatt_put(ncout, var12, "generic_name", variable_12)
                        ncatt_put(ncout, var12, "sdn_parameter_urn", P01_VAR12)
                        ncatt_put(ncout, var12, "sdn_parameter_name", P01_name_var12)
                        ncatt_put(ncout, var12, "sdn_uom_urn", P06_var12)
                        ncatt_put(ncout, var12, "sdn_uom_name", P06_name_var12)
                        if (!is.null(std_variable_12)){
                          ncatt_put(ncout, var12, "standard_name", std_variable_12)
                        }
                        ncatt_put(ncout, var12, "data_max", max(obj[[variable_12]], na.rm = TRUE))
                        ncatt_put(ncout, var12, "data_min", min(obj[[variable_12]], na.rm = TRUE))
                        ncatt_put(ncout, var12, "valid_max", var12max)
                        ncatt_put(ncout, var12, "valid_min", var12min)
                        
                        if (numvar > 12){
                          ncatt_put(ncout, var13, "sensor_type", obj[['model']])
                          ncatt_put(ncout, var13, "sensor_depth", obj[['depthMin']])
                          ncatt_put(ncout, var13, "serial_number", obj[['serialNumber']])
                          ncatt_put(ncout, var13, "generic_name", variable_13)
                          ncatt_put(ncout, var13, "sdn_parameter_urn", P01_VAR13)
                          ncatt_put(ncout, var13, "sdn_parameter_name", P01_name_var13)
                          ncatt_put(ncout, var13, "sdn_uom_urn", P06_var13)
                          ncatt_put(ncout, var13, "sdn_uom_name", P06_name_var13)
                          if (!is.null(std_variable_13)){
                            ncatt_put(ncout, var13, "standard_name", std_variable_13)
                          }
                          ncatt_put(ncout, var13, "data_max", max(obj[[variable_13]], na.rm = TRUE))
                          ncatt_put(ncout, var13, "data_min", min(obj[[variable_13]], na.rm = TRUE))
                          ncatt_put(ncout, var13, "valid_max", var13max)
                          ncatt_put(ncout, var13, "valid_min", var13min)
                          
                          
                          if (numvar >13){
                            ncatt_put(ncout, var14, "sensor_type", obj[['model']])
                            ncatt_put(ncout, var14, "sensor_depth", obj[['depthMin']])
                            ncatt_put(ncout, var14, "serial_number", obj[['serialNumber']])
                            ncatt_put(ncout, var14, "generic_name", variable_14)
                            ncatt_put(ncout, var14, "sdn_parameter_urn", P01_VAR14)
                            ncatt_put(ncout, var14, "sdn_parameter_name", P01_name_var14)
                            ncatt_put(ncout, var14, "sdn_uom_urn", P06_var14)
                            ncatt_put(ncout, var14, "sdn_uom_name", P06_name_var14)
                            if (!is.null(std_variable_14)){
                              ncatt_put(ncout, var14, "standard_name", std_variable_14)
                            }
                            ncatt_put(ncout, var14, "data_max", max(obj[[variable_14]], na.rm = TRUE))
                            ncatt_put(ncout, var14, "data_min", min(obj[[variable_14]], na.rm = TRUE))
                            ncatt_put(ncout, var14, "valid_max", var14max)
                            ncatt_put(ncout, var14, "valid_min", var14min)
                            
                            
                            if (numvar >14){
                              ncatt_put(ncout, var15, "sensor_type", obj[['model']])
                              ncatt_put(ncout, var15, "sensor_depth", obj[['depthMin']])
                              ncatt_put(ncout, var15, "serial_number", obj[['serialNumber']])
                              ncatt_put(ncout, var15, "generic_name", variable_15)   
                              ncatt_put(ncout, var15, "sdn_parameter_urn", P01_VAR15)
                              ncatt_put(ncout, var15, "sdn_parameter_name", P01_name_var15)
                              ncatt_put(ncout, var15, "sdn_uom_urn", P06_var15)
                              ncatt_put(ncout, var15, "sdn_uom_name", P06_name_var15)
                              if (!is.null(std_variable_15)){
                                ncatt_put(ncout, var15, "standard_name", std_variable_15)
                              }
                              ncatt_put(ncout, var15, "data_max", max(obj[[variable_15]], na.rm = TRUE))
                              ncatt_put(ncout, var15, "data_min", min(obj[[variable_15]], na.rm = TRUE))
                              ncatt_put(ncout, var15, "valid_max", var15max)
                              ncatt_put(ncout, var15, "valid_min", var15min)
                              
                              
                              if (numvar >15){
                                ncatt_put(ncout, var16, "sensor_type", obj[['model']])
                                ncatt_put(ncout, var16, "sensor_depth", obj[['depthMin']])
                                ncatt_put(ncout, var16, "serial_number", obj[['serialNumber']])
                                ncatt_put(ncout, var16, "generic_name", variable_16)
                                ncatt_put(ncout, var16, "sdn_parameter_urn", P01_VAR16)
                                ncatt_put(ncout, var16, "sdn_parameter_name", P01_name_var16)
                                ncatt_put(ncout, var16, "sdn_uom_urn", P06_var16)
                                ncatt_put(ncout, var16, "sdn_uom_name", P06_name_var16)
                                if (!is.null(std_variable_16)){
                                  ncatt_put(ncout, var16, "standard_name", std_variable_16)
                                }
                                ncatt_put(ncout, var16, "data_max", max(obj[[variable_16]], na.rm = TRUE))
                                ncatt_put(ncout, var16, "data_min", min(obj[[variable_16]], na.rm = TRUE))
                                ncatt_put(ncout, var16, "valid_max", var16max)
                                ncatt_put(ncout, var16, "valid_min", var16min)
                                
                                
                                if (numvar >16){
                                  ncatt_put(ncout, var17, "sensor_type", obj[['model']])
                                  ncatt_put(ncout, var17, "sensor_depth", obj[['depthMin']])
                                  ncatt_put(ncout, var17, "serial_number", obj[['serialNumber']])
                                  ncatt_put(ncout, var17, "generic_name", variable_17)
                                  ncatt_put(ncout, var17, "sdn_parameter_urn", P01_VAR17)
                                  ncatt_put(ncout, var17, "sdn_parameter_name", P01_name_var17)
                                  ncatt_put(ncout, var17, "sdn_uom_urn", P06_var17)
                                  ncatt_put(ncout, var17, "sdn_uom_name", P06_name_var17)
                                  if (!is.null(std_variable_17)){
                                    ncatt_put(ncout, var17, "standard_name", std_variable_17)
                                  }
                                  ncatt_put(ncout, var17, "data_max", max(obj[[variable_17]], na.rm = TRUE))
                                  ncatt_put(ncout, var17, "data_min", min(obj[[variable_17]], na.rm = TRUE))
                                  ncatt_put(ncout, var17, "valid_max", var17max)
                                  ncatt_put(ncout, var17, "valid_min", var17min)
                                  
                                  
                                  if (numvar > 17){
                                    ncatt_put(ncout, var18, "sensor_type", obj[['model']])
                                    ncatt_put(ncout, var18, "sensor_depth", obj[['depthMin']])
                                    ncatt_put(ncout, var18, "serial_number", obj[['serialNumber']])
                                    ncatt_put(ncout, var18, "generic_name", variable_18)
                                    ncatt_put(ncout, var18, "sdn_parameter_urn", P01_VAR18)
                                    ncatt_put(ncout, var18, "sdn_parameter_name", P01_name_var18)
                                    ncatt_put(ncout, var18, "sdn_uom_urn", P06_var18)
                                    ncatt_put(ncout, var18, "sdn_uom_name", P06_name_var18)
                                    if (!is.null(std_variable_18)){
                                      ncatt_put(ncout, var18, "standard_name", std_variable_18)
                                    }
                                    ncatt_put(ncout, var18, "data_max", max(obj[[variable_18]], na.rm = TRUE))
                                    ncatt_put(ncout, var18, "data_min", min(obj[[variable_18]], na.rm = TRUE))
                                    ncatt_put(ncout, var18, "valid_max", var18max)
                                    ncatt_put(ncout, var18, "valid_min", var18min)
                                    
                                    
                                    if (numvar > 18){
                                      ncatt_put(ncout, var19, "sensor_type", obj[['model']])
                                      ncatt_put(ncout, var19, "sensor_depth", obj[['depthMin']])
                                      ncatt_put(ncout, var19, "serial_number", obj[['serialNumber']])
                                      ncatt_put(ncout, var19, "generic_name", variable_19)
                                      ncatt_put(ncout, var19, "sdn_parameter_urn", P01_VAR19)
                                      ncatt_put(ncout, var19, "sdn_parameter_name", P01_name_var19)
                                      ncatt_put(ncout, var19, "sdn_uom_urn", P06_var19)
                                      ncatt_put(ncout, var19, "sdn_uom_name", P06_name_var19)
                                      if (!is.null(std_variable_19)){
                                        ncatt_put(ncout, var19, "standard_name", std_variable_19)
                                      }
                                      ncatt_put(ncout, var19, "data_max", max(obj[[variable_19]], na.rm = TRUE))
                                      ncatt_put(ncout, var19, "data_min", min(obj[[variable_19]], na.rm = TRUE))
                                      ncatt_put(ncout, var19, "valid_max", var19max)
                                      ncatt_put(ncout, var19, "valid_min", var19min)
                                      
                                      
                                      if (numvar > 19){
                                        ncatt_put(ncout, var20, "sensor_type", obj[['model']])
                                        ncatt_put(ncout, var20, "sensor_depth", obj[['depthMin']])
                                        ncatt_put(ncout, var20, "serial_number", obj[['serialNumber']])
                                        ncatt_put(ncout, var20, "generic_name", variable_20)
                                        ncatt_put(ncout, var20, "sdn_parameter_urn", P01_VAR20)
                                        ncatt_put(ncout, var20, "sdn_parameter_name", P01_name_var20)
                                        ncatt_put(ncout, var20 , "sdn_uom_urn", P06_var20)
                                        ncatt_put(ncout, var20, "sdn_uom_name", P06_name_var20)
                                        if (!is.null(std_variable_20)){
                                          ncatt_put(ncout, var20, "standard_name", std_variable_20)
                                        }
                                        ncatt_put(ncout, var20, "data_max", max(obj[[variable_20]], na.rm = TRUE))
                                        ncatt_put(ncout, var20, "data_min", min(obj[[variable_20]], na.rm = TRUE))
                                        ncatt_put(ncout, var20, "valid_max", var20max)
                                        ncatt_put(ncout, var20, "valid_min", var20min)
                                        
                                        
                                        if (numvar >20){
                                          ncatt_put(ncout, var21, "sensor_type", obj[['model']])
                                          ncatt_put(ncout, var21, "sensor_depth", obj[['depthMin']])
                                          ncatt_put(ncout, var21, "serial_number", obj[['serialNumber']])
                                          ncatt_put(ncout, var21, "generic_name", variable_21)
                                          ncatt_put(ncout, var21, "sdn_parameter_urn", P01_VAR21)
                                          ncatt_put(ncout, var21, "sdn_parameter_name", P01_name_var21)
                                          ncatt_put(ncout, var21, "sdn_uom_urn", P06_var21)
                                          ncatt_put(ncout, var21, "sdn_uom_name", P06_name_var21)
                                          if (!is.null(std_variable_21)){
                                            ncatt_put(ncout, var21, "standard_name", std_variable_21)
                                          }
                                          ncatt_put(ncout, var21, "data_max", max(obj[[variable_21]], na.rm = TRUE))
                                          ncatt_put(ncout, var21, "data_min", min(obj[[variable_21]], na.rm = TRUE))
                                          ncatt_put(ncout, var21, "valid_max", var21max)
                                          ncatt_put(ncout, var21, "valid_min", var21min)
                                          
                                          
                                          if (numvar >21){
                                            ncatt_put(ncout, var22, "sensor_type", obj[['model']])
                                            ncatt_put(ncout, var22, "sensor_depth", obj[['depthMin']])
                                            ncatt_put(ncout, var22, "serial_number", obj[['serialNumber']])
                                            ncatt_put(ncout, var22, "generic_name", variable_22)
                                            ncatt_put(ncout, var22, "sdn_parameter_urn", P01_VAR22)
                                            ncatt_put(ncout, var22, "sdn_parameter_name", P01_name_var22)
                                            ncatt_put(ncout, var22, "sdn_uom_urn", P06_var22)
                                            ncatt_put(ncout, var22, "sdn_uom_name", P06_name_var22)
                                            if (!is.null(std_variable_22)){
                                              ncatt_put(ncout, var22, "standard_name", std_variable_22)
                                            }
                                            ncatt_put(ncout, var22, "data_max", max(obj[[variable_22]], na.rm = TRUE))
                                            ncatt_put(ncout, var22, "data_min", min(obj[[variable_22]], na.rm = TRUE))
                                            ncatt_put(ncout, var22, "valid_max", var22max)
                                            ncatt_put(ncout, var22, "valid_min", var22min)
                                            
                                            
                                            if (numvar >22){
                                              ncatt_put(ncout, var23, "sensor_type", obj[['model']])
                                              ncatt_put(ncout, var23, "sensor_depth", obj[['depthMin']])
                                              ncatt_put(ncout, var23 , "serial_number", obj[['serialNumber']])
                                              ncatt_put(ncout, var23, "generic_name", variable_23)
                                              ncatt_put(ncout, var23, "sdn_parameter_urn", P01_VAR23)
                                              ncatt_put(ncout, var23, "sdn_parameter_name", P01_name_var23)
                                              ncatt_put(ncout, var23, "sdn_uom_urn", P06_var23)
                                              ncatt_put(ncout, var23, "sdn_uom_name", P06_name_var23)
                                              if (!is.null(std_variable_23)){
                                                ncatt_put(ncout, var23, "standard_name", std_variable_23)
                                              }
                                              ncatt_put(ncout, var23, "data_max", max(obj[[variable_23]], na.rm = TRUE))
                                              ncatt_put(ncout, var23, "data_min", min(obj[[variable_23]], na.rm = TRUE))
                                              ncatt_put(ncout, var23, "valid_max", var23max)
                                              ncatt_put(ncout, var23, "valid_min", var23min)
                                              
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
  
  
  
  ####CF conventions & BODC standards####
  ncatt_put(ncout, 0, 'Conventions', 'CF-1.7')
  ncatt_put(ncout, 0, "creator_type", "person")
  ncatt_put(ncout, 0, "time_coverage_start", as.character(as.POSIXct(obj[['time']][1])))
  ncatt_put(ncout, 0, "time_coverage_end", as.character(as.POSIXct(tail(obj[['time']], n= 1))))
  # ncatt_put(ncout, 0, "time_coverage_start", as.character(as.POSIXct(obj[['startTime']])))
  # ncatt_put(ncout, 0, "time_coverage_end", as.character(as.POSIXct(obj[['startTime']])))
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
 
  
  ncatt_put(ncout, "lon", "sdn_parameter_urn", "SDN:P01::ALONZZ01")
  ncatt_put(ncout, "lat", "sdn_parameter_urn", "SDN:P01::ALATZZ01")
  ncatt_put(ncout, "ELTMEP01", "sdn_parameter_urn", "SDN:P01::ELTMEP01")
  ncatt_put(ncout, "time_string", "sdn_parameter_urn", "SDN:P01::DTUT8601")
  
  ncatt_put(ncout, "lon", "sdn_parameter_name", "Longitude east")
  ncatt_put(ncout, "lat", "sdn_parameter_name", "Latitude north")
  ncatt_put(ncout, 'ELTMEP01', "sdn_parameter_name", "Elapsed time (since 1970-01-01T00:00:00Z)")
  ncatt_put(ncout, 'time_string', "sdn_parameter_name", "String corresponding to format 'YYYY-MM-DDThh:mm:ss.sssZ' or other valid ISO8601 string")
  
  ncatt_put(ncout, "lon", "sdn_uom_urn", "SDN:P06::DEGE")
  ncatt_put(ncout, "lat", "sdn_uom_urn", "SDN:P06:DEGN")
  ncatt_put(ncout, "ELTMEP01", "sdn_uom_urn", "SDN:P06::UTBB")
  ncatt_put(ncout, "time_string", "sdn_uom_urn", "SDN:P06::TISO")
  
  
  ncatt_put(ncout, "lon", "sdn_uom_name", "Degrees east")
  ncatt_put(ncout, "lat", "sdn_uom_name", "Degrees north")
  ncatt_put(ncout, "ELTMEP01", "sdn_uom_name", "Seconds")
  ncatt_put(ncout, "time_string", "sdn_uom_name", "ISO8601")
  
  #####CF standard names####

  ncatt_put(ncout, "lat", "standard_name", "latitude")
  ncatt_put(ncout, "lon", "standard_name", "longitude")
  ncatt_put(ncout, "ELTMEP01", "standard_name", "time")
  
  
  ####QUALITY CONTROL####
  if (numflag >0){
    ncatt_put(ncout, var1_QC, 'flag_values', c(0:9))
    ncatt_put(ncout, var1_QC, 'flag_meanings', 'none good probably_good probably_bad bad changed BD excess interpolated missing')
    ncatt_put(ncout, var1_QC, 'references', 'http://www.iode.org/mg22')
    
    if (numflag >1){
      ncatt_put(ncout, var2_QC, 'flag_values', c(0:9))
      ncatt_put(ncout, var2_QC, 'flag_meanings', 'none good probably_good probably_bad bad changed BD excess interpolated missing')
      ncatt_put(ncout, var2_QC, 'references', 'http://www.iode.org/mg22')
      
      if (numflag >2){
        ncatt_put(ncout, var3_QC, 'flag_values', c(0:9))
        ncatt_put(ncout, var3_QC, 'flag_meanings', 'none good probably_good probably_bad bad changed BD excess interpolated missing')
        ncatt_put(ncout, var3_QC, 'references', 'http://www.iode.org/mg22')
        
        if (numflag >3){
          ncatt_put(ncout, var4_QC, 'flag_values', c(0:9))
          ncatt_put(ncout, var4_QC, 'flag_meanings', 'none good probably_good probably_bad bad changed BD excess interpolated missing')
          ncatt_put(ncout, var4_QC, 'references', 'http://www.iode.org/mg22')
          
          if (numflag >4){
            ncatt_put(ncout, var5_QC, 'flag_values', c(0:9))
            ncatt_put(ncout, var5_QC, 'flag_meanings', 'none good probably_good probably_bad bad changed BD excess interpolated missing')
            ncatt_put(ncout, var5_QC, 'references', 'http://www.iode.org/mg22')
            
            if (numflag >5){
              ncatt_put(ncout, var6_QC, 'flag_values', c(0:9))
              ncatt_put(ncout, var6_QC, 'flag_meanings', 'none good probably_good probably_bad bad changed BD excess interpolated missing')
              ncatt_put(ncout, var6_QC, 'references', 'http://www.iode.org/mg22')
              
              if (numflag >6){
                ncatt_put(ncout, var7_QC, 'flag_values', c(0:9))
                ncatt_put(ncout, var7_QC, 'flag_meanings', 'none good probably_good probably_bad bad changed BD excess interpolated missing')
                ncatt_put(ncout, var7_QC, 'references', 'http://www.iode.org/mg22')
                
                if (numflag >7){
                  ncatt_put(ncout, var8_QC, 'flag_values', c(0:9))
                  ncatt_put(ncout, var8_QC, 'flag_meanings', 'none good probably_good probably_bad bad changed BD excess interpolated missing')
                  ncatt_put(ncout, var8_QC, 'references', 'http://www.iode.org/mg22')
                  
                  if (numflag >8){
                    ncatt_put(ncout, var9_QC, 'flag_values', c(0:9))
                    ncatt_put(ncout, var9_QC, 'flag_meanings', 'none good probably_good probably_bad bad changed BD excess interpolated missing')
                    ncatt_put(ncout, var9_QC, 'references', 'http://www.iode.org/mg22')
                    
                    if (numflag >9){
                      ncatt_put(ncout, var10_QC, 'flag_values', c(0:9))
                      ncatt_put(ncout, var10_QC, 'flag_meanings', 'none good probably_good probably_bad bad changed BD excess interpolated missing')
                      ncatt_put(ncout, var10_QC, 'references', 'http://www.iode.org/mg22')
                      
                      if (numflag >10){
                        ncatt_put(ncout, var11_QC, 'flag_values', c(0:9))
                        ncatt_put(ncout, var11_QC, 'flag_meanings', 'none good probably_good probably_bad bad changed BD excess interpolated missing')
                        ncatt_put(ncout, var11_QC, 'references', 'http://www.iode.org/mg22')
                        
                        if (numflag >11){
                          ncatt_put(ncout, var12_QC, 'flag_values', c(0:9))
                          ncatt_put(ncout, var12_QC, 'flag_meanings', 'none good probably_good probably_bad bad changed BD excess interpolated missing')
                          ncatt_put(ncout, var12_QC, 'references', 'http://www.iode.org/mg22')
                          
                          if (numflag >12){
                            ncatt_put(ncout, var13_QC, 'flag_values', c(0:9))
                            ncatt_put(ncout, var13_QC, 'flag_meanings', 'none good probably_good probably_bad bad changed BD excess interpolated missing')
                            ncatt_put(ncout, var13_QC, 'references', 'http://www.iode.org/mg22')
                            
                            if (numflag >13){
                              ncatt_put(ncout, var14_QC, 'flag_values', c(0:9))
                              ncatt_put(ncout, var14_QC, 'flag_meanings', 'none good probably_good probably_bad bad changed BD excess interpolated missing')
                              ncatt_put(ncout, var14_QC, 'references', 'http://www.iode.org/mg22')
                              
                              if (numflag >14){
                                ncatt_put(ncout, var15_QC, 'flag_values', c(0:9))
                                ncatt_put(ncout, var15_QC, 'flag_meanings', 'none good probably_good probably_bad bad changed BD excess interpolated missing')
                                ncatt_put(ncout, var15_QC, 'references', 'http://www.iode.org/mg22')
                                
                                if (numflag >15){
                                  ncatt_put(ncout, var16_QC, 'flag_values', c(0:9))
                                  ncatt_put(ncout, var16_QC, 'flag_meanings', 'none good probably_good probably_bad bad changed BD excess interpolated missing')
                                  ncatt_put(ncout, var16_QC, 'references', 'http://www.iode.org/mg22')
                                  
                                  if (numflag >16){
                                    ncatt_put(ncout, var17_QC, 'flag_values', c(0:9))
                                    ncatt_put(ncout, var17_QC, 'flag_meanings', 'none good probably_good probably_bad bad changed BD excess interpolated missing')
                                    ncatt_put(ncout, var17_QC, 'references', 'http://www.iode.org/mg22')
                                    
                                    if (numflag >17){
                                      ncatt_put(ncout, var18_QC, 'flag_values', c(0:9))
                                      ncatt_put(ncout, var18_QC, 'flag_meanings', 'none good probably_good probably_bad bad changed BD excess interpolated missing')
                                      ncatt_put(ncout, var18_QC, 'references', 'http://www.iode.org/mg22')
                                      
                                      if (numflag >18){
                                        ncatt_put(ncout, var19_QC, 'flag_values', c(0:9))
                                        ncatt_put(ncout, var19_QC, 'flag_meanings', 'none good probably_good probably_bad bad changed BD excess interpolated missing')
                                        ncatt_put(ncout, var19_QC, 'references', 'http://www.iode.org/mg22')
                                        
                                        if (numflag >19){
                                          ncatt_put(ncout, var20_QC, 'flag_values', c(0:9))
                                          ncatt_put(ncout, var20_QC, 'flag_meanings', 'none good probably_good probably_bad bad changed BD excess interpolated missing')
                                          ncatt_put(ncout, var20_QC, 'references', 'http://www.iode.org/mg22')
                                          
                                          if (numflag >20){
                                            ncatt_put(ncout, var21_QC, 'flag_values', c(0:9))
                                            ncatt_put(ncout, var21_QC, 'flag_meanings', 'none good probably_good probably_bad bad changed BD excess interpolated missing')
                                            ncatt_put(ncout, var21_QC, 'references', 'http://www.iode.org/mg22')
                                            
                                            if (numflag >21){
                                              ncatt_put(ncout, var22_QC, 'flag_values', c(0:9))
                                              ncatt_put(ncout, var22_QC, 'flag_meanings', 'none good probably_good probably_bad bad changed BD excess interpolated missing')
                                              ncatt_put(ncout, var22_QC, 'references', 'http://www.iode.org/mg22')
                                              
                                              if (numflag >22){
                                                ncatt_put(ncout, var23_QC, 'flag_values', c(0:9))
                                                ncatt_put(ncout, var23_QC, 'flag_meanings', 'none good probably_good probably_bad bad changed BD excess interpolated missing')
                                                ncatt_put(ncout, var23_QC, 'references', 'http://www.iode.org/mg22')
                                                
                                            
                                                
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
  }
  
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
