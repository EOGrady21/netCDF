


#timeseries profile template



nc_createTSP <- function(obj, name){
  
  #file name and path
  ncpath <- "./"
  ncname <- paste(ncpath, name, ".nc", sep = "")
  
####dimensions####
  timedim <- ncdim_def("time", "seconds since 1970-01-01T00:00:00Z", as.double(obj[['time']]))    #time formatting FIX
  depthdim <- ncdim_def("depth", "metres", as.double(dist))
  stationdim <- ncdim_def("station", "counts", as.numeric(obj[['station']]))
  londim <- ncdim_def("lon", "degrees_east" , as.double(obj[['lon']]))
  latdim <- ncdim_def("lat", "degrees_north", as.double(obj[['lat']]))
  dimnchar <- ncdim_def('nchar', '', 1:23, create_dimvar = FALSE)
  

####variables definitions####
#FillValue
  FillValue <- 1e35
  #set list of proper code names with name values found in oce objects
  longname <- list('v_1' = 'EWCT', 'v_2' = 'NSCT', 'v_3' = 'ERRV' )
  #set list of appropriate units for each variable
  units <- list('v_1' = 'm/s', 'v_2' = 'm/s', 'v_3' = 'm/s')
  
  
  var <- obj@data
  names <- names(var)
  # 
  # if (inherits(obj, "adp")){
  #   data <- list ('v' = obj@data$v, 'q' =  obj@data$q, 'a' = obj@data$a, 'g' = obj@data$g)
  #   for( d in data){
  #     
  #   }
  #   
  # }
  #separate each slice of arrayed data 
  #FIXME: instead of hard coding in 4 put in dimensions of obj array
for(vr in var){
#   for( i in 1:4){
#     if (length(dim(var[vr])) >= 3){
#     obj <- oceSetData(obj, paste(names[vr]), i, sep = '_', obj[[names[vr]]][,,i])
#     }
# }

    for( i in 1:3){
      if(is.null(dim(var[vr]))){
        dim <- list(timedim, stationdim)
      }
        else if(names(var[vr]) == 'lon' | 'lat'){
          dim <- stationdim
        }
        else (names(var[vr]) == 'time_string'){
          dim <- list(dimnchar, timedim)
        
        }
      }
      if(length(dim(var[vr])) == 3){
        dim <- list(timedim, depthdim, stationdim)
      }
      
      
  
  
prec <- 'float'

  if(names(var[vr] == 'lon' |'lat')){
    prec <- 'double'
  }
   if (names(var[vr]) == 'time_string'){
    prec <- 'char'
  }
    
    
  

  if (length(dim(var[vr])) >= 3){
    for( i in 1:4){
  dlname[vr] <- paste(names(var[vr]), i, sep = '_')
  paste(dlname, 'def', sep = '_') <- ncvar_def(longname = longname$dlname, units = units$dlname, dim = dim, name = dlname, prec = prec)
    }
    else{
      dlname[vr] <- names(var[vr])
      paste(dlname, 'def', sep = '_')<- ncvar_def(longname = longname$dlname, units = units$dlname, dim = dim, name = dlname, prec = prec)
    }
  }

defnames <- paste(dlname[vr], 'def', sep = '_')
}
  
  ncout <- nc_create(ncname, defnames, force_v4 = TRUE)
  
####insert variables####
  #loop through definitions
for( d in defnames){
    #loop through variables
  for(v in var){
    if (length(dim(var[v])) >= 3){
      for(i in 1:4){
        #loop through slices of array
      ncvar_put(ncout, defnames[d], var[v][,,i])
      } else{ #or if data is matrix
          ncvar_put(ncout, defnames[d], var[v])
      }
  }  
}
}
####metadata####
for( m in obj@metadata)  {
  ncatt_put(ncout, 0, names(obj@metadata)[m], obj@metadata[m] )
}
  #list of attributes each variable should include
  attlist <- list('units', 'sdn_parameter_urn', 'sdn_parameter_name', 'standard_name')
  for(vr in var){
    for (a in attlist){
    ncatt_put(ncout, dlname[vr], attlist[a], obj@metadata$attlist$vr)
    }
  }

nc_close(ncout)
  
}