####metadata check####

#When converting ODF to netCDF, run this check to ensure metadata is properly
#pulled from ODF and there are no inconsistencies in the original document


check_nc <- function(ncfile){
  
  nc <- nc_open(ncfile)
  
  a <- list()
  ####ODF atts####
  #check file name
  a$fileName <- ncfile
  
  #check country code
  
  country_code <- ncatt_get(nc, 0, 'country_code')
  if (country_code$hasatt) {
    if (country_code$value != '1810') {
      a$country_code <- country_code$value
    }
  }else{
    a$country_code <- 'MISSING'
  }
  
  
  #check organization/institution
  institution <- ncatt_get(nc, 0, 'institution')
  if (institution$hasatt) {
    if (institution$value != 'DFO BIO') {
      a$institution <- institution$value
    }
    }else{
    a$institution <- 'MISSING'
  }
  
  #chief scientist
  chief_scientist <- ncatt_get(nc, 0, 'chief_scientist')
  if (chief_scientist$hasatt) {
    ;
  } else{
  a$chief_scientist <- 'MISSING'
}


 #start end times

time_coverage_start <- ncatt_get(nc, 0, 'time_coverage_start')
time_coverage_end <- ncatt_get(nc, 0, 'time_coverage_end')

if (time_coverage_start$hasatt & time_coverage_end$hasatt){
  time <- ncvar_get(nc, 'time_string')
  if (time_coverage_start$value !=  startTime[1]){
  a$time_coverage_start <- time_coverage_start$value
  }
  
}else{
  a$time_coverage <- 'MISSING'
}

  #data type
  
data_type <- ncatt_get(nc, 0, 'data_type')

if (data_type$hasatt){
  ;
}else{
  a$data_type <- 'MISSING'
}

  #event number
event_number <- ncatt_get(nc, 0, 'event_number')

if (event_number$hasatt){
  ;
}else{
  a$event_number <- 'MISSING'
}
  
  #event qulifiers
  
  #creation date
  
  #start end date time
  
  #lat and lons
  
  #depths
  
  ####NC atts####
}