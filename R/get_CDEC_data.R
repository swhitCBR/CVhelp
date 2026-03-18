#' get_CDEC_data 
#'
#' @param dt_rng character string of length 2 with default date format
#' @param cdec_sites site labels
#' @param sensor_num site numbers
#' @param dur_code duration code; defaults to 'D' for day
#' @param verbose if FALSE (default), suppress dplyr::mutate() warnings
#'
#' @description A utils function
#'
#' @return The return value, if any, from executing the utility.
#'
#' @noRd
get_CDEC_data <- function(
    dt_rng=c("2011-01-01","2016-12-31"),
    cdec_sites=c("CLC",'MSD'),
    sensor_num=c(25,146), # default to Fahrenheit sensor
    dur_code="D", # default to Daily summary
    verbose=FALSE
)
{
  
  # https://cdec.water.ca.gov/misc/senslist.html
  
  paste0(cdec_sites,collapse="%2C") ## '%2C' encoding for comma. See: https://en.wikipedia.org/wiki/Percent-encoding
  url_in <- paste0(
    "https://cdec.water.ca.gov/dynamicapp/req/CSVDataServlet?Stations=",
    # paste0(cdec_sites,collapse="%2C"),"&SensorNums=146&dur_code=D&Start=",
    paste0(cdec_sites,collapse="%2C"),"&SensorNums=",
    paste0(sensor_num,collapse="%2C"),"&dur_code=",
    paste0(dur_code,collapse="%2C"),"&Start=",
    dt_rng[1],"&End=",dt_rng[2])
  # return(url_in)
  
  raw_csv <- read.csv(url_in)
  
  if(verbose){
    CDEC_data <- raw_csv %>% 
                  dplyr::rename(site=STATION_ID,value=VALUE) %>%
                  dplyr::mutate(value=as.numeric(value)) %>%
                  dplyr::mutate(Year=as.numeric(substr(DATE.TIME,1,4)),
                                month=as.numeric(substr(DATE.TIME,5,6)),
                                day=as.numeric(substr(DATE.TIME,7,8)),
                                date=as.Date(paste(Year,month,day,sep="-")),
                                DOY=as.numeric(format(date,"%j")))
  } else{
  CDEC_data <- suppressWarnings(raw_csv %>% 
    dplyr::rename(site=STATION_ID,value=VALUE) %>%
    dplyr::mutate(value=as.numeric(value)) %>%
    dplyr::mutate(Year=as.numeric(substr(DATE.TIME,1,4)),
                  month=as.numeric(substr(DATE.TIME,5,6)),
                  day=as.numeric(substr(DATE.TIME,7,8)),
                  date=as.Date(paste(Year,month,day,sep="-")),
                  DOY=as.numeric(format(date,"%j"))))
  }
  
  # CDEC_data$value 
  CDEC_data$value<- ifelse(CDEC_data$UNITS=="DEG F",fah_to_cel(CDEC_data$value),CDEC_data$value)
  
  CDEC_data_ls <- lapply(cdec_sites,function(x) CDEC_data[CDEC_data$site==x,])
  names(CDEC_data_ls) <- cdec_sites
  
  return(CDEC_data_ls)
}

#' Title
#'
#' @param fah temperature in Fahrenheit
#'
#' @returns temperature in celsius
#' @export
#'
fah_to_cel <- function(fah){
  cel <- (fah-32)*5/9
  return(cel)
}