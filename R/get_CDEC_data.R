#' get_CDEC_data 
#'
#' @param dt_rng character string of length 2 with default date format
#' @param cdec_sites site labels
#' @param sensor_num site numbers
#' @param dur_code duration code; defaults to 'D' for day
#' @param verbose if FALSE (default), suppress dplyr::mutate() warnings
#' @param output type of data table output to return, "long", "wide" or "list". Defaults to "wide", which is used in env_comp()
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
    verbose=FALSE,
    output="wide"
)
{
  
  # https://cdec.water.ca.gov/misc/senslist.html
  
  stopifnot(output %in% c("wide","long","list"))
  
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
  

  CDEC_data_ls <- lapply(cdec_sites,function(x) {
    
    CDEC_data_w_raw <- CDEC_data[CDEC_data$site==x,] %>%
      dplyr::select(-SENSOR_NUMBER,-SENSOR_TYPE) %>%
      dplyr::mutate(UNITS=ifelse(UNITS=="DEG F","DEG_F",UNITS)) %>%
      dplyr::mutate(UNITS=ifelse(UNITS=="DEG C","DEG_C",UNITS)) %>%
      tidyr::pivot_wider(values_from = value,names_from = UNITS) 
    
    if(x=="CLC"){
      CDEC_data_w_raw <-   CDEC_data_w_raw %>% dplyr::mutate(DEG_F=NA)
    }

    CDEC_data_w_raw <-   CDEC_data_w_raw %>%  dplyr::mutate(value=ifelse(is.na(DEG_C),fah_to_cel(DEG_F),DEG_C))
    return(CDEC_data_w_raw)
  }
  )
  
  names(CDEC_data_ls) <- cdec_sites
  

  CDEC_data_l <- dplyr::bind_rows(CDEC_data_ls) %>%
    dplyr::mutate(
      UNIT="DEG_C",
      value=ifelse(is.na(DEG_C),fah_to_cel(DEG_F),DEG_C)) %>%
    dplyr::select(-DEG_F,-DEG_C)
  
  if(output=="long"){return(CDEC_data_l)}
  
  CDEC_data_w <- CDEC_data_l %>% 
    dplyr::select(date,DOY,site,value) %>%
    tidyr::pivot_wider(names_from = "site")
  
  if(output=="wide"){return(CDEC_data_w)}
  

  # objects that that output="list" returns
  list(
    "raw_csv"=raw_csv,
    "CDEC_data_unprocessed"=CDEC_data,
    "wide"=CDEC_data_w,
    "long"=CDEC_data_l
    )
  
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