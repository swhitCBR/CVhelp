#' Title
#'
#' @param dt_rng date range
#' @description
#' Accessing Dayflow datatables in chunks broken up by year
#' Dayflow summary:
#' https://water.ca.gov/Programs/Integrated-Science-and-Engineering/Compliance-Monitoring-And-Assessment/Dayflow-Data
#' 
#'
#' @returns dataframe with dayflow measures from CNRA
#' @export
#'
get_dayflow_data <- function(dt_rng=c("2011-01-01","2016-12-31")){
  
  dts_in <- seq.Date(dt_rng[1],dt_rng[2])
  yr_in <- unique(format(dts_in,"%Y"))
  
  # return(yr_in)
  
  tst_ls <- lapply(yr_in,function(x){
    a = get_CNRA_dayflow_by_year(x)
    yr_chk=1+(x==2024)
    a$Date=as.Date(a$Date,format=c("%m/%d/%Y","%Y-%m-%d")[yr_chk])
    # a$Date=as.Date(a$Date,format="%Y-%m-%d")
    a$date=format(a$Date,"%Y-%m-%d")
    return(a)
  })
  
  
  dayflow_yr_sclDF <- dplyr::bind_rows(tst_ls)
  dayflowDF <- dayflow_yr_sclDF %>%
    dplyr::filter(date >= dt_rng[1] & date <= dt_rng[2]) %>%
    dplyr::select(-Date,-X_id) %>%
    dplyr::relocate(date) %>%
    dplyr::mutate(date=as.Date(date))
  
  return(dayflowDF)
} 