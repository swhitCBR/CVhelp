#' Title
#'
#' @param dt_rng date range
#'
#' @returns dataframe with dayflow measures from CNRA
#' @export
#'
get_dayflow_data <- function(dt_rng=c("2011-01-01","2016-12-31")){
  
  dts_in <- seq.Date(dt_rng[1],dt_rng[2])
  yr_in <- unique(format(dts_in,"%Y"))
  
  # return(yr_in)

  tst_ls <- lapply(yr_in,get_CNRA_dayflow_by_year)
  dayflow_yr_sclDF <- dplyr::bind_rows(tst_ls)

  # return(dayflow_yr_sclDF)
  
  dayflow_yr_sclDF$date <- as.Date(dayflow_yr_sclDF$Date,format="%m/%d/%Y")
  
  # print(dim(dayflow_yr_sclDF))
  
  dayflowDF <- dayflow_yr_sclDF %>% 
    dplyr::filter(date >= dt_rng[1] & date <= dt_rng[2]) %>%
    dplyr::select(-Date,-X_id) %>%
    dplyr::relocate(date)
  
  # print(dim(dayflowDF))
  
  return(dayflowDF)
} 




#' generate URL for downloading .csv files from CNRA dayflow
#' @description based on https://data.cnra.ca.gov/dataset/dayflow/resource/6a7cb172-fb16-480d-9f4f-0322548fee83?view_id=5017ca69-a29c-43f4-9007-20ef0e1bdc54
#' @param yr_in single year in 
#'
#' @returns URL for downloading csvs
#' @export
#'
get_CNRA_dayflow_by_year <- function(yr_in){
  
  stopifnot(length(yr_in)==1|!is.na(yr_in))
  if(!yr_in %in% 1985:2025){stop("year_in must be between 1985 and 2025")}
  
  # does not include X2
  if(yr_in %in% c(1985:1995)){
    URL_out <- 
    paste0(
      "https://data.cnra.ca.gov/datastore/dump/cb04e626-9729-4105-af81-f6e5a37f116a?q=&plain=False&language=simple&sort=_id+asc&filters=%7B%22Year%22%3A+%5B%22",
      yr_in,
      "%22%5D%7D&format=csv&fields=_id%2CYear%2CDate%2CSWP%2CCVP%2CEXPORT%2COUT")
  }
  
  if(yr_in == c(1996)){
    # string of 2 URLS
    URL_out <- 
    c(
      paste0(
        "https://data.cnra.ca.gov/datastore/dump/cb04e626-9729-4105-af81-f6e5a37f116a?q=&plain=False&language=simple&sort=_id+asc&filters=%7B%22Year%22%3A+%5B%22",
        yr_in,
        "%22%5D%7D&format=csv&fields=_id%2CYear%2CDate%2CSWP%2CCVP%2CEXPORT%2COUT"),
      paste0(
        "https://data.cnra.ca.gov/datastore/dump/21c377fe-53b8-4bd6-9e1f-2025221be095?q=&plain=False&language=simple&sort=_id+asc&filters=%7B%22Year%22%3A+%5B%22",
        yr_in,
        "%22%5D%7D&format=csv&fields=_id%2CYear%2CDate%2CSWP%2CCVP%2CEXPORTS%2COUT%2CX2"))
  }
  
  if(yr_in %in% c(1997:2022)){
    URL_out <- 
    paste0(
      "https://data.cnra.ca.gov/datastore/dump/21c377fe-53b8-4bd6-9e1f-2025221be095?q=&plain=False&language=simple&sort=_id+asc&filters=%7B%22Year%22%3A+%5B%22",
      yr_in,
      "%22%5D%7D&format=csv&fields=_id%2CYear%2CDate%2CSWP%2CCVP%2CEXPORTS%2COUT%2CX2")
  }
  
  if(yr_in ==2023){
    URL_out <- 
    paste0(
      "https://data.cnra.ca.gov/datastore/dump/f7c1ba7f-bd64-4762-88e3-6db9b2501b38?q=&plain=False&language=simple&sort=_id+asc&filters=%7B%7D&format=csv&",
      "fields=_id%2CYear%2CDate%2CSWP%2CCVP%2CEXPORTS%2COUT%2CX2")
  }
  
  if(yr_in ==2024){
    URL_out <- 
    paste0(
      "https://data.cnra.ca.gov/datastore/dump/6a7cb172-fb16-480d-9f4f-0322548fee83?q=&plain=False&language=simple&sort=_id+asc&filters=%7B%7D&format=csv&",
      "fields=_id%2CYear%2CDate%2CSWP%2CCVP%2CEXPORTS%2COUT%2CX2")
  }
  if(yr_in ==2025){
    URL_out <- 
    paste0(
      "https://data.cnra.ca.gov/datastore/dump/541fe1b7-919a-467d-ac1e-ddbb8328c8f1?q=&plain=False&language=simple&sort=_id+asc&filters=%7B%7D&format=csv&",
      "fields=_id%2CYear%2CDate%2CSWP%2CCVP%2CEXPORTS%2COUT%2CX2")
  }
  
  # return(URL_out)
  
  out_ls_yr = lapply(URL_out,read.csv)  
  
  # for(ii in 1:len)
  # if(names(out_ls_yr)=="EXPORT"){}
  
  outDF_yr = dplyr::bind_rows(out_ls_yr) 
  outDF_yr = outDF_yr[!duplicated(outDF_yr$Date),]
  

  return(outDF_yr)
  
  
}
