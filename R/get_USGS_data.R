#' @title Access USGS data via  
#'
#' @param dt_rng character string of length 2 with default date format
#' @param monitoring_location_id character or string with monitoring ids 'USGS-XXXXXXXX'
#' @param parameter_codes character string with 5 digit numeric parameter codes
#' @param sites character string of site labels corresponding to monitoring ds
#' @param verbose if TRUE, print USGS API messages, including number of remaining requests this hour
#'
#' @description A utils function that queries the USGS' NWIS database: https://waterdata.usgs.gov/nwis/
#' 
#' @details Link to usgs sites in the South Delta: https://waterdata.usgs.gov/explore/#mapCenter=37.910090880908236,-121.54037475585939&dataCollections=continuous&mapZoomLevel=11
#'
#' @return The return value, if any, from executing the utility.
#' 
#' @import dataRetrieval
#'
#' @noRd
get_USGS_data <- function(
    dt_rng=c("2011-01-01","2016-12-31"),
    monitoring_location_id = c("USGS-11303500","USGS-11313405","USGS-11312676"),
    parameter_codes = c("00060","72137","72137"),
    sites= c("VNS","ORB","MID"),
    verbose=FALSE
)
{
  
  # 11313434 quimby code
  # 11312676 Middle River at Middle River
  
  last_dt <- dt_rng[2]
  first_dt <- dt_rng[1]

  dat_in <- data.frame(sites,monitoring_location_id,first_dt,last_dt,parameter_codes)

  if(verbose){
    out <- lapply( 1:nrow(dat_in), function(ii) {
      dataRetrieval::read_waterdata_daily(
        monitoring_location_id = dat_in$monitoring_location_id[ii],
        parameter_code = dat_in$parameter_codes[ii],
        time = c(dat_in$first_dt[ii],dat_in$last_dt[ii]),
        skipGeometry = TRUE,) %>%
        dplyr::select(monitoring_location_id,time,unit_of_measure,value) %>%
        dplyr::rename(date=time) %>%
        dplyr::mutate(year=format(date,"%Y"),
                      DOY=as.numeric(format(date,"%j")),
                      site=dat_in$sites[ii]) %>%
        dplyr::relocate(site)
    })
  } else{
  
  out <- suppressMessages(lapply( 1:nrow(dat_in), function(ii) {
    dataRetrieval::read_waterdata_daily(
      monitoring_location_id = dat_in$monitoring_location_id[ii],
      parameter_code = dat_in$parameter_codes[ii],
      time = c(dat_in$first_dt[ii],dat_in$last_dt[ii]),
      skipGeometry = TRUE,) %>%
      dplyr::select(monitoring_location_id,time,unit_of_measure,value) %>%
      dplyr::rename(date=time) %>%
      dplyr::mutate(year=format(date,"%Y"),
                    DOY=as.numeric(format(date,"%j")),
                    site=dat_in$sites[ii]) %>%
      dplyr::relocate(site)
  }))
  }
  

  
  names(out) <- dat_in$sites
  
  return(out)
}


