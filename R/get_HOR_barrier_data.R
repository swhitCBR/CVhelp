#' Title
#'
#' @param last_year final year where daily barrier TRUE/FALSE values will be reported
#'
#' @description
#' Access the Head of Old River (HOR) barrier installation (start and end times), breach, and removal (start and end times)
#' PDF doc with related info is here:
#' https://water.ca.gov/-/media/DWR-Website/Web-Pages/Programs/State-Water-Project/Operations-And-Maintenance/Files/Bay-Delta/South-Delta-Temporary-Barriers-Project/History/2023SpringHeadOfOldRiverSch_ADA.pdf
#' The actual data table is taken from the USGS gitlab 'predator_filter' package files here: data/auxiliary_data/HORB_physical_schedule_1992_2021.csv 
#'
#'
#' @returns
#' @export
#'
get_HOR_barrier_data <- function(last_year=NULL){
  HORB_scedDF_summ <- get_barrier_strt_stop()
  
  dt_ls <- list()
  for(ii in 1:nrow(HORB_scedDF_summ)){
    dt_ls[[ii]] <- data.frame(Year=HORB_scedDF_summ$Year[ii],
                              date=seq.Date(from = HORB_scedDF_summ$Install[ii],to = HORB_scedDF_summ$Remove[ii]))
  }
  # dates where barrier=IN (by the broadest definition)
  barrier_indays_DF <- do.call(rbind,dt_ls)
  
  if(is.null(last_year)){
    last_year=max(HORB_scedDF_summ$Year)
  }
  
  barrierDF <- data.frame(
    date=seq.Date(paste0(min(HORB_scedDF_summ$Year),"-01-01"),
                  paste0(last_year,"-12-31")))
  barrierDF$barrierTF=barrierDF$date %in% barrier_indays_DF$date
  
  return(barrierDF)
}



#' Title
#'
#' @returns table of years between 1992 and 2021 where HOR barrier was in place
#' @export
#'
get_barrier_strt_stop <- function(){
  data.frame(
    Year=c(1992,1994,1996,1997,2000,2001,2002,2003,2004,2007,2012,2014,2015,2016,2018),
    barrier_yearTF=c(TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,TRUE),
    Install=as.Date(c('1992-04-15','1994-04-21','1996-05-06','1997-04-09','2000-04-05','2001-04-17','2002-04-02','2003-04-01','2004-04-01','2007-04-11','2012-03-15','2014-03-25','2015-03-16','2016-03-10','2018-03-16')),
    Remove=as.Date(c('1992-06-08','1994-05-20','1996-09-03','1997-05-19','2000-06-02','2001-05-30','2002-06-07','2003-06-03','2004-06-10','2007-06-06','2012-06-20','2014-06-26','2015-06-08','2016-06-14','2018-05-17')))
}
