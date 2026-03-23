#' Title
#'
#' @param last_year final year where daily barrier TRUE/FALSE values will be reported
#' @param dt_rng date range
#'
#' @description
#' Access the Head of Old River (HOR) barrier installation (start and end times), breach, and removal (start and end times)
#' PDF doc with related info is here:
#' https://water.ca.gov/-/media/DWR-Website/Web-Pages/Programs/State-Water-Project/Operations-And-Maintenance/Files/Bay-Delta/South-Delta-Temporary-Barriers-Project/History/2023SpringHeadOfOldRiverSch_ADA.pdf
#' The actual data table is taken from the USGS gitlab 'predator_filter' package files here: data/auxiliary_data/HORB_physical_schedule_1992_2021.csv 
#' 
#' Spring HOR barrier not installed after 2019 b/c of high flow in 2019 and because after 2020 the barrier was not required in the NMFS BiOp on Long-term Operations of the CVP and SWP (see 'xx' comment in PDF).
#'
#' @import dplyr
#'
#' @returns head of old river (HOR) barrier status
#' @export
#'
get_HOR_barrier_data <- function(
    dt_rng=c("2011-01-01","2016-12-31"),
    last_year=NULL
    )
  {
  
  if(any(sapply(dt_rng,function(x) as.Date(x)> as.Date("2023-10-29")))){
    message("spring HOR barrier not installed after 2018 (run `?get_HOR_barrier_data()` for details)")
    message("daily HOR barrier status beyond 2019-01-01 set to FALSE")
  }
  
  if(any(sapply(dt_rng,function(x) as.Date(x)< as.Date("1992-04-15")))){
    message("no spring HOR barrier information prior to 1992 (run `?get_HOR_barrier_data()` for details)")
  }
  
  
  HORB_scedDF_summ <- get_barrier_strt_stop()
  
  dt_ls <- list()
  for(ii in 1:nrow(HORB_scedDF_summ)){
    dt_ls[[ii]] <- data.frame(Year=HORB_scedDF_summ$Year[ii],
                              date=seq.Date(from = HORB_scedDF_summ$Install[ii],
                                            to = HORB_scedDF_summ$Remove[ii]))
  }
  # dates where barrier=IN (by the broadest definition)
  barrier_indays_DF <- do.call(rbind,dt_ls)
  
  if(is.null(last_year)){
    # last_year=max(HORB_scedDF_summ$Year)
    last_year = format(as.Date(dt_rng)[2],"%Y")
  }

  
  barrierDF <- data.frame(
    date=seq.Date(paste0(min(HORB_scedDF_summ$Year),"-01-01"),
                  paste0(last_year,"-12-31")))
  barrierDF$barrierTF=barrierDF$date %in% barrier_indays_DF$date
  barrierDF$barrierTF[barrierDF$date > as.Date("2019-01-01")]=FALSE
  
  barrierDF <- barrierDF %>% dplyr::filter(date >= dt_rng[1] & date <= dt_rng[2])
  
  
  return(barrierDF)
}



#' Title
#'
#' @description
#' Hard-coding of spring HOR barrier 
#' 
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
