#' @title Compile environmental data for CVPAS-steelhead inputs 
#'
#' @param url defaults to CDEC url of water year types (WYT) 'WSIHIST'
#' @param basin basin for WYT lookup; either 'SJ' or 'SAC' 
#' @param dt_rng character string of length 2 with default date format
#' @param DOY_rng character string
#' @param impute_OMT if TRUE, fill in missing daily values for OMT using linear imputation ('simputation' package)
#' @param output type of data table output to return, "long", "wide" or "list"; defaults to "wide".
#'
#' @seealso get_CDEC_data()
#'
#' @return dataframe or list with combined, daily-scale environmental variabltes
#' 
#'
env_comp <- function(
    url="https://cdec.water.ca.gov/reportapp/javareports?name=WSIHIST",
    basin="SJ",
    dt_rng=c("2011-01-01","2016-12-31"),
    DOY_rng=1:250,
    impute_OMT=TRUE,
    output="wide"
){
  
  stopifnot(output %in% c("wide","long","list"))
  
  CDEC_env_raw <- get_CDEC_data(dt_rng=dt_rng,output = "wide")
  USGS_env_raw <- get_USGS_data(dt_rng = dt_rng)
  CDEC_wyt_class_raw <- get_WYT_data(dt_rng = dt_rng)
  HOR_barrier_raw <- get_HOR_barrier_data(dt_rng = dt_rng)
  get_dayflow_raw <- get_dayflow_data(dt_rng = dt_rng)

  full_dt_arr <- data.frame(
    date=seq.Date(dt_rng[1],dt_rng[2])) %>% 
    dplyr::mutate(Year=as.numeric(format(date,"%Y")),
                  DOY=as.numeric(format(date,"%j"))) %>%
    dplyr::filter(DOY %in% DOY_rng)
  # 
  # return(full_dt_arr)
  
  envDat_w <- full_dt_arr %>%   
    dplyr::left_join(CDEC_wyt_class_raw %>% dplyr::select(Year,WYT),by="Year")  %>% 
    dplyr::left_join(USGS_env_raw[["VNS"]] %>% dplyr::rename(VNS=value) %>% dplyr::select(date,VNS),by="date") %>%
    dplyr::left_join(USGS_env_raw[["ORB"]] %>% dplyr::rename(ORB=value) %>% dplyr::select(date,ORB),by="date") %>%
    dplyr::left_join(USGS_env_raw[["MID"]] %>% dplyr::rename(MID=value) %>% dplyr::select(date,MID),by="date") %>%
    dplyr::mutate(OMT = ORB + MID) %>%
    # adding wide-format CDEC data
    dplyr::left_join(CDEC_env_raw %>% dplyr::select(date,MSD,CLC),by="date") %>%
    # adding barrierTF data
    left_join(HOR_barrier_raw %>% dplyr::select(date,barrierTF),by="date") %>%
    left_join(get_dayflow_raw %>% dplyr::select(date,SWP,CVP,OUT),by="date")
  

  missing_env_raw <- data.frame(which(is.na(envDat_w),arr.ind = T)) %>%
    dplyr::mutate(site=names(envDat_w)[col],
                  Year=envDat_w$Year[row],
                  doy=envDat_w$DOY[row])
  
  missing_env_summ <- missing_env_raw %>% 
    group_by(site,Year) %>% 
    summarize(
      n_=length(unique(Year)),
      n_doy=length(doy),
      n_min=min(doy),
      n_max=max(doy))
  
  message(paste0(sum(is.na(envDat_w$OMT))," days with missing OMT values (",
                round(sum(is.na(envDat_w$OMT))/nrow(envDat_w)*100),"%)"))
  
  if(impute_OMT){
      # imp_obj <- simputation::impute_lm(dat=envDat_w,formula = OMT~VNS+ORB+MSD+CLC)
      imp_obj <- simputation::impute_lm(dat=envDat_w,formula = OMT~VNS+MID+MSD+CLC)
      envDat_w$OMT_imputed <- is.na(envDat_w$OMT)
      envDat_w$OMT <- imp_obj$OMT
    }
  
  
  if(!detail){
    envDat_w <- envDat_w %>% select(-OMT_imputed,-X2,-EXPORTS)
  }
  
  if(output=="wide"){return(envDat_w)}
  
  envDat_l <- tidyr::pivot_longer(envDat_w,cols = c("VNS","ORB","MID","MSD","CLC","OMT"),names_to = "site") %>%
    arrange(site,Year,WYT,date)

  if(output=="long"){return(envDat_l)}
  
  # gets rid of extra TRUE values that are an arifact of lengthening DF
  if(impute_OMT){
    envDat_l$OMT_imputed[envDat_l$site!="OMT"] <- FALSE
  }
  
  env_data_ls <- list(
    "full_dt_arr"=full_dt_arr,
    "envDat_w"=envDat_w,
    "envDat_l"=envDat_l,
    "missing_env_raw"=missing_env_raw)
  
  return(env_data_ls)
  
}