#' @title Impute missing values for OMT
#'
#' @param USGS_dat_ls_in data obtained from USGS 
#' @param raw if TRUE, also prind ORB and MID values
#' @param wide if TRUE, output wide format
#'
#' @returns wide- or long-form dataframe with OMT value computed (i.e., ORB + MID)
#' 
#' @import simputation
#' @export
#'
calc_OMT <- function(USGS_dat_ls_in,raw=FALSE,wide=F){
  
  dv_data_ls_2 <- USGS_dat_ls_in[["ORB"]] %>% dplyr::rename(ORB=value) %>% dplyr::select(-site,-monitoring_location_id)
  dv_data_ls_3  <- USGS_dat_ls_in[["MID"]] %>% dplyr::rename(MID=value) %>% dplyr::select(-site,-monitoring_location_id)
  OMT_df <- dv_data_ls_3 %>% 
    dplyr::full_join(dv_data_ls_2) %>% 
    dplyr::mutate(OMT=ORB + MID,site="OMT") %>%
    dplyr::relocate(date,unit_of_measure,year,DOY,site) 
  
  if(wide){return(OMT_df)}
  if(raw){
    OMT_df <- OMT_df %>%  
      dplyr::select(-site) %>% 
      tidyr::gather(key="site",value="value", c("MID","ORB","OMT"))
    return(OMT_df) }
  
  OMT_df <- OMT_df %>% dplyr::select(-MID,-ORB) %>% dplyr::rename(value=OMT)
  return(OMT_df)
  
}