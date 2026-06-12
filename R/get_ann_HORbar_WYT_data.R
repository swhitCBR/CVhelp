#' Title
#'
#' @param dt_rng_in character string with two dates
#'
#' @returns annual summary of water year type and HOR barrier conditions
#' @export
#'
get_ann_HORbar_WYT_data <- function(
    dt_rng_in = c("2011-01-01", "2024-12-31")  
  ){
  HOR_bar_ann_data <- data.frame(CVhelp::get_HOR_barrier_data(dt_rng=dt_rng_in) %>% 
                                   dplyr::mutate(year=format(date,"%Y"),
                                                 Year=as.numeric(year)) %>%
                                   dplyr::group_by(Year) %>% 
                                   dplyr::summarize(barrier_days=sum(barrierTF),
                                                    barrier=ifelse(sum(barrierTF)>0,"In","Out"))) #%>%
  
  WYT_data <- CVhelp:::get_WYT_data(dt_rng=dt_rng_in) #%>% rename(year=Year)# local access
  ann_HORbar_WYT_data <- HOR_bar_ann_data %>% dplyr::left_join(WYT_data,by="Year")
  return(ann_HORbar_WYT_data)}