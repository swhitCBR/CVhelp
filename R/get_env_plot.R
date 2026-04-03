#' Title
#'
#' @param env_data_long_in long-form version of daily environmental covariates
#' @param DOY_rng Day of year range
#' @param highlight_years years to highlight; detault to mdoel years
#' @param yr_high_lb factor variable for highlight years
#' @param var_fact_levels factor variable ordering
#'
#' @returns ggplot of relevant environmental variables
#' @export
#' 
#' @examples
#' # example code
#' \dontrun{
#' env_all_dat <- CVhelp::env_comp(output="long",dt_rng = c("2010-01-01","2024-12-31"))
#' env_all_plt <- CVhelp::get_env_plot(env_all_dat)
#' 
#' }
#' 
#'
get_env_plot <- function(
     env_data_long_in,
     DOY_rng=NULL,
     highlight_years=c(2011:2016),
     # yr_high_lb="Model_years",
     var_fact_levels=
       c("VNS","OUT","CVP","SWP","ORB",
         "MID","OMT","CLC","MSD")
     ){

  
  env_data_long_in <- env_data_long_in %>% 
    mutate(
      tmp_yr_lab=Year %in% highlight_years,
      year=Year,
      Year=factor(Year),
      variable=factor(variable,levels=var_fact_levels))
  
  # names(env_data_long_in)[names(env_data_long_in)=="tmp_yr_lab"]=yr_high_lb
  
  
  if(is.null(DOY_rng)){
    DOY_rng =c(min(env_data_long_in$DOY),max(env_data_long_in$DOY))}
  
  ggplot2::ggplot(data=env_data_long_in) + 
    ggplot2::facet_grid(variable~WYT,scales="free") + 
    ggplot2::geom_line(data=env_data_long_in %>%
                         dplyr::filter(Year %in% highlight_years)  %>%
                         dplyr::mutate(Year=factor(Year)),
                       ggplot2::aes(x=DOY,y=value,group=Year),color="black",linewidth=1.5) +
    ggplot2::geom_line(data=env_data_long_in,
                       ggplot2::aes(x=DOY,y=value,color=Year)#,linewidth=yr_high_lb)
                       ) +
    ggplot2::geom_rug(data=env_data_long_in %>% filter(is.na(value)),
                      ggplot2::aes(x=DOY),color="black",linewidth = 1.5) +
    ggplot2::geom_rug(data=env_data_long_in %>% filter(is.na(value)),
                      ggplot2::aes(x=DOY,color=Year)) +
    ggplot2::scale_linewidth_manual(values=c(0.2,0.4)) + 
    ggplot2::scale_linetype_manual(values=c("dotted","solid")) +
    ggplot2::scale_x_continuous(limits=DOY_rng)
} 