#' @title Create a DEDF from ping- and receiver level detection data
#' 
#' @description Reduce ping-level detection data to first and last time at general location event
#'
#' @param dat_in dataframe with ping-level and receiver level detection, corresponds to  `det_pings` output from `extract_single_study_ping_data()`
#' @param max_delay maximum delay for defining separate detection events
#' @param min_detect minimum number of individual instantaneous detection (pings) for defining an 'event'
#' @param start_time earliest allowable detection time
#' @param end_time latest allowable detection time
#'
#' @returns detection event data frame (DEDF)
#' @export
#'
DEDF_from_ping_data <- function(dat_in,
                                max_delay = ubridate::hours(1),
                                min_detect = 2,
                                start_time = NULL,
                                end_time = NULL){
  
  if(is.null(start_time))
    start_time = min(dat_in$detectDT_UTC)
  
  if(is.null(end_time))
    end_time = max(dat_in$detectDT_UTC)
  
  dat_in %>%
    filter(detectDT_UTC >= start_time,
           detectDT_UTC <= end_time) %>%
    group_by(studyID, tagID) %>%
    arrange(detectDT_UTC) %>%
    mutate(diff_time_ind = as.integer(detectDT_UTC - lag(detectDT_UTC) > max_delay |
                                        gen_loc != lag(gen_loc))) %>%
    tidyr::replace_na(list(diff_time_ind = 1)) %>%
    mutate(det_event = cumsum(diff_time_ind)) %>%
    dplyr::select(-diff_time_ind) %>%
    ungroup() %>%
    arrange(tagID, detectDT_UTC) %>%
    group_by(studyID,
             tagID,
             det_event,
             gen_loc) %>%
    summarise(first_det = min(detectDT_UTC),
              last_det = max(detectDT_UTC),
              n_det = n()) %>%
    filter(n_det >= min_detect) %>%
    ungroup() %>%
    dplyr::select(-det_event) %>%
    `attr<-`("max_delay", max_delay) %>%
    `attr<-`("min_detect", min_detect) %>%
    return()
}