#' @title Extract example data set from db using studyID
#' 
#' @description
#'  Extract list of tables containing information relevant to each studyID in the sqlite3 database created for 'predator_filter' gitlab
#'
#' @param study corresponds to 'studyID' variable in 'studies' table of sqlite3 database 
#' @param db location of sqllite database created for 'predator_filter' gitlab
#' @param start_time earliest allowable detection time
#' @param end_time latest allowable detection time
#'
#' @returns named list of table , including ping-level and receiver level detection `det_pings`,
#' deployment ids matched with general locations `dep_gl_tb`=gen_loc_sub,tag release information `rls`, and release location info `rel_loc`
#' 
#' @export
#'
extract_single_study_ping_data <- function(study,
                                           db,
                                           start_time = NULL,
                                           end_time = NULL
){
  # extracting data group (dg) vector
  dg = tbl(db, "studies") %>%
    filter(.data$studyID == study) %>%
    collect() %>% 
    pull(.data$data_group)
  
  # filtering by data group and creating table to match deployments (depID) with general locations (gen_loc)
  gen_locs <-  tbl(db, "deploys") %>%
    filter(.data$data_group == dg) %>%
    collect() %>%
    dplyr::select(.data$depID, .data$gen_loc)
  
  rls <- tbl(db, "rls") %>%
    filter(.data$studyID == study) %>%
    collect() %>%
    mutate(releaseDT_UTC  = lubridate::ymd_hms(.data$releaseDT_UTC ))
  
  rel_loc <- tbl(db, "release_locs") %>% 
    collect() %>%
    filter(.data$rel_loc %in% (rls %>% pull(rel_loc)) )
  
  dat <- tbl(db, "detection") %>%
    filter(.data$studyID == study) %>%
    collect() %>%
    left_join(.data$gen_locs %>%
                dplyr::select(.data$depID, .data$gen_loc)) %>%
    filter(!is.na(.data$gen_loc)) %>%
    mutate(detectDT_UTC = lubridate::ymd_hms(.data$detectDT_UTC))
  
  gen_loc_sub <- gen_locs %>% 
    collect() %>%
    filter(.data$depID %in% (dat %>% pull(.data$depID)) )
  
  if(is.null(start_time))
    start_time = min(dat$detectDT_UTC)
  
  if(is.null(end_time))
    end_time = max(dat$detectDT_UTC)
  

    study_tbs <- list(
      "det_pings"=dat,
      "dep_gl_tb"=gen_loc_sub,
      "rls"=rls,
      "rel_loc"=rel_loc)
    
    return(study_tbs)

}