# Generates media/svg/CVhelp_data_flow.svg: default env_comp() compilation flow.
# Run from the package root. Layout is specified here; drawing is done by
# build_pipeline_svg() in R/build_pipeline_svg.R.

devtools::load_all(quiet = TRUE)

node <- function(id, type, x, y, w, h, title, lines, dashed = FALSE) {
  data.frame(id, type, x, y, w, h, title, lines, dashed)
}

nodes <- rbind(
  # external sources
  node("s_cdec", "src", 20, 90, 220, 80, "CDEC CSV servlet", "stations CLC, MSD|sensors 25 / 146, daily"),
  node("s_usgs", "src", 20, 215, 220, 80, "USGS Water Data API", "11303500 (VNS, 00060)|11313405 (ORB), 11312676 (MID), 72137"),
  node("s_wyt", "src", 20, 340, 220, 80, "CDEC WSIHIST page", "water year type report|(HTML, scraped)"),
  node("s_dayflow", "src", 20, 465, 220, 80, "CNRA Dayflow CSVs", "data.cnra.ca.gov datastore|hard-coded URLs, 1985-2025"),
  node("s_hor", "src", 20, 590, 220, 80, "HOR barrier schedule", "hard-coded table in package|(no live download, 1992-2018)", dashed = TRUE),

  # retrieval functions
  node("f_cdec", "fn", 290, 90, 230, 80, "get_CDEC_data()", "read.csv; DEG_F to DEG_C|(fah_to_cel)"),
  node("f_usgs", "fn", 290, 215, 230, 80, "get_USGS_data()", "dataRetrieval::|read_waterdata_daily, per site"),
  node("f_wyt", "fn", 290, 340, 230, 80, "get_WYT_data()", "rvest + string parsing|(basin SJ default)"),
  node("f_dayflow", "fn", 290, 465, 230, 80, "get_dayflow_data()", "loops years, calls|get_CNRA_dayflow_by_year()|then date filter"),
  node("f_hor", "fn", 290, 590, 230, 80, "get_HOR_barrier_data()", "get_barrier_strt_stop()|expands to daily TRUE/FALSE"),

  # raw objects
  node("o_cdec", "obj", 570, 90, 210, 80, "CDEC_env_raw", "wide: date, DOY, CLC, MSD|(deg C)"),
  node("o_usgs", "obj", 570, 215, 210, 80, "USGS_env_raw", "list of data frames:|VNS, ORB, MID (date, value)"),
  node("o_wyt", "obj", 570, 340, 210, 80, "CDEC_wyt_class_raw", "Year, ind, wyt, WYT"),
  node("o_dayflow", "obj", 570, 465, 210, 80, "get_dayflow_raw", "date, SWP, CVP, OUT,|EXPORTS, X2"),
  node("o_hor", "obj", 570, 590, 210, 80, "HOR_barrier_raw", "date, barrierTF"),

  # assembly
  node("a_grid", "obj", 835, 90, 240, 75, "full_dt_arr", "daily date grid from dt_rng|+ Year, DOY; filter DOY_rng (1:250)"),
  node("a_join", "obj", 835, 260, 240, 140, "envDat_w (joined)",
       paste("left_join onto full_dt_arr", "by Year (WYT) or date (rest)", "OMT = ORB + MID",
             "MSD outside 3-30 set to NA", "cols: WYT, VNS, ORB, MID, OMT, MSD,",
             "CLC, barrierTF, SWP, CVP, OUT, EXPORTS, X2", sep = "|")),
  node("a_miss", "obj", 835, 470, 240, 95, "missing_env_raw / _summ",
       "NA locations and per-site,|per-year counts; message of|% missing (before imputation)"),

  # post-processing
  node("p_imp", "fn", 1140, 90, 230, 95, "impute_daily_vals()",
       "NAs in ORB, MID, OMT, MSD, CLC|filled with 3-day centered MA|(imputeTS::na_ma); if impute_daily_vals=TRUE"),
  node("p_log", "fn", 1140, 225, 230, 75, "log transform", "log() of VNS and OUT|(log_trans default)"),
  node("p_ma", "fn", 1140, 340, 230, 95, "sub_MA_data()",
       "centered MA replaces values:|CLC 3d, VNS 5d, SWP 5d,|CVP 5d, MSD 7d; edges filled; if sub_MA_data=TRUE"),
  node("r_wide", "out", 1140, 480, 230, 75, "envDat_w (default return)", 'output = "wide"|daily wide data frame'),
  node("r_long", "out", 1140, 590, 230, 55, "envDat_l", 'output = "long" (pivot_longer)'),
  node("r_list", "out", 1140, 665, 230, 75, 'output = "list"',
       "full_dt_arr, envDat_w, envDat_l,|missing_env_summ, missing_env_raw")
)

pairs <- function(from, to) data.frame(from, to)
edges <- rbind(
  pairs(paste0("s_", c("cdec", "usgs", "wyt", "dayflow", "hor")),
        paste0("f_", c("cdec", "usgs", "wyt", "dayflow", "hor"))),
  pairs(paste0("f_", c("cdec", "usgs", "wyt", "dayflow", "hor")),
        paste0("o_", c("cdec", "usgs", "wyt", "dayflow", "hor")))
)
edges$from_y <- edges$to_y <- NA_real_

into_join <- data.frame(
  from = paste0("o_", c("cdec", "usgs", "wyt", "dayflow", "hor")),
  to = "a_join", from_y = NA_real_, to_y = c(300, 320, 340, 360, 380))
edges <- rbind(
  edges, into_join,
  data.frame(from = c("a_grid", "a_join", "p_imp", "p_log", "p_ma", "r_wide", "r_long"),
             to = c("a_join", "a_miss", "p_log", "p_ma", "r_wide", "r_long", "r_list"),
             from_y = NA_real_, to_y = NA_real_),
  data.frame(from = "a_join", to = "p_imp", from_y = 300, to_y = NA_real_)
)
# vertical joins need same-column nodes; a_join/a_miss/a_grid share a column
# (the p_* chain and r_* nodes do too), so only the cross-column links above use curves.

build_pipeline_svg(
  nodes, edges,
  title = "CVhelp: default env_comp() data compilation",
  headers = data.frame(
    x = c(130, 405, 675, 955, 1255), y = 60,
    label = c("External sources", "Retrieval functions", "Raw objects",
              "Assembly (in env_comp)", "Post-processing")),
  groups = data.frame(x = 820, y = 72, w = 270, h = 640),
  footnote = paste("Order in env_comp(): fetch -> join -> NA summary -> MSD QC -> impute ->",
                   "log transform -> moving-average substitution -> reshape per output."),
  file = "media/svg/CVhelp_data_flow.svg")
