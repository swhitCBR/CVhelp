
devtools::load_all()

get_HOR_barrier_data()

get_barrier_strt_stop()

library(waterYearType)
# WaterYearType
# str(waterYearType)

# source("https://raw.github.com/Columbia-Basin-Research-West/predator_filter_package/R/get_HOR_schedule.R")
# 
# https://github.com/Columbia-Basin-Research-West/predator_filter_package
# 
# source("https://github.com/Columbia-Basin-Research-West/predator_filter_package/raw/refs/heads/main/R/get_HOR_schedule.R")
# 
# devtools::source_url("https://github.com/Columbia-Basin-Research-West/predator_filter_package/raw/refs/heads/main/R/get_HOR_schedule.R?raw=TRUE")
# 
# 
# source("https://github.com/Columbia-Basin-Research-West/predator_filter_package/blob/f383e562133b558835ad598fb8409c96b2449f57/R/get_HOR_schedule.R?raw=TRUE")
# 
# devtools::source_url("https://github.com/Columbia-Basin-Research-West/predator_filter_package/blob/f383e562133b558835ad598fb8409c96b2449f57/R/get_HOR_schedule.R?raw=TRUE")


DF_tmp <- env_comp(dt_rng=c("2011-01-01","2016-12-31"),
                   DOY_rng=1:250)

DF_tmp <- env_comp(dt_rng=c("2011-01-01","2016-12-31"),
                   DOY_rng=1:365)


DF_tmp <- env_comp(dt_rng=c("2011-01-01","2025-12-31"),
                   DOY_rng=1:250)
# spring HOR barrier not installed after 2018 (run `?get_HOR_barrier_data()` for details)
# daily HOR barrier status beyond 2019-01-01 set to FALSE
# 458 missing values found (12%)
# imputed values for: ORB, MID, OMT, MSD, CLC
# Error in imputeTS::na_ma(DF_w_in[, imp_cols[ii]], weighting = "simple",  : 
#                            Input x is not numeric.
#                          In addition: 
#                            Warning message:
#                            Values from `value` are not uniquely identified; output will contain list-cols.
#                          • Use `values_fn = list` to suppress this warning.
#                          • Use `values_fn = {summary_fun}` to summarise duplicates.
#                          • Use the following dplyr code to identify duplicates.
#                          {data} |>
#                            dplyr::summarise(n = dplyr::n(), .by = c(date, DOY, site)) |>
#                            dplyr::filter(n > 1L) 

DF_tmp <- env_comp(dt_rng=c("2010-01-01","2025-12-31"),
                   DOY_rng=1:250)
# spring HOR barrier not installed after 2018 (run `?get_HOR_barrier_data()` for details)
# daily HOR barrier status beyond 2019-01-01 set to FALSE
# 458 missing values found (12%)
# imputed values for: ORB, MID, OMT, MSD, CLC
# Error in imputeTS::na_ma(DF_w_in[, imp_cols[ii]], weighting = "simple",  : 
#                            Input x is not numeric.
#                          In addition: 
#                            Warning message:
#                            Values from `value` are not uniquely identified; output will contain list-cols.
#                          • Use `values_fn = list` to suppress this warning.
#                          • Use `values_fn = {summary_fun}` to summarise duplicates.
#                          • Use the following dplyr code to identify duplicates.
#                          {data} |>
#                            dplyr::summarise(n = dplyr::n(), .by = c(date, DOY, site)) |>
#                            dplyr::filter(n > 1L) 




ann_HORbar_WYT_data <- CVhelp::get_ann_HORbar_WYT_data()
CVhelp_dat_l <- CVhelp::env_comp(dt_rng=c("2011-01-01","2024-12-31"),output = "long")
CVhelp_dat_w <- CVhelp::env_comp(dt_rng=c("2011-01-01","2024-12-31"),output = "wide")



CVhelp::get_env_plot(CVhelp_dat_l)
