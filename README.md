# CVhelp

Package containing helper functions for looking up environmental data, mostly for South Delta

feel free to add 'issues' for added features or errors.

## Installation
```
devtools::install_github("https://github.com/swhitCBR/CVhelp")
```
## Wrapper function
```
# only required argument is a range of dates (Note: not fully tested; do not date earlier than 1984)
dt_rng_v <- c("2011-02-13","2016-12-31")

# date range data compilation wrapper
env_data_ls <- env_comp(
  dt_rng=dt_rng_v,...)
```
## functions called by env_comp() for dealing with individual APIs
```
# lookup water year type
CDEC_wyt_class_raw <- get_WYT_data(dt_rng = dt_rng,basin="SJ")

# lookup flow,temperature, exports, x2
CDEC_env_raw <- get_CDEC_data(dt_rng=dt_rng_v)
USGS_env_raw <- get_USGS_data(dt_rng = dt_rng_v)
DAYFLOW_env_raw <- get_dayflow_data(dt_rng=dt_rng_v)
```

## Other functions

```
# used for linear interpolation of OMT values based on other measurements
calc_OMT()

```
