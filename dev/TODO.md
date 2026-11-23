# TODO

## Major

- [ ] Review `dev/CVPASapp_env_comp_and_plotting_calls.R` and identify why one of the facets is all NA (check the variable, source function, join keys, date range/DOY filter, and any log transform or imputation applied to that variable).
- [ ] Update the existing vignette (`vignettes/generate_datasets_for_CVPASapp.Rmd`) so it demonstrates the different dependent functions used in `dev/CVPASapp_env_comp_and_plotting_calls.R`.
- [ ] Add a `dev/` script that recreates the daily environmental data from CVPAS-Steelhead (compare against `env_comp()` output).

## Minor: fixes

### Data retrieval
- [ ] Add error handling (`tryCatch`, retries, status checks) around the CDEC, USGS and CNRA network calls; fail with clear messages on outages or schema changes.
- [ ] Make `get_WYT_data()` less fragile: it matches exact hard-coded text lines, assumes `\r` line endings and a hard-coded end year (2024). Consider the `waterYearType` package; remove stray no-op `html` and `gsub` lines.
- [ ] Dayflow: replace hard-coded resource URLs and the 1985-2025 limit with a lookup of current CNRA resource IDs; remove the 2023/2024 special cases and the `yr_chk = 1 + (x == 2024)` date-format switch; cache yearly downloads instead of re-downloading each call.
- [ ] `get_CNRA_dayflow_by_year()`: the year check uses `|` where `&` is probably intended; `yr_in == c(1996)` comparison is sloppy; `get_dayflow_data()` passes `yr_in` as a character.
- [ ] Head of Old River barrier schedule is hard-coded and ends in 2018; make it updatable and fix inconsistent cutoff messages (2018, 2019-01-01, 2023-10-29).
- [ ] `seq.Date()` is called on character `dt_rng` in `env_comp()` and `get_dayflow_data()`; convert with `as.Date()` (fails on current R).
- [ ] `HOR_CHP_comp`-style relative `.RData` paths: none remain in the package, but check `CVPAS_STH_app/model_fitting` for the same problem.

### Processing
- [ ] `env_comp()` accepts a `url` argument but does not pass it to `get_WYT_data()`.
- [ ] Moving-average substitution runs after the log transform: VNS is smoothed on the log scale, SWP and CVP on the raw scale. Confirm this is intended and document it.
- [ ] Check ORB, MID and OMT: values are mostly negative (median about -1165, -1847, -3040). Confirm sign convention and whether the USGS parameters are tidally filtered.
- [ ] The MSD quality control (values outside 3-30 set to NA) runs after the missing-value summary, so those values are not counted in `missing_env_summ`.

### Cleanup
- [ ] Remove duplicated verbose / non-verbose branches in `get_USGS_data()` and `get_CDEC_data()`; remove the stray `paste0()` call in `get_CDEC_data()`.
- [ ] Deduplicate default site and sensor vectors across functions.
- [ ] `scale_data_cols()` is no longer called anywhere in the package; decide whether to keep, move or document it.

## Minor: additions

- [ ] Add `testthat` tests, starting with `impute_daily_vals()` (no-NA and NA cases), `sub_MA_data()` and `fah_to_cel()`.
- [ ] Add `rlang` to Imports (`.data$` is used).
- [ ] Replace the placeholder author in `DESCRIPTION` (`First Last`, `first.last@example.com`).
- [ ] Replace boilerplate roxygen `@return` text and `Title` placeholders; document `@noRd` functions that the vignette calls via `:::` (or export them).
- [ ] Reconcile `RoxygenNote` (7.3.3) with `Config/roxygen2/version` (8.1.1) in `DESCRIPTION`.
- [ ] Add the process summary and `media/svg/CVhelp_data_flow.svg` to the README.
- [ ] Check units and descriptions in `media/xlsx/CVhelp_data_sources.xlsx` (cfs/km/deg C and which CDEC sensor applies to each station were inferred); note that VNS and OUT are log-transformed by default and that ORB, MID, OMT are mostly negative.
- [ ] Regenerate `media/` files with the `dev/` scripts and compare the new SVG visually with the hand-drawn original.
- [ ] Decide whether `dev/` scripts should be part of a documented workflow (for example a short `dev/README`).
