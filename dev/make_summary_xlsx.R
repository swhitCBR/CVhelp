# Generates media/xlsx/CVhelp_data_sources.xlsx: data sources and compiled variables.
# Run from the package root. Formatting is done by write_summary_xlsx().
# Units are partly inferred from the code and should be checked.

devtools::load_all(quiet = TRUE)

sources <- data.frame(
  Function = c("get_CDEC_data", "get_USGS_data", "get_WYT_data",
               "get_CNRA_dayflow_by_year / get_dayflow_data", "get_HOR_barrier_data", "env_comp"),
  Exported = c("No", "No", "No", "Yes", "Yes", "Yes"),
  Source = c(
    "CDEC CSV data servlet (cdec.water.ca.gov)",
    "USGS Water Data API (dataRetrieval::read_waterdata_daily)",
    "CDEC water year type report page (WSIHIST)",
    "CNRA open-data portal (data.cnra.ca.gov) Dayflow datastore CSVs, 1985-2025",
    "None live; hard-coded Head of Old River barrier schedule, 1992-2018",
    "Orchestrator calling all of the above"),
  Method = c(
    "Pasted URL + read.csv; F to C conversion", "lapply over sites",
    "rvest scrape of <pre> block + string parsing",
    "Hard-coded resource URLs per year range + read.csv", "Hard-coded table",
    "Joins, imputation, moving averages, log transforms"),
  Defaults = c(
    "Stations CLC, MSD; sensors 25, 146; daily",
    "Sites USGS-11303500, USGS-11313405, USGS-11312676; params 00060, 72137, 72137",
    "Basin SAC or SJ", "Years 1985-2025 only", "Date range",
    "dt_rng 2011-01-01 to 2016-12-31; basin SJ; DOY 1:250"),
  Credentials = "None",
  Output = c("Data frame (wide/long/list)", "Named list of data frames",
             "Data frame (Year, ind, wyt, WYT)", "Data frame (SWP, CVP, EXPORT(S), OUT, X2)",
             "Daily TRUE/FALSE data frame", "Data frame or list (wide/long/list)"))

variables <- data.frame(
  Variable = c("date", "Year", "DOY", "WYT", "VNS", "ORB", "MID", "OMT", "MSD", "CLC",
               "barrierTF", "SWP", "CVP", "OUT", "EXPORTS", "X2"),
  Description = c(
    "Calendar date", "Calendar year", "Day of year", "Water year type (basin set by `basin`)",
    "San Joaquin R. near Vernalis discharge",
    "Old River at Bacon Island discharge (tidally filtered)",
    "Middle River at Middle River discharge (tidally filtered)", "ORB + MID (derived)",
    "Water temperature, station MSD", "Water temperature, station CLC",
    "Head of Old River barrier in place", "State Water Project exports",
    "Central Valley Project exports", "Net Delta outflow index", "Total Delta exports",
    "X2 position"),
  Source = c("Derived", "Derived", "Derived", "CDEC WSIHIST", "USGS", "USGS", "USGS",
             "Derived from USGS", "CDEC", "CDEC", "Hard-coded schedule", "CNRA Dayflow",
             "CNRA Dayflow", "CNRA Dayflow", "CNRA Dayflow", "CNRA Dayflow"),
  Identifier = c(NA, NA, NA, "WSIHIST", "USGS-11303500 (param 00060)", "USGS-11313405 (param 72137)",
                 "USGS-11312676 (param 72137)", "ORB + MID", "CDEC MSD, sensors 25/146",
                 "CDEC CLC, sensors 25/146", "Internal table", "Dayflow SWP", "Dayflow CVP",
                 "Dayflow OUT", "Dayflow EXPORT(S)", "Dayflow X2"),
  Units = c(NA, NA, NA, "Category", "cfs", "cfs", "cfs", "cfs", "deg C", "deg C", "TRUE/FALSE",
            "cfs", "cfs", "cfs", "cfs", "km"),
  Processing = c(NA, NA, NA, "Joined by Year",
                 "Log transform (default); moving-average substitution (default)",
                 "Imputation (default)", "Imputation (default)", "ORB + MID; imputation (default)",
                 "Values <3 or >30 set to NA; imputation and moving-average substitution (default)",
                 "Imputation and moving-average substitution (default)", NA,
                 "Moving-average substitution (default)", "Moving-average substitution (default)",
                 "Log transform (default)", NA, NA),
  Notes = c(NA, NA, NA, NA, NA, NA, NA, NA, "Data start about 2010-10-10",
            "Data start about 2009-10-29", "Schedule ends 2018", NA, NA, NA,
            "Dayflow field name is EXPORT before 1997", "Not available 1985-1995"))

write_summary_xlsx(
  list("Data sources" = sources, "Variables" = variables),
  file = "media/xlsx/CVhelp_data_sources.xlsx",
  overwrite = TRUE)
