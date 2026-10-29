#' Settings shared by the data-build scripts (00 to 03).
#' Sourced at the top of each script. Run the scripts from the project root.
#' ================================================

if (!file.exists("app.R")) {
  cli::cli_abort(c(
    "Run the data-build scripts from the project root.",
    x = "Could not find {.file app.R} in {.path {getwd()}}."
  ))
}

## ===============================================
#' Sources ----
## ===============================================

# Pinned to a commit so a rebuild gets the same ICE files every time. To update:
# pick a newer commit of the ICE folder, change the hash, rebuild, then re-run
# 03_check_data.R and review source_log.csv.
ice_repo <- "samjaros-stanford/spatial_social_polarization_database"
ice_commit <- "d26cf5e690b0cee96ba80e8701cea2e3fe607112"
ice_base_url <- paste0(
  "https://raw.githubusercontent.com/", ice_repo, "/", ice_commit, "/ICE"
)

# Census crosswalks: state/county names and ZCTA-to-county relationships (2010).
census_urls <- c(
  fips = "https://www2.census.gov/geo/docs/reference/codes/files/national_county.txt",
  zcta_rel = "https://www2.census.gov/geo/docs/maps-data/data/rel/zcta_county_rel_10.txt"
)

## ===============================================
#' What to build ----
## ===============================================

# ICE data are ACS 5-year estimates ending in these years.
data_years <- 2010:2019
# ZCTA ICE data start a year later than county and tract.
zcta_years <- 2011:2019

# All boundaries are 2010 Census vintage, to match the ACS 2010-2019 data.
boundary_year <- 2010

# Tract data exist for the 50 states and DC. There are no ICE tract data for
# Puerto Rico.
tract_states <- c(state.abb, "DC")

# Share of boundary detail kept when simplifying, so maps draw quickly.
simplify_keep <- 0.2

## ===============================================
#' Folders ----
## ===============================================

# Raw downloads and intermediate files can be large and are only needed to
# rebuild, so they live outside the app's deployed files. Override either with
# an environment variable (for example to build into a temp folder for testing).
raw_dir <- Sys.getenv("SSP_RAW_DIR", unset = "data-raw/cache")
out_dir <- Sys.getenv("SSP_OUT_DIR", unset = "data")

# Committed record of what was downloaded (urls and file hashes).
source_log_path <- "data-raw/source_log.csv"

# Where each raw boundary layer is saved. Tracts are saved one state at a time.
boundary_path <- function(layer, state = NULL) {
  file_name <- paste(c(layer, boundary_year, state), collapse = "_")
  file.path(raw_dir, "boundaries", paste0(file_name, ".rds"))
}

# 02_write_app_data.R replaces the app datasets in out_dir. Replacing is only
# allowed when this is set, so a rerun cannot silently overwrite data that is
# slow to rebuild.
overwrite_ok <- identical(Sys.getenv("SSP_OVERWRITE"), "yes")
