#' Download the source data for the SSP map app.
#' Pulls the ICE tables, Census crosswalks and 2010 boundaries into raw_dir.
#' Run first. Nothing else needs to run before it.
#' ================================================

library(dplyr)
library(purrr)
library(tidyr)
library(sf)
library(tigris)

source("data-raw/config.R")
source("data-raw/helper_functions/download_helpers.R")

# tigris keeps its own copy of downloaded shapefiles in a user cache folder
# outside the project, so reruns skip the Census download.
options(tigris_use_cache = TRUE)

## ===============================================
#' Tables ----
## ===============================================

# Census crosswalks ----
census_files <- tibble(
  url = unname(census_urls),
  dest = file.path(raw_dir, "census", basename(url))
)

# ICE tables (pinned to ice_commit) ----
ice_files <- expand_grid(geo = c("county", "zcta", "tract"), year = data_years) |>
  filter(geo != "zcta" | year %in% zcta_years) |>
  mutate(
    url = paste0(ice_base_url, "/ice_acs_", year, "_", geo, ".csv"),
    dest = file.path(raw_dir, "ice", basename(url))
  ) |>
  select(url, dest)

## Download and record ----
source_log <- bind_rows(census_files, ice_files) |>
  pmap(\(url, dest) download_logged(url, dest)) |>
  list_rbind()

# Compare with the committed log before overwriting it, so a changed source
# file is noticed.
check_source_log(source_log, source_log_path)
write.csv(source_log, source_log_path, row.names = FALSE)

## ===============================================
#' Boundaries ----
## ===============================================

# Each layer is saved on its own so a failed run can resume without
# redownloading. Tracts are the largest layer, so they are kept one state at a
# time and only simplified and combined in 01_prep_data.R.
dir.create(file.path(raw_dir, "boundaries"), recursive = TRUE, showWarnings = FALSE)

save_boundary <- function(path, fetch) {
  if (!file.exists(path)) {
    # EPSG:4326 (lon/lat) is what leaflet needs.
    fetch() |>
      sf::st_transform(4326) |>
      saveRDS(path)
  }
  invisible(path)
}

save_boundary(
  boundary_path("county"),
  \() tigris::counties(year = boundary_year, state = NULL, progress_bar = FALSE)
)

save_boundary(
  boundary_path("zcta"),
  \() tigris::zctas(year = boundary_year, state = NULL, progress_bar = FALSE)
)

walk(
  tract_states,
  \(st) {
    save_boundary(
      boundary_path("tract", st),
      \() tigris::tracts(state = st, year = boundary_year, progress_bar = FALSE)
    )
  },
  .progress = "Tract boundaries"
)
