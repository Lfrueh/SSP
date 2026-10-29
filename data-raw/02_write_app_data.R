#' Write the datasets the Shiny app reads.
#' Needs 01_prep_data.R to have run. Writes parquet files to out_dir:
#' attributes partitioned by year and state, boundaries partitioned by state.
#' ================================================

library(arrow)
library(sfarrow)
library(dplyr)
library(purrr)

source("data-raw/config.R")

## ===============================================
#' Guard against overwriting ----
## ===============================================

geographies <- c("county", "zcta", "tract")

attr_dir <- \(geo) file.path(out_dir, paste0(geo, "_partitioned"))
geom_dir <- \(geo) file.path(out_dir, paste0(geo, "_sf_partitioned"))

existing <- c(attr_dir(geographies), geom_dir(geographies))
existing <- existing[dir.exists(existing)]

if (length(existing) > 0 && !overwrite_ok) {
  cli::cli_abort(c(
    "App data already exist in {.path {out_dir}}.",
    i = "Set {.code SSP_OVERWRITE=yes} to replace them, or set {.code SSP_OUT_DIR} to build somewhere else.",
    i = "Existing: {.path {existing}}"
  ))
}

## ===============================================
#' Write ----
## ===============================================

read_clean <- \(name) readRDS(file.path(raw_dir, "clean", paste0(name, ".rds")))

walk(
  geographies,
  \(geo) {
    unlink(c(attr_dir(geo), geom_dir(geo)), recursive = TRUE)

    # Partitioning by year and state keeps each app query to a few small files.
    read_clean(paste0(geo, "_attr")) |>
      write_dataset(attr_dir(geo), partitioning = c("year", "state.abb"))

    # Boundaries do not change with year, so they are partitioned by state only.
    sfarrow::write_sf_dataset(
      read_clean(paste0(geo, "_sf")),
      geom_dir(geo),
      partitioning = "state.abb"
    )
  },
  .progress = "Writing app data"
)
