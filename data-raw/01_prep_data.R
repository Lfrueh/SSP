#' Prepare the app datasets from the raw downloads.
#' Needs 00_get_data.R to have run. Joins state and county names, picks one
#' primary county per ZCTA, simplifies boundaries, and saves clean objects
#' (in raw_dir/clean) for 02_write_app_data.R to write out.
#' ================================================

library(dplyr)
library(purrr)
library(readr)
library(stringr)
library(sf)

source("data-raw/config.R")
source("data-raw/helper_functions/geometry_helpers.R")

## ===============================================
#' Crosswalks ----
## ===============================================

# Everything is read as text so codes keep their leading zeros ("01" not 1);
# only the two columns used to pick a primary county are numbers.
fips <- read_csv(
  file.path(raw_dir, "census", "national_county.txt"),
  col_names = c("state.abb", "statefp", "countyfp", "county.name", "classfp"),
  col_types = cols(.default = col_character())
) |>
  select(-classfp)

zcta_rel <- read_delim(
  file.path(raw_dir, "census", "zcta_county_rel_10.txt"),
  delim = ",",
  col_types = cols(
    .default = col_character(),
    POPPT = col_double(),
    AREALANDPT = col_double()
  )
) |>
  rename(GEOID10 = ZCTA5, statefp = STATE, countyfp = COUNTY)

zcta_primary_county <- pick_primary_county(zcta_rel)

## ===============================================
#' Boundaries ----
## ===============================================

## Counties ----
county_sf <- readRDS(boundary_path("county")) |>
  rename(statefp = STATEFP10, countyfp = COUNTYFP10) |>
  left_join(fips, by = join_by(statefp, countyfp), relationship = "many-to-one") |>
  select(GEOID10, state.abb, county.name) |>
  drop_no_state("county boundary") |>
  simplify_by_state(keep = simplify_keep)

## ZCTAs ----
# Inner joins: a ZCTA without a primary county cannot be placed in a state.
zcta_sf <- readRDS(boundary_path("zcta")) |>
  select(GEOID10) |>
  inner_join(
    zcta_primary_county,
    by = join_by(GEOID10),
    relationship = "one-to-one"
  ) |>
  inner_join(fips, by = join_by(statefp, countyfp), relationship = "many-to-one") |>
  select(GEOID10, state.abb, county.name) |>
  simplify_by_state(keep = simplify_keep)

## Tracts ----
# One state at a time, then combined: the raw national layer is too large to
# hold and simplify in one piece.
tract_sf <- map(
  tract_states,
  \(st) {
    state_tracts <- readRDS(boundary_path("tract", st)) |>
      rename(
        statefp = STATEFP10,
        countyfp = COUNTYFP10,
        tract.name = NAMELSAD10
      ) |>
      left_join(fips, by = join_by(statefp, countyfp), relationship = "many-to-one") |>
      select(GEOID10, state.abb, county.name, tract.name)

    # Guards against a state file holding another state's tracts.
    if (!all(state_tracts$state.abb == st)) {
      cli::cli_abort("Tract boundaries for {st} contain other states.")
    }
    simplify_by_state(state_tracts, keep = simplify_keep)
  },
  .progress = "Simplifying tracts"
) |>
  bind_rows()

## ===============================================
#' ICE attributes ----
## ===============================================

# ICE values are numbers between -1 and 1. GEOID stays text for leading zeros.
read_ice <- function(geo, years) {
  years |>
    set_names() |>
    map(
      \(year) {
        read_csv(
          file.path(raw_dir, "ice", paste0("ice_acs_", year, "_", geo, ".csv")),
          col_types = cols(GEOID = col_character(), .default = col_double())
        )
      }
    ) |>
    list_rbind(names_to = "year") |>
    mutate(year = as.integer(year)) |>
    rename(GEOID10 = GEOID)
}

# County and tract GEOIDs start with the state and county codes.
add_names_from_geoid <- function(ice) {
  ice |>
    mutate(
      statefp = str_sub(GEOID10, 1, 2),
      countyfp = str_sub(GEOID10, 3, 5)
    ) |>
    left_join(fips, by = join_by(statefp, countyfp), relationship = "many-to-one")
}

county_attr <- read_ice("county", data_years) |>
  add_names_from_geoid() |>
  drop_no_state("county ICE")

# Primary county only (see pick_primary_county). The Census relationship
# columns are not carried over: they were only needed to choose the county.
zcta_attr <- read_ice("zcta", zcta_years) |>
  inner_join(
    zcta_primary_county,
    by = join_by(GEOID10),
    relationship = "many-to-one"
  ) |>
  inner_join(fips, by = join_by(statefp, countyfp), relationship = "many-to-one") |>
  select(GEOID10, starts_with("ICE"), county.name, year, state.abb)

tract_attr <- read_ice("tract", data_years) |>
  add_names_from_geoid() |>
  drop_no_state("tract ICE")

## ===============================================
#' Save ----
## ===============================================

clean_dir <- file.path(raw_dir, "clean")
dir.create(clean_dir, recursive = TRUE, showWarnings = FALSE)

list(
  county_sf = county_sf,
  zcta_sf = zcta_sf,
  tract_sf = tract_sf,
  county_attr = county_attr,
  zcta_attr = zcta_attr,
  tract_attr = tract_attr
) |>
  iwalk(\(object, name) saveRDS(object, file.path(clean_dir, paste0(name, ".rds"))))
