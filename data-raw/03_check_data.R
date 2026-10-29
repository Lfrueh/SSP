#' Check the finished app datasets before they are deployed.
#' Needs 02_write_app_data.R to have run (or an existing out_dir). Fails on
#' problems that would break the app, and reports gaps that are only worth
#' knowing about. The report is saved in raw_dir.
#' ================================================

library(arrow)
library(dplyr)
library(purrr)
library(cli)

source("data-raw/config.R")
source("data-raw/helper_functions/check_helpers.R")

## ===============================================
#' What the app expects ----
## ===============================================

ice_cols <- c(
  "ICEincome", "ICEraceeth", "ICEhome", "ICEincwb",
  "ICEincwnh", "ICEedu", "ICElanguage"
)

# Must match the state list in app.R.
expected <- list(
  county = list(states = c(state.abb, "DC", "PR"), years = data_years),
  zcta = list(states = c(state.abb, "DC", "PR"), years = zcta_years),
  tract = list(states = c(state.abb, "DC"), years = data_years)
)

## ===============================================
#' Checks ----
## ===============================================

check_geography <- function(geo) {
  # Only IDs and states are read from the boundary files, not the geometry,
  # which keeps this fast.
  geom <- open_dataset(file.path(out_dir, paste0(geo, "_sf_partitioned"))) |>
    select(GEOID10, state.abb) |>
    collect()

  attrs <- open_dataset(file.path(out_dir, paste0(geo, "_partitioned"))) |>
    select(GEOID10, state.abb, year, all_of(ice_cols)) |>
    collect()

  exp_states <- expected[[geo]]$states
  exp_years <- expected[[geo]]$years

  ice_range <- range(unlist(attrs[ice_cols]), na.rm = TRUE)

  # Attribute rows whose area has no boundary (these never show on the map).
  no_boundary <- anti_join(attrs, geom, by = join_by(GEOID10))
  # Boundaries with no data in any year.
  no_data <- anti_join(geom, attrs, by = join_by(GEOID10))

  bind_rows(
    check_result(
      "boundary IDs are unique", geo,
      !anyDuplicated(geom$GEOID10),
      paste(sum(duplicated(geom$GEOID10)), "duplicated GEOID10")
    ),
    check_result(
      "boundary states are as expected", geo,
      setequal(unique(geom$state.abb), exp_states),
      describe_set_difference(unique(geom$state.abb), exp_states)
    ),
    check_result(
      "attribute rows are unique per area and year", geo,
      !anyDuplicated(attrs[c("GEOID10", "year")]),
      paste(sum(duplicated(attrs[c("GEOID10", "year")])), "duplicated rows")
    ),
    check_result(
      "attribute years are as expected", geo,
      setequal(unique(attrs$year), exp_years),
      describe_set_difference(unique(attrs$year), exp_years)
    ),
    check_result(
      "attribute states are as expected", geo,
      all(unique(attrs$state.abb) %in% exp_states),
      describe_set_difference(unique(attrs$state.abb), exp_states)
    ),
    check_result(
      "ICE values are between -1 and 1", geo,
      all(ice_range >= -1 - 1e-9 & ice_range <= 1 + 1e-9),
      paste("range:", round(min(ice_range), 3), "to", round(max(ice_range), 3))
    ),
    check_result(
      "attribute rows without a boundary", geo, TRUE,
      paste(nrow(no_boundary), "rows in", n_distinct(no_boundary$GEOID10), "areas"),
      severity = "info"
    ),
    check_result(
      "boundaries without any data", geo, TRUE,
      paste(nrow(no_data), "areas"),
      severity = "info"
    ),
    check_result(
      "areas on the map", geo, TRUE,
      paste(nrow(geom), "boundaries,", n_distinct(attrs$GEOID10), "areas with data"),
      severity = "info"
    )
  )
}

results <- map(names(expected), check_geography) |>
  list_rbind()

## ===============================================
#' Report ----
## ===============================================

report_path <- file.path(raw_dir, "data_check_report.csv")
dir.create(raw_dir, recursive = TRUE, showWarnings = FALSE)
write.csv(results, report_path, row.names = FALSE)

results |>
  filter(severity == "info") |>
  pwalk(\(check, geography, detail, ...) cli_alert_info("{geography}: {check}: {detail}"))

failed <- filter(results, !ok, severity == "error")

if (nrow(failed) > 0) {
  cli_abort(c(
    "{nrow(failed)} data check{?s} failed.",
    set_names(
      paste0(failed$geography, ": ", failed$check, " (", failed$detail, ")"),
      rep("x", nrow(failed))
    ),
    i = "Full report: {.path {report_path}}"
  ))
}

cli_alert_success("All {sum(results$severity == 'error')} data checks passed. Report: {.path {report_path}}")
