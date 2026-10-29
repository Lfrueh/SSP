#' Helpers for preparing boundary geometry.
#' Used by 01_prep_data.R. Packages are namespaced, so nothing needs loading.
#' ================================================

#' Simplify boundaries one state at a time
#'
#' Simplifying a state at a time keeps each job small enough for the V8 engine
#' that rmapshaper uses when Node/mapshaper are not installed (the national
#' tract layer is too big for it). The only side effect is that shared borders
#' between two states are simplified separately, which is not visible at the
#' zoom levels this app uses.
#'
#' @param boundaries sf object with a `state.abb` column.
#' @param keep Share of vertices to keep (0 to 1).
#' @return sf object with MULTIPOLYGON geometry, valid, with the same columns.
simplify_by_state <- function(boundaries, keep = 0.2) {
  boundaries |>
    split(boundaries$state.abb) |>
    lapply(
      \(state_sf) {
        rmapshaper::ms_simplify(
          state_sf,
          keep = keep,
          keep_shapes = TRUE,
          sys = FALSE
        )
      }
    ) |>
    # bind_rows() keeps the sf class; purrr::list_rbind() does not reliably.
    dplyr::bind_rows() |>
    sf::st_cast("MULTIPOLYGON") |>
    sf::st_make_valid()
}

#' Drop rows that did not match a state, and say how many
#'
#' A row without a state cannot be partitioned or selected in the app, so it is
#' dropped, but loudly, because a large number would mean a crosswalk problem.
#'
#' @param data Data frame (or sf) with a `state.abb` column.
#' @param label Short name of the table, used in the message.
#' @return `data` without the unmatched rows.
drop_no_state <- function(data, label) {
  n_missing <- sum(is.na(data$state.abb))
  if (n_missing > 0) {
    cli::cli_warn("{n_missing} {label} row{?s} had no state and {?was/were} dropped.")
  }
  dplyr::filter(data, !is.na(state.abb))
}

#' Pick the primary county of each ZCTA
#'
#' A ZCTA can span several counties. Keeping the county with most of the ZCTA's
#' population (land area breaks ties for uninhabited ZCTAs) gives every ZCTA one
#' row, so it is drawn once and counted once.
#'
#' @param zcta_rel Census ZCTA-to-county table with `GEOID10`, `statefp`,
#'   `countyfp`, `POPPT` and `AREALANDPT`.
#' @return Data frame with one row per ZCTA: `GEOID10`, `statefp`, `countyfp`.
pick_primary_county <- function(zcta_rel) {
  zcta_rel |>
    dplyr::arrange(
      GEOID10, dplyr::desc(POPPT), dplyr::desc(AREALANDPT), statefp, countyfp
    ) |>
    dplyr::slice(1, .by = GEOID10) |>
    dplyr::select(GEOID10, statefp, countyfp)
}
