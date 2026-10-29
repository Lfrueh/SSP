#' Helpers for checking the finished app datasets.
#' Used by 03_check_data.R. Packages are namespaced, so nothing needs loading.
#' ================================================

#' Record the result of one check
#'
#' @param check Short name of what was checked.
#' @param geography "county", "zcta" or "tract".
#' @param ok TRUE if the check passed.
#' @param detail Text explaining a failure, or giving a count for information.
#' @param severity "error" fails the run; "info" is only reported.
#' @return One-row tibble.
check_result <- function(check, geography, ok, detail = "", severity = "error") {
  tibble::tibble(
    check = check,
    geography = geography,
    ok = ok,
    severity = severity,
    detail = detail
  )
}

#' Describe the difference between two sets, for failure messages
#'
#' @param found Values found in the data.
#' @param expected Values the app expects.
#' @return Text such as "missing: DC; unexpected: XX", or "" if they match.
describe_set_difference <- function(found, expected) {
  missing <- setdiff(expected, found)
  unexpected <- setdiff(found, expected)
  parts <- c(
    if (length(missing) > 0) paste0("missing: ", paste(missing, collapse = ", ")),
    if (length(unexpected) > 0) paste0("unexpected: ", paste(unexpected, collapse = ", "))
  )
  paste(parts, collapse = "; ")
}
