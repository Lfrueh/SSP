#' Rebuild the app data from scratch: download, prepare, write, check.
#' Run from the project root with `Rscript data-raw/run_all.R`.
#' See the README for the folders and environment variables this uses.
#' ================================================

library(cli)

scripts <- c(
  "00_get_data.R",
  "01_prep_data.R",
  "02_write_app_data.R",
  "03_check_data.R"
)

for (script in scripts) {
  cli_h1(script)
  started <- Sys.time()
  source(file.path("data-raw", script))
  cli_alert_success(
    "{script} finished in {round(difftime(Sys.time(), started, units = 'mins'), 1)} min"
  )
}
