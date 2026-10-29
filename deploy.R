#' Deploy the app to shinyapps.io.
#' Uploads only the files the app needs, listed explicitly, so data-raw/, its
#' cache, the review notes and other project files are never sent. Needs the
#' data/ folder (see data-raw/) and CARTO_API_KEY set in this R session.
#' Run from the project root.
#' ================================================

## ===============================================
#' Settings ----
## ===============================================

# Confirm these before deploying. They come from rsconnect/ (account
# lisa-frueh, app ssp_maps). The header links to the drexel-uhc account.
account <- "lisa-frueh"
app_name <- "ssp_maps"

# Everything app.R reads at run time.
app_files <- c("app.R", "CSS", "HTML", "www", "data", "renv.lock")

## ===============================================
#' Checks ----
## ===============================================

missing_files <- setdiff(app_files, list.files(all.files = TRUE, no.. = TRUE))
if (length(missing_files) > 0) {
  cli::cli_abort(c(
    "Cannot deploy: files are missing from the project root.",
    x = "Missing: {.file {missing_files}}",
    i = "Run this from the project root, and rebuild data/ with data-raw/run_all.R if needed."
  ))
}

if (!nzchar(Sys.getenv("CARTO_API_KEY"))) {
  cli::cli_abort(c(
    "CARTO_API_KEY is not set, so the deployed map would show watermarked tiles.",
    i = "Put it in .Renviron and restart R."
  ))
}

## ===============================================
#' Deploy ----
## ===============================================

# envVars forwards the key from this session to the deployed app without
# writing it to any file. (Not tested here; if the key does not reach the
# app, check the rsconnect documentation for your version.)
rsconnect::deployApp(
  appDir = ".",
  appName = app_name,
  account = account,
  appFiles = app_files,
  envVars = "CARTO_API_KEY"
)
