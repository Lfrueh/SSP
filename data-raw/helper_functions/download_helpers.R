#' Helpers for downloading source files and recording what was downloaded.
#' Used by 00_get_data.R. Packages are namespaced, so nothing needs loading.
#' ================================================

#' Download one file (skipped if it is already there) and describe it
#'
#' Skipping existing files makes reruns cheap and lets you resume after a
#' failed run. Delete the cache folder to force a fresh download.
#'
#' @param url Where to download from.
#' @param dest Local path to save to.
#' @return One-row data frame with the url, file path, md5 hash and date.
download_logged <- function(url, dest) {
  dir.create(dirname(dest), recursive = TRUE, showWarnings = FALSE)

  if (!file.exists(dest)) {
    status <- utils::download.file(url, dest, mode = "wb", quiet = TRUE)
    if (status != 0) {
      cli::cli_abort("Download failed for {.url {url}}.")
    }
  }

  data.frame(
    url = url,
    file = basename(dest),
    md5 = unname(tools::md5sum(dest)),
    stringsAsFactors = FALSE
  )
}

#' Compare new downloads with the committed source log
#'
#' A changed hash means the source file is not the one the committed data were
#' built from, so the data may differ. This warns instead of failing, because a
#' deliberate update (new pinned commit) will also change hashes.
#'
#' @param new_log Data frame from `download_logged()` rows bound together.
#' @param log_path Path to the committed log (may not exist yet).
#' @return Invisibly, the rows whose hash changed.
check_source_log <- function(new_log, log_path) {
  if (!file.exists(log_path)) {
    cli::cli_inform("No earlier source log at {.path {log_path}}; nothing to compare.")
    return(invisible(new_log[0, ]))
  }

  old_log <- utils::read.csv(log_path, stringsAsFactors = FALSE)
  merged <- merge(new_log, old_log, by = c("url", "file"), suffixes = c("", "_old"))
  changed <- merged[merged$md5 != merged$md5_old, ]

  if (nrow(changed) > 0) {
    cli::cli_warn(c(
      "{nrow(changed)} source file{?s} changed since the committed log.",
      i = "Changed: {.file {changed$file}}",
      i = "Rebuilt data may differ from the committed data."
    ))
  }
  invisible(changed)
}
