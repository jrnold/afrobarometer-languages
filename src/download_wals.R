#' ---
#' title: Download WALS data
#' ---
library("httr")
library("rprojroot")

OUTPUT <- find_rstudio_root_file("external", "wals")

WALS_CSV_URL <- "http://wals.info/static/download/wals-language.csv.zip"

#' Download WALS Data
#'
#' @param dst string. Directory to save the file to.
#' @return character vector. Contents of the destination directory.
download_wals <- function(dst) {
  path <- WALS_CSV_URL
  if (!dir.exists(dst)) {
    dir.create(dst, recursive = TRUE)
    zipfile <- tempfile(fileext = ".zip")
    writeBin(content(GET(path), as = "raw"), zipfile)
    unzip(zipfile, exdir = dst)
    file.remove(zipfile)
    dir(dst)
  } else {
    message(sprintf("Directory %s exits; Not downloading WALS files\n", dst))
  }
}

download_wals(OUTPUT)
