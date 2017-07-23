#' ---
#' title: Download Ethnologue Code Tables
#' ---
#'
library("rprojroot")
library("httr")

OUTPUT <- find_rstudio_root_file("external", "ethnologue")

# URL of the current version of
ETHNOLOGUE_URL <- "https://www.ethnologue.com/codes/Language_Code_Data_20170221.zip"

download_ethnologue <- function(dst) {
  path <- ETHNOLOGUE_URL
  if (!dir.exists(dst)) {
    dir.create(dst, recursive = TRUE)
  }
  dst_dir <- file.path(dst, basename(tools::file_path_sans_ext(path)))
  if (!dir.exists(dst_dir)) {
    zipfile <- tempfile(fileext = ".zip")
    message("Downloading ", path, "\n")
    GET(path, write_disk(zipfile))
    unzip(zipfile, exdir = dst_dir)
    file.remove(zipfile)
    dir(dst_dir)
  } else {
    message(sprintf("Directory %s exists; Not downloading Ethnologue files\n", dst_dir))
  }
}

download_ethnologue(OUTPUT)
