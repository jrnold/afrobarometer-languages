#' ---
#' title:  Download Ethnologue Data
#' ---
#'
#' Download Ethnologue language data
#'
#' Documentation of these data are [here](https://www.ethnologue.com/codes/code-table-structure).
library("httr")

ETHNOLOGUE_VERSION <- "20160222"
ETHNOLOGUE_URL <- paste0("https://www.ethnologue.com/codes/",
                         "Language_Code_Data_", ETHNOLOGUE_VERSION, ".zip")

download_ethnologue <- function(dst) {
  path <- ETHNOLOGUE_URL
  if (!dir.exists(dst)) {
    dir.create(dst)
    message("Downloading ", path, "\n")
    zipfile  <- tempfile(fileext = ".zip")
    GET(path, write_disk(zipfile))
    unzip(zipfile, exdir = dst)
    file.remove(zipfile)
    dir(dst)
  } else {
    message(sprintf("Directory %s exists; Not downloading Ethnologue files\n", dst))
  }

}
