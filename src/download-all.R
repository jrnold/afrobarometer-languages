#' ---
#' title: Download Sources
#' ---
#'
#' Download various sources for this project
#'
library("httr")

#'
#' ## Afrobarometer
#'
#' Now use merged afrobarometer
#'

AFROB_DIR <- project_path("data-raw", "external", "afrobarometer")
AFROB_FILE <- project_path("data-raw", "afrobarometer-countrylist.csv")
AFROB_URL <- "http://afrobarometer.org/sites/default/files/data/round-6/merged_r6_data_2016_36countries2.sav"


download_afrobarometer <- function(path, dst) {
  if (!dir.exists(path)) {
    dir.create(path)
  }
  dst <- file.path(path, basename(path))
  if (!file.exists(dst)) {
    message("Downloading ", path, "\n")
    resp <- GET(path, add_headers("user-agent" = "Mozilla/5.0"),
                write_disk(dst, overwrite = TRUE))
    status <- status_code(resp)
    message("Status code: ", status, "\n")
    if (status == 200) {
      message("Wrote to ", dst, "\n")
    } else {
      stop("Failed to download \n", call. = FALSE)
    }
  } else {
    message(sprintf("%s already exists; not downloading it\n", dst))
  }

}

#'
#' ## WALS
#'
#' Download the csv file of WALS available from http://wals.info/download
#'

WALS_DIR <- project_path("data-raw", "external", "wals")
WALS_CSV_URL <- "http://wals.info/static/download/wals-language.csv.zip"


download_wals <- function(path, dst) {
  if (!dir.exists(dst)) {
    dir.create(dst)
    zipfile <-  file.path(WALS_DIR, basename(path))
    writeBin(content(GET(path), as = "raw"), zipfile)
    unzip(zipfile, exdir = dst)
    file.remove(zipfile)
  } else {
    message(sprintf("Directory %s exits; Not downloading WALS files\n", dst))
  }
}


#'
#' ## Ethnologue
#'
#' Ethnologue
#'
#' Do not check these data into git since the [Terms of Use](https://www.ethnologue.com/terms-use#CodeTables)
#' prohibit redistribution.
#'
#' Documentation of these data are [here](https://www.ethnologue.com/codes/code-table-structure).
#'

ETHNOLOGUE_DIR <- project_path("data-raw", "external", "ethnologue")
ETHNOLOGUE_URL <- "https://www.ethnologue.com/codes/Language_Code_Data_20160222.zip"

download_ethnologue <- function(path, dst) {
  if (!dir.exists(dst)) {
    dir.create(dst)
    message("Downloading", path, "\n")
    zipfile  <- file.path(dst, basename(path))
    GET(path, write_disk(zipfile)
    unzip(zipfile, exdir = dst)
    file.remove(zipfile)
  } else {
    message(sprintf("Directory %s exists; Not downloading Ethnologue files\n", dst))
  }

}



#'
#' ## ISO Codes
#'
#' - 639.2 is available from the Library of Congress: http://www.loc.gov/standards/iso639-2/langhome.html
#' - 639.3 are available from the SIL: http://www-01.sil.org/iso639-3/
#'
#' SIL terms of use require it not to be redistributed.
#'

ISO_639_3_URL <- "http://www-01.sil.org/iso639-3/iso-639-3_Code_Tables_20160525.zip"
ISO_639_3_DIR <- project_path("data-raw", "external", "iso_639-3")
iso_639_3_zip_file  <-

download_iso_639_3 <- function(path, dst) {
  if (!dir.exists(dst)) {
    dir.create(dst)
    zipfile <- file.path(dst, basename(path))
    message("Downloading", path, "\n")
    GET(path, write_disk(zipfile))
    unzip(zipfile, exdir = dst)
    file.remove(zipfile)
  } else {
    message(sprintf("Directory %s exists; Not downloading ISO 639-3 files\n", ISO_639_3_DIR))
  }
}
