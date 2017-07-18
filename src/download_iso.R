#' ---
#' title: Download ISO 639 Codes
#' ---
#'
#' - 639.2 is available from the Library of Congress: http://www.loc.gov/standards/iso639-2/langhome.html
#' - 639.3 are available from the SIL: http://www-01.sil.org/iso639-3/
#'
library("rprojroot")
library("httr")

OUTPUT <- find_rstudio_root_file("external", "iso-639-3")

# URL of the current version of
ISO_639_3_VERSION <- "20160525"
ISO_639_3_URL <- paste0("http://www-01.sil.org/iso639-3/",
                        "iso-639-3_Code_Tables_",
                        ISO_639_3_VERSION, ".zip")

download_iso_639_3 <- function(dst) {
  path <- ISO_639_3_URL
  if (!dir.exists(dst)) {
    dir.create(dst, recursive = TRUE)
  }
  dst_dir <- file.path(dst, basename(tools::file_path_sans_ext(path)))
  if (!dir.exists(dst_dir)) {
    zipfile <- tempfile(fileext = ".zip")
    message("Downloading ", path, "\n")
    GET(path, write_disk(zipfile))
    unzip(zipfile, exdir = dst)
    file.remove(zipfile)
    dir(dst_dir)
  } else {
    message(sprintf("Directory %s exists; Not downloading ISO 639-3 files\n", dst_dir))
  }
}

download_iso_639_3(OUTPUT)
