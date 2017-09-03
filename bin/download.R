#' ---
#' title: Download Prerequisite Data
#' ---
#'
source("src/R/init.R")

download_file <- function(url, dst = ".", overwrite = FALSE, ...) {
  dst <- project_path(dst[1])
  if (dir.exists(dst)) {
    urlfn <- basename(urltools::url_parse(url[[1]])$path)
    outfile <- file.path(dst, urlfn)
  } else {
    outfile <- dst
  }
  dstdir <- dirname(dst)
  if (!dir.exists(dstdir)) {
    dir.create(dstdir, recursive = TRUE)
    message("Created directory ", dstdir)
  }
  if (!file.exists(outfile) || overwrite) {
    GET(url, write_disk(outfile, overwrite = TRUE), ...)
    message("Downloading ", url, " to ", outfile)
    outfile
  } else {
    message(outfile, " exists")
  }

}

files <- list(
  list(url = "http://glottolog.org/static/download/tree-glottolog-newick.txt",
       dst = "external/glottolog"),
  list(url = "http://glottolog.org/resourcemap.json?rsc=language",
       dst = "external/glottolog"),
  list(url = "http://glottolog.org/static/download/languages-and-dialects-geo.csv",
       dst = "external/glottolog"),
  list(url = "http://glottolog.org/static/download/glottolog-languoid.csv.zip",
       dst = "external/glottolog"),
  list(url = "http://wals.info/static/download/wals-language.csv.zip",
       dst = "external"),
  list(url = "http://www-01.sil.org/iso639-3/iso-639-3_Code_Tables_20170217.zip",
       dst = "external"),
  list(url = "https://www.ethnologue.com/codes/Language_Code_Data_20170221.zip",
       dst = "external"),
  list(url = "http://afrobarometer.org/sites/default/files/data/round-1/merged_r1_data.sav",
       dst = "external/afrobarometer"),
  list(url = "http://afrobarometer.org/sites/default/files/data/round-2/merged_r2_data.sav",
       dst = "external/afrobarometer"),
  list(url = "http://afrobarometer.org/sites/default/files/data/round-3/merged_r3_data.sav",
       dst = "external/afrobarometer"),
  list(url = "http://afrobarometer.org/sites/default/files/data/round-4/merged_r4_data.sav",
       dst = "external/afrobarometer"),
  list(url = "http://afrobarometer.org/sites/default/files/data/round-5/merged-round-5-data-34-countries-2011-2013-last-update-july-2015.sav",
       dst = "external/afrobarometer"),
  list(url = "http://afrobarometer.org/sites/default/files/data/round-6/merged_r6_data_2016_36countries2.sav",
       dst = "external/afrobarometer")
)

invisible(invoke_map(download_file, .x = files))
