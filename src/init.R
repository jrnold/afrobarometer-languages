suppressPackageStartupMessages({
  library("tidyverse")
  library("rprojroot")
  library("magrittr")
  library("stringr")
  library("assertthat")
  library("rlang")
})

#'
#' Utility Functions
#'
project_path <- function(...) {
  find_rstudio_root_file(...)
}

DEBUG <- TRUE

#' Debugging Messages
debug_msg <- function(...) {
  if (DEBUG) {
    message(str_c(..., sep = "", collapse = ""))
  }
}

#' Especially note Input/Output functions
#'
#' This wraps an IO function so it will make a message about
#' what it is reading or writing.
#'
debug_io <- function(f, .msg = "Reading %s") {
  function(path, ...) {
    debug_msg(sprintf(.msg, path))
    f(path, ...)
  }
}

#' Convenience functions for reading and writing
debug_read <- function(f, ...) {
  debug_io(f, .msg = "Reading %s\n")
}

debug_write <- function(data, path, f, ...) {
  debug_io(f, .msg = "Writing %s\n")
}

read_csv <- readr::read_csv

read_tsv <- readr::read_tsv

read_sav <- haven::read_sav

read_yml <- function(path, ...) {
  yaml::yaml.load_file(path, ...)
}

write_yml <- function(data, path, ...) {
  cat(yaml::as.yaml(data, ...), file = path)
}

read_json <- function(path, ...) {
  jsonlite::fromJSON(path, ...)
}

write_json <- function(data, path, ...) {
  cat(jsonlite::toJSON(...), file = path)
}

#'
#' IO Functions for specific datasets
#'

# Reading and Writing Afrobarometer Datasets
afrobarometer_countries <- function() {
  path <- project_path("data-raw", "afrobarometer_countries.csv")
  read_csv(path,
           na = "",
           col_types = cols(round = col_character(),
                            variable = col_character(),
                            value = col_integer(),
                            name = col_character(),
                            iso_alpha3 = col_character(),
                            iso_alpha2 = col_character()))
}

#' Read afrobarometer files
afrobarometer <- function(.round) {
  paths <-
    list("r1" = list("external", "afrobarometer", "merged_r1_data.sav"),
         "r2" = list("external", "afrobarometer", "merged_r2_data.sav"),
         "r3" = list("external", "afrobarometer", "merged_r3_data.sav"),
         "r4" = list("external", "afrobarometer", "merged_r4_data.sav"),
         "r5" = list("external", "afrobarometer", "merged-round-5-data-34-countries-2011-2013-last-update-july-2015.sav"),
         "r6" = list("external", "afrobarometer", "merged_r6_data_2016_36countries2.sav")) %>%
    map_chr(function(x) invoke(project_path, x))
  read_sav(paths[[.round]])
}

misc_data <- function() {
  path <- project_path("data-raw", "misc.yml")
  read_yml(path)
}

#' Read directory with Afrobarometer yml files of mappings
.afrobarometer_yml_files <- function(dirname) {
  rounds <- str_c("r", 1:6)
  filenames <- file.path(dirname, str_c(rounds, ".yml"))
  map2(filenames, rounds,
       function(x, r) {
         map(read_yml(x), ~ c(.x, list(round = r)))
       })
}

afrobarometer_to_wals <- function() {
  .afrobarometer_yml_files(project_path("data-raw", "afrobarometer_to_wals"))
}

afrobarometer_to_iso <- function() {
  .afrobarometer_yml_files(project_path("data-raw", "afrobarometer_to_iso"))
}
