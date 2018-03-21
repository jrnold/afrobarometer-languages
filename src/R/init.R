suppressPackageStartupMessages({
  library("tidyverse")
  library("rprojroot")
  library("magrittr")
  library("stringr")
  library("assertthat")
  library("rlang")
  library("httr")
  library("yaml")
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

IO <- rlang::env()

.afrobarometer <- function(.round) {
  paths <-
    list("r1" = list("external", "afrobarometer", "merged_r1_data.sav"),
         "r2" = list("external", "afrobarometer", "merged_r2_data.sav"),
         "r3" = list("external", "afrobarometer", "merged_r3_data.sav"),
         "r4" = list("external", "afrobarometer", "merged_r4_data.sav"),
         "r5" = list("external", "afrobarometer",
                     "merged-round-5-data-34-countries-2011-2013-last-update-july-2015.sav"),
         "r6" = list("external", "afrobarometer",
                     "merged_r6_data_2016_36countries2.sav")) %>%
    map_chr(function(x) invoke(project_path, x))
  read_sav(paths[[str_c("r", .round)]])
}
env_bind(IO, afrobarometer = .afrobarometer)

# Raw Datasets
env_bind_fns(IO,

   # name _raw to distinguish from data/afrobarometer_variables
   afrobarometer_variables_raw = function() {
     read_csv(here::here("data-raw", "afrobarometer_variables.csv"), na = "",
              col_types = cols(
                round = col_integer(),
                respno = col_character(),
                country = col_character(),
                withinwt = col_character(),
                combinwt = col_character(),
                respondent = col_character(),
                respondent_other = col_character(),
                interview = col_character(),
                interview_other = col_character(),
                interviewer = col_character(),
                interviewer_other = col_character(),
                district = col_character(),
                region = col_character()
              ))
   },

   #' return a data frame with columns
   #'
   #' - iso_alpha2
   #' - iso_alpha3
   #' - name
   countries_raw = function() {
     yaml.load_file(here::here("data-raw", "countries.yml")) %>%
       map_df(cross_df)
   },

   #' Language names -> linguistic identifiers
   languages_raw = function() {
     yaml.load_file(here::here("data-raw", "languages.yml"))
   }
)

# New Datasets
env_bind_fns(IO,

   afrobarometer_variables = function() {
     read_csv(here::here("data", "afrobarometer_variables.csv"), na = "",
              col_types = cols(
                round = col_integer(),
                respno = col_character(),
                country = col_character(),
                withinwt = col_character(),
                combinwt = col_character(),
                respondent = col_character(),
                respondent_other = col_character(),
                interview = col_character(),
                interview_other = col_character(),
                interviewer = col_character(),
                interviewer_other = col_character(),
                district = col_character(),
                region = col_character()
              ))
   },

   languages = function() {
     read_csv(here::here("data", "afrobarometer_variables.csv"), na = "",
              col_types = cols(
                round = col_integer(),
                respno = col_character(),
                country = col_character(),
                withinwt = col_character(),
                combinwt = col_character(),
                respondent = col_character(),
                respondent_other = col_character(),
                interview = col_character(),
                interview_other = col_character(),
                interviewer = col_character(),
                interviewer_other = col_character()
              ))
   },

   countries = function() {
     read_csv(here::here("data", "countries.csv"), na = "",
              col_types = cols(
                round = col_integer(),
                value = col_integer(),
                label = col_character(),
                iso_alpha2 = col_character()
              ))
   },

   language_names = function() {
     read_csv(here::here("data", "language_names.csv"), na = "",
              col_types = cols(
                name = col_character(),
                country = col_character(),
                iso_639_3 = col_character(),
                glottocode = col_character(),
                wals = col_character()
              )) %>%
       mutate(name = if_else(is.na(name), "", name))
   },

   languages_respno = function() {
     read_csv(here::here("data", "languages_respno.csv"), na = "",
              col_types = cols(
                 round = col_integer(),
                 variable = col_character(),
                 respno = col_character(),
                 country = col_character(),
                 lang_name = col_character(),
                 is_other = col_logical(),
                 iso_639_3 = col_character(),
                 glottocode = col_character(),
                 wals = col_character(),
                 district = col_character(),
                 region = col_character(),
                 withinwt = col_double()
               ))
   },



)

IO$glottolog <- src_sqlite(here::here("external", "lingdata", "glottolog.db"))

IO$iso_639_3 <- src_sqlite(here::here("external", "lingdata", "iso_639_3.db"))

IO$ethnologue <- src_sqlite(here::here("external", "lingdata", "ethnologue.db"))


#' Misc functions
#'
#'
#'  Set equality
seteq <- function(x, y) {
  !length(setdiff(x, y)) && !length(setdiff(y, x))
}


ethnologue_url <- function(id) {
  str_c("https://www.ethnologue.com/language/", id)
}

iso_639_url <- function(id) {
  str_c("http://www-01.sil.org/iso639-3/documentation.asp?id=", id)
}

glottolog_url <- function(id) {
  str_c("http://glottolog.org/resource/languoid/id/", id)
}

wals_url <- function(id, type="language") {
  switch(type,
    language = str_c("http://wals.info/languoid/lect/wals_code_", id),
    genus = str_c("http://wals.info/languoid/genus/", id),
    family = str_c("http://wals.info/languoid/family/", id)
  )

}

# fct_set_na <- function(f, na_values = character(), na_default = "(Missing)") {
#   out <- fct_explicit_na(f, na_level)
#   attr(fct_with_na, "na_values") <- na_values
#   structure(f, class = c("factor_na", "factor"))
# }
#
# # Method to remove NA values.
# # - factor
# # - fct_with_na
# fct_rm_na <- function(f, ...) {
#
# }
