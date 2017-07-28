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

IO <- rlang::env()

# Reading and Writing Afrobarometer Datasets
env_bind_fns(IO,

  afrobarometer_countries = function() {
    path <- project_path("data-raw", "afrobarometer_countries.csv")
    read_csv(path,
             na = "",
             col_types = cols(round = col_character(),
                              variable = col_character(),
                              value = col_integer(),
                              name = col_character(),
                              iso_alpha3 = col_character(),
                              iso_alpha2 = col_character()))
  },



  misc_data = function() {
    path <- project_path("data-raw", "misc.yml")
    read_yml(path)
  },

  afrobarometer_mappings = function() {
    rounds <- str_c("r", 1:6)
    dirname <- project_path("data-raw", "afrobarometer-mappings")
    filenames <- file.path(dirname, str_c(rounds, ".yml"))
    map2(filenames, rounds,
         function(x, r) {
           print(x)
           out <- read_yml(x)
           map(out, ~ c(.x, list(round = r)))
         }) %>%
      purrr::flatten()
  },

  afrobarometer_other_mappings = function() {
    path <- project_path("data-raw", "afrobarometer_other_mappings.yml")
    read_yml(path)
  },

  # ISO Data
  iso_639_3_codes = function() {
    path <- project_path("external",
                         "iso-639-3",
                         "iso-639-3_Code_Tables_20170217",
                         "iso-639-3_20170202.tab")
    col_types = cols(
      Id = col_character(),
      Part2B = col_character(),
      Part2T = col_character(),
      Part1 = col_character(),
      Scope = col_character(),
      Language_Type = col_character(),
      Ref_Name = col_character(),
      Comment = col_character()
    )
    read_tsv(path, na = "", col_types = col_types)
  },

  iso_639_3_macrolanguages = function() {
    path <- project_path("external",
                        "iso-639-3",
                        "iso-639-3_Code_Tables_20170217",
                        "iso-639-3-macrolanguages_20170131.tab")
    col_types <- cols(
      M_Id = col_character(),
      I_Id = col_character(),
      I_Status = col_character()
    )
    read_tsv(path, na = "", col_types = col_types)
  },

  # Ethnologue data
  ethnologue = function() {
    path <- project_path("external", "ethnologue", "Language_Code_Data_20170221",
                         "LanguageIndex.tab")
    col_types <- cols(
      LangID = col_character(),
      CountryID = col_character(),
      NameType = col_character(),
      Name = col_character()
    )
    read_tsv(path, na = "", col_types = col_types)
  },

  # WALS
  # WALS data with ISO updates applied and only language level data
  wals = function() {
    vars <- c("wals_code", "iso_code", "glottocode", "Name", "latitude",
              "longitude", "genus", "family", "macroarea", "countrycodes")
    updates <- read_csv(project_path("data-raw", "wals-updates.csv"),
                        na = "",
                        col_types =
                        cols_only(
                           wals_code = col_character(),
                           iso_code = col_character()
                         ))
    path <- project_path("external", "wals", "language.csv")
    wals <- read_csv(path,
                     col_types = cols(
                       .default = col_character(),
                       latitude = col_double(),
                       longitude = col_double()
                     ), na = "") %>%
      left_join(select(updates,
                       wals_code,
                       iso_code_new = iso_code),
                by = "wals_code") %>%
      mutate(iso_code = coalesce(iso_code_new, iso_code)) %>%
      select(one_of(vars))
  },

  ethnologue_language_codes = function() {
    path <- project_path("external", "ethnologue", "Language_Code_Data_20170221",
                         "LanguageCodes.tab")
    col_types <- cols(
      LangID = col_character(),
      CountryID = col_character(),
      LangStatus = col_character(),
      Name = col_character()
    )
    read_tsv(path, na = "", col_types = col_types) %>%
      # patch Aka macrolang
      filter(LangID != "aka") %>%
      bind_rows(
        tibble(LangID = c("twi", "fat"),
               CountryID = "GH",
               LangStatus = "L",
               Name = c("Twi", "Fantse")))
  },

  afrobarometer_to_wals_countries = function() {
    path <- project_path("data-raw", "afrobarometer_to_wals_countries.csv")
    col_types <- cols(
      round = col_character(),
      question = col_character(),
      lang_id = col_integer(),
      lang_name = col_character(),
      wals_code = col_character(),
      countries = col_character()
    )
    read_csv(path, col_types = col_types, na = "")
  },

  afrobarometer_to_iso_639_3_countries = function() {
    path <- project_path("data-raw", "afrobarometer_to_iso_639_3_countries.csv")
    col_types <- cols(
      round = col_character(),
      question = col_character(),
      lang_id = col_integer(),
      lang_name = col_character(),
      iso_639_3 = col_character(),
      countries = col_character()
    )
    read_csv(path, col_types = col_types, na = "")
  },

  ethnologue_distances = function() {
    path <- project_path("data-raw", "ethnologue-distances.csv.gz")
    col_types <- cols(
      from = col_character(),
      to = col_character(),
      distance = col_integer()
    )
    read_csv(gzfile(path), na = "", col_types = col_types)
  }

)

.afrobarometer <- function(.round) {
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
env_bind(IO, afrobarometer = .afrobarometer)

#' Read Generated Data
env_bind_fns(IO, afrobarometer_langs = function() {
  path <- project_path("data", "afrobarometer_langs.csv")
  col_types <- cols(
    round = col_character(),
    value = col_integer(),
    question = col_character(),
    name = col_character(),
    countries = col_character()
  )
  read_csv(path, na = "", col_types = col_types)
})

env_bind_fns(IO, afrobarometer_langs_other = function() {
  path <- project_path("data", "afrobarometer_langs_other.csv")
  col_types <- cols(
    round = col_character(),
    question = col_character(),
    country = col_integer(),
    value = col_character(),
    iso_alpha2 = col_character()
  )
  read_csv(path, na = "", col_types = col_types)
})

env_bind_fns(IO, iso_to_wals = function() {
  path <- project_path("data", "iso_to_wals.csv")
  col_types <- cols(
    iso_code = col_character(),
    wals_code = col_character(),
    iso_code_to = col_character(),
    distance = col_integer()
  )
  read_csv(path, na = "", col_types = col_types)
})

env_bind_fns(IO, afrobarometer_to_iso = function() {
  path <- project_path("data", "afrobarometer_to_iso_639_3.csv")
  col_types <- cols(
    round = col_character(),
    question = col_character(),
    lang_id = col_integer(),
    lang_name = col_character(),
    iso_639_3 = col_character(),
    iso_ref_name = col_character(),
    iso_scope = col_character()
  )
  read_csv(path, na = "", col_types = col_types)
})

env_bind_fns(IO, afrobarometer_other_to_iso = function() {
  path <- project_path("data", "afrobarometer_other_to_iso_639_3.csv")
  col_types <- cols(
    round = col_character(),
    question = col_character(),
    country = col_integer(),
    value = col_character(),
    iso_639_3 = col_character(),
    iso_scope = col_character(),
    iso_ref_name = col_character(),
    iso_alpha2 = col_character()
  )
  read_csv(path, na = "", col_types = col_types)
})


#' Misc functions
#'
#'
#'  Set equality
seteq <- function(x, y) {
!length(setdiff(x, y)) && !length(setdiff(y, x))
}
