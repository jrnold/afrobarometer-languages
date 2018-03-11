suppressPackageStartupMessages({
  library("tidyverse")
  library("rprojroot")
  library("magrittr")
  library("stringr")
  library("assertthat")
  library("rlang")
  library("httr")
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

  afrobarometer_lang_variables = function() {
    path <- project_path("data-raw", "afrobarometer_lang_variables.csv")
    col_types <- cols(
      round = col_integer(),
      name = col_character(),
      type = col_character(),
      other = col_logical()
    )
    read_csv(path, na = "", col_types = col_types)
  },

  afrobarometer_country_variables = function() {
    path <- project_path("data-raw", "afrobarometer_country_variables.csv")
    col_types <- cols(
      round = col_integer(),
      name = col_character()
    )
    read_csv(path, na = "", col_types = col_types)
  },

  afrobarometer_countries = function() {
    path <- project_path("data-raw", "afrobarometer_countries.csv")
    read_csv(path,
             na = "",
             col_types = cols(round = col_integer(),
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
    rounds <- 1:6
    dirname <- project_path("data-raw", "afrobarometer-mappings")
    filenames <- file.path(dirname, str_c("r", rounds, ".yml"))
    map2(filenames, rounds,
         function(x, r) {
           print(x)
           out <- read_yml(x)
           map(out, ~ c(.x, list(round = r)))
         }) %>%
      purrr::flatten()
  },

  afrobarometer_other_mappings = function() {
    path <- project_path("data-raw", "afrobarometer-mappings", "other.yml")
    read_yml(path)
  },

  # ISO Data
  iso_639_3_codes = function() {
    src_sqlite("external/lingdata/iso_639_3.db") %>%
      tbl("iso_639_3") %>%
      collect()
  },

  iso_639_3_macrolanguages = function() {
    src_sqlite("external/lingdata/iso_639_3.db") %>%
      tbl("iso_639_3_macrolanguages") %>%
      collect()
  },

  # WALS
  # WALS data with ISO updates applied and only language level data
  wals = function() {
    tbl(src_sqlite("external/lingdata/wals.db"), "languages") %>%
      collect()
  },

  wals = function() {
    tbl(src_sqlite("external/lingdata/wals.db"), "languages") %>%
      collect()
  },

  ethnologue_language_codes = function() {
    src_sqlite("external/lingdata/ethnologue.db") %>%
      tbl("LanguageCodes") %>%
      collect() %>%
      # patch Aka macrolang
      filter(LangID != "aka") %>%
      bind_rows(
        tibble(LangID = c("twi", "fat"),
               CountryID = "GH",
               LangStatus = "L",
               Name = c("Twi", "Fantse")))
  },

  ethnologue_countries = function() {
    src_sqlite("external/lingdata/ethnologue.db") %>%
      tbl("LanguageIndex") %>%
      select(LangID, CountryID) %>%
      distinct() %>%
      collect() %>%
      # patch Aka macrolang
      bind_rows(tibble(LangID = c("twi", "fat"), CountryID = "GH"))
  },

  afrobarometer_to_wals_countries = function() {
    path <- project_path("data-raw", "afrobarometer_to_wals_countries.csv")
    col_types <- cols(
      round = col_integer(),
      variable = col_character(),
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
      round = col_integer(),
      variable = col_character(),
      lang_id = col_integer(),
      lang_name = col_character(),
      iso_639_3 = col_character(),
      countries = col_character()
    )
    read_csv(path, col_types = col_types, na = "")
  },

  ethnologue_distances = function() {
    path <- project_path("data-raw", "ethnologue",
                         "iso_639_3-distances.csv.gz")
    # col_types <- cols(
    #   iso_639_3_from = col_character(),
    #   iso_639_3_to = col_character(),
    #   distance = col_integer()
    # )
    col_types <- NULL
    read_csv(gzfile(path), na = "", col_types = col_types)
  },

  additional_iso_countries = function() {
    path <- project_path("data-raw",
                         "additional_iso_countries.csv")
    col_types <- cols(
      CountryID = col_character(),
      LangID = col_character()
    )
    read_csv(path, na = "", col_types = col_types)
  },

  afrobarometer_to_iso_distant_matches = function() {
    path <- project_path("data-raw",
                         "afrobarometer_to_iso_distant_matches.csv")
    col_types <- cols(
      round = col_integer(),
      variable = col_character(),
      lang_id = col_integer(),
      lang_name = col_character(),
      iso_alpha2 = col_character(),
      distance = col_double()
    )
    read_csv(path, na = "", col_types = col_types)
  },

  afrobarometer_to_wals_country_nonmatches = function() {
    path <- project_path("data-raw",
                         "afrobarometer_to_wals_country_nonmatches.csv")
    col_types <- cols(
      round = col_integer(),
      variable = col_character(),
      lang_id = col_integer(),
      iso_alpha2 = col_character(),
      wals_code = col_character()
    )
    read_csv(path, na = "", col_types = col_types)
  },

  afrobarometer_other_to_wals_country_nonmatches = function() {
    path <- project_path("data-raw",
                         "afrobarometer_other_to_wals_country_nonmatches.csv")
    col_types <- cols_only(
      lang_name = col_character(),
      iso_alpha2 = col_character(),
      wals_code = col_character()
    )
    read_csv(path, na = "", col_types = col_types)
  },

  afrobarometer_to_wals = function() {
    path <- project_path("data", "afrobarometer_to_wals.csv")
    col_types <- cols(
      round = col_integer(),
      variable = col_character(),
      lang_id = col_integer(),
      lang_name = col_character(),
      country = col_integer(),
      iso_alpha2 = col_character(),
      wals_code = col_character(),
      wals_name = col_character(),
      auto = col_integer()
    )
    read_csv(path, na = "", col_types = col_types)
  },

  afrobarometer_other_to_wals = function() {
    path <- project_path("data", "afrobarometer_other_to_wals.csv")
    col_types <- cols(
      round = col_integer(),
      variable = col_character(),
      country = col_integer(),
      lang_name = col_character(),
      iso_alpha2 = col_character(),
      wals_code = col_character(),
      wals_name = col_character(),
      auto = col_integer()
    )
    read_csv(path, na = "", col_types = col_types)
  },

  afrobarometer_to_iso = function() {
    path <- project_path("data", "afrobarometer_to_iso_639_3.csv")
    col_types <- cols(
      round = col_integer(),
      variable = col_character(),
      country = col_integer(),
      lang_id = col_character(),
      iso_639_3 = col_character(),
      iso_scope = col_character(),
      iso_ref_name = col_character(),
      iso_alpha2 = col_character()
    )
    read_csv(path, na = "", col_types = col_types)
  },

  afrobarometer_other_to_iso = function() {
    path <- project_path("data", "afrobarometer_other_to_iso_639_3.csv")
    col_types <- cols(
      round = col_integer(),
      variable = col_character(),
      country = col_integer(),
      lang_name = col_character(),
      iso_639_3 = col_character(),
      iso_scope = col_character(),
      iso_ref_name = col_character(),
      iso_alpha2 = col_character()
    )
    read_csv(path, na = "", col_types = col_types)
  },

  glottolog_languoids = function() {
    src_sqlite("external/lingdata/glottolog.db") %>%
      tbl("languoids") %>%
      filter(!bookkeeping) %>%
      collect()
  },

  glottolog_macroareas = function() {
    src_sqlite("external/lingdata/glottolog.db") %>%
      tbl("macroareas") %>%
      collect()
  },

  glottolog_to_wals = function() {
    src_sqlite("external/lingdata/glottolog.db") %>%
      tbl("wals_codes") %>%
      collect()
  },

  afrobarometer_variables = function() {
    misc <- yaml::yaml.load_file(project_path("data-raw", "misc.yml"))
    map_df(misc$afrobarometer$variables, as_tibble)
  }

)

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

#' Read Generated Data
env_bind_fns(IO, afrobarometer_langs = function() {
  path <- project_path("data", "afrobarometer_langs.csv")
  col_types <- cols(
    round = col_integer(),
    variable = col_character(),
    lang_id = col_integer(),
    lang_name = col_character(),
    country = col_integer(),
    iso_alpha2 = col_character()
  )
  read_csv(path, na = "", col_types = col_types)
})

env_bind_fns(IO, afrobarometer_langs_other = function() {
  path <- project_path("data", "afrobarometer_langs_other.csv")
  col_types <- cols(
    round = col_integer(),
    variable = col_character(),
    country = col_integer(),
    lang_name = col_character(),
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
    round = col_integer(),
    variable = col_character(),
    lang_id = col_integer(),
    lang_name = col_character(),
    country = col_integer(),
    iso_alpha2 = col_character(),
    iso_639_3 = col_character(),
    iso_ref_name = col_character(),
    iso_scope = col_character()
  )
  read_csv(path, na = "", col_types = col_types)
})


env_bind_fns(IO, afrobarometer_to_glottolog = function() {
  path <- project_path("data", "afrobarometer_to_glottolog.csv")
  col_types <- cols(
    round = col_integer(),
    variable = col_character(),
    lang_id = col_integer(),
    lang_name = col_character(),
    country = col_integer(),
    iso_alpha2 = col_character(),
    glottocode = col_character()
  )
  read_csv(path, na = "", col_types = col_types)
})

env_bind_fns(IO, afrobarometer_other_to_glottolog = function() {
  path <- project_path("data", "afrobarometer_other_to_glottolog.csv")
  col_types <- cols(
    round = col_integer(),
    variable = col_character(),
    lang_name = col_character(),
    iso_alpha2 = col_character(),
    country = col_integer(),
    glottocode = col_character()
  )
  read_csv(path, na = "", col_types = col_types)
})

#' Known missing values for glottolog
env_bind_fns(IO, afrobarometer_to_glottolog_nonmatches = function() {
  path <- project_path("data-raw", "afrobarometer_to_glottolog_nonmatches.csv")
  read_csv(path, na = "",
           col_types = cols(
               round = col_integer(),
               variable = col_character(),
               iso_alpha2 = col_character(),
               lang_name = col_character()
             ))
})


#' Countries associated with each ISO 639
iso_639_countries <- function() {
  macrolang_countries <-
    IO$iso_639_3_macrolanguages %>%
    left_join(IO$ethnologue_countries, by = c("I_Id" = "LangID")) %>%
    select(-I_Id, -I_Status) %>%
    rename(LangID = M_Id) %>%
    filter(!is.na(CountryID)) %>%
    bind_rows(tibble(LangID = "aka", CountryID = "GH"))

  bind_rows(IO$ethnologue_countries,
            macrolang_countries,
            IO$additional_iso_countries)
}
rlang::env_bind_fns(IO, iso_639_3_countries = iso_639_countries)


#' Misc functions
#'
#'
#'  Set equality
seteq <- function(x, y) {
  !length(setdiff(x, y)) && !length(setdiff(y, x))
}
