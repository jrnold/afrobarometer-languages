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
    description <- project_path("external",
                                "iso-639-3_Code_Tables_20170217.zip")
    filename <- paste("iso-639-3_Code_Tables_20170217",
                      "iso-639-3_20170202.tab", sep = "/")
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
    read_tsv(unz(description, filename), na = "", col_types = col_types)
  },

  iso_639_3_macrolanguages = function() {
    description <- project_path("external",
                                "iso-639-3_Code_Tables_20170217.zip")
    filename <- paste("iso-639-3_Code_Tables_20170217",
                      "iso-639-3-macrolanguages_20170131.tab", sep = "/")
    col_types <- cols(
      M_Id = col_character(),
      I_Id = col_character(),
      I_Status = col_character()
    )
    read_tsv(unz(description, filename), na = "", col_types = col_types)
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
                           iso_code = col_character(),
                           macroarea = col_character(),
                           countrycodes = col_character()
                         ))
    description <- project_path("external", "wals-language.csv.zip")
    filename <- "language.csv"
    wals <- read_csv(unz(description, filename),
                     col_types = cols(
                       .default = col_character(),
                       latitude = col_double(),
                       longitude = col_double()
                     ), na = "") %>%
      left_join(select(updates,
                       wals_code,
                       iso_code_new = iso_code,
                       macroarea_new = macroarea,
                       countrycodes_new = countrycodes),
                by = "wals_code") %>%
      mutate(iso_code = coalesce(iso_code_new, iso_code),
             macroarea = coalesce(macroarea_new, macroarea),
             countrycodes = coalesce(countrycodes_new, countrycodes)) %>%
      select(one_of(vars)) %>%
      mutate(countrycodes = map_if(str_split(countrycodes, " "),
                                   is.null, list(character())))
  },

  # Ethnologue data
  ethnologue = function() {
    description <- project_path("external", "Language_Code_Data_20170221.zip")
    filename <- "LanguageIndex.tab"
    col_types <- cols(
      LangID = col_character(),
      CountryID = col_character(),
      NameType = col_character(),
      Name = col_character()
    )
    read_tsv(unz(description, filename), na = "", col_types = col_types)
  },

  ethnologue_language_codes = function() {
    description <- project_path("external", "Language_Code_Data_20170221.zip")
    filename <- "LanguageCodes.tab"
    col_types <- cols(
      LangID = col_character(),
      CountryID = col_character(),
      LangStatus = col_character(),
      Name = col_character()
    )
    read_tsv(unz(description, filename), na = "", col_types = col_types) %>%
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
    path <- project_path("data-raw", "ethnologue", "ethnologue-distances.csv.gz")
    col_types <- cols(
      from = col_character(),
      to = col_character(),
      distance = col_integer()
    )
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
      value = col_character(),
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
      value = col_character(),
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
      value = col_character(),
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
      value = col_character(),
      iso_639_3 = col_character(),
      iso_scope = col_character(),
      iso_ref_name = col_character(),
      iso_alpha2 = col_character()
    )
    read_csv(path, na = "", col_types = col_types)
  },

  glottolog_languoids = function() {
    description <- project_path("external", "glottolog",
                                "glottolog-languoid.csv.zip")
    filename <- "languoid.csv"
    col_types <- cols(
      bookkeeping = col_character(),
      child_dialect_count = col_integer(),
      child_family_count = col_integer(),
      child_language_count = col_integer(),
      description = col_character(),
      family_pk = col_integer(),
      father_pk = col_integer(),
      hid = col_character(),
      id = col_character(),
      jsondata = col_character(),
      latitude = col_double(),
      level = col_character(),
      longitude = col_double(),
      markup_description = col_character(),
      name = col_character(),
      newick = col_character(),
      pk = col_integer(),
      status = col_character()
    )
    read_csv(unz(description, filename), col_types = col_types, na = "")
  },

  glottolog_resourcemap = function() {
    resource2df <- function(x) {
      if (length(x$identifiers)) {
        out <- map_df(x$identifiers, as_tibble)
        out$glottocode <- x$id
        out
      }
    }

    read_json(project_path("external", "glottolog", "resourcemap.json"),
              simplifyVector = FALSE) %>%
      `[[`("resources") %>%
      map_df(resource2df)
  },

  glottolog_descendants = function() {
    path <- project_path("external", "glottolog", "tree-glottolog.json")
    walk_tree <- function(x, level = 0L, env = rlang::new_environment()) {
      glottocode <- x[["glottocode"]]
      descendants <- flatten_chr(map(x[["children"]], walk_tree,
                                     level = level + 1L, env = env))
      env[[glottocode]] <- list(
        descendants = c(glottocode, descendants),
        level = level
      )
      c(glottocode, descendants)
    }

    env <- rlang::new_environment()
    glottolog_tree <-
      read_json(path, simplifyVector = FALSE) %>%
      walk(walk_tree, level = 0, env = env)

    map_df(ls(env),
           function(i) {
             out <- as.tibble(env[[i]])
             out[["node"]] <- i
             out
           })
  },

  glottolog_lang_geo = function() {
    path <- project_path("external", "glottolog",
                         "languages-and-dialects-geo.csv")
    col_types = cols(
      glottocode = col_character(),
      name = col_character(),
      isocodes = col_character(),
      level = col_character(),
      macroarea = col_character(),
      latitude = col_double(),
      longitude = col_double()
    )
    read_csv(path, col_types = col_types, na = "")
  },

  glottolog = function() {
    path <- project_path("data", "glottolog.csv")
    col_types <- cols(
      glottocode = col_character(),
      name = col_character(),
      level = col_character(),
      depth = col_integer(),
      family = col_character(),
      parent = col_character(),
      ancestors = col_character(),
      children = col_character(),
      descendants = col_character(),
      iso_639_3 = col_character(),
      wals_codes = col_character(),
      macroarea = col_character(),
      latitude = col_double(),
      longitude = col_double(),
      bookkeeping = col_character()
    )
    read_csv(path, na = "", col_types = col_types)
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
    value = col_integer(),
    name = col_character(),
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

env_bind_fns(IO, afrobarometer_other_to_iso = function() {
  path <- project_path("data", "afrobarometer_other_to_iso_639_3.csv")
  col_types <- cols(
    round = col_integer(),
    variable = col_character(),
    country = col_integer(),
    value = col_character(),
    iso_639_3 = col_character(),
    iso_scope = col_character(),
    iso_ref_name = col_character(),
    iso_alpha2 = col_character()
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
    value = col_character(),
    iso_alpha2 = col_character(),
    country = col_integer(),
    glottocode = col_character()
  )
  read_csv(path, na = "", col_types = col_types)
})


#' Glottolog to ISO matches
#'
#' - ISO codes in a list
glottolog_to_iso <- function() {
  IO$glottolog_descendants %>%
    inner_join(filter(glottolog_resourcemap, type == "iso639-3") %>%
                 select(glottocode, iso_639_3 = identifier),
               by = c("descendants" = "glottocode")) %>%
    filter(!is.na(iso_639_3)) %>%
    select(iso_639_3, glottocode = node, level) %>%
    group_by(glottocode) %>%
    mutate(isocodes = list(unique(iso_639_3)))
}

#' Countries associated with each ISO 639
iso_639_countries <- function() {
  iso_639_countries <-
    iso_countries <- IO$ethnologue %>%
    select(LangID, CountryID) %>%
    distinct()

  macrolang_countries <- IO$iso_639_3_macrolanguages %>%
    rename(LangID = M_Id) %>%
    left_join(iso_countries, by = c(I_Id = "LangID")) %>%
    select(-I_Id, -I_Status) %>%
    filter(!is.na(CountryID)) %>%
    bind_rows(tibble(LangID = "aka", CountryID = "GH"))

  bind_rows(iso_countries, macrolang_countries,
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
