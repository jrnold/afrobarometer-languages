#' ---
#' title: "Match Afrobarometer Languages to ISO 639-3 Language Codes"
#' ---
#'
#' These matches were originally made by matching
#' alternative names along with manual editing.
#'
suppressPackageStartupMessages({
  library("tidyverse")
  library("stringr")
  library("rprojroot")
  library("purrr")
  library("magrittr")
  library("yaml")
})

OUTPUT <- find_rstudio_root_file("data", "afrobarometer_to_iso_639_3.csv")

INPUTS <- list(
  afrobarometer_langs = list("data", "afrobarometer_langs.csv"),
  iso_langs = list("external",
                   "iso-639-3",
                   "iso-639-3_Code_Tables_20170217",
                   "iso-639-3_20170202.tab"),
  iso_macrolangs = list("external",
                            "iso-639-3",
                            "iso-639-3_Code_Tables_20170217",
                            "iso-639-3-macrolanguages_20170131.tab"),
  afrobarometer_to_iso = list("data-raw", "afrobarometer_to_iso.yml")
  ) %>%
  {setNames(map(., function(x) invoke(find_rstudio_root_file, x)),
            names(.))}

# Read Afrobarometer Languages
read_afrobarometer_langs <- function() {
  read_csv(INPUTS$afrobarometer_langs,
           col_types = cols(
             lang_id = col_integer(),
             question = col_character(),
             lang_name = col_character(),
             countries = col_character(),
             languages = col_character(),
             is_language = col_logical()
           ))
}
afrobarometer_langs <- read_afrobarometer_langs()

#'
#' Create a dataset that will be used match names.
#' Often the Afrobarometer lists multiple names for languages,
#' these are seperated into multiple rows.
#' Additionally the names are standardized to be more likely to match:
#' all lower ASCII, non-letters removed.

#'
#' Includes:
#'
#' - `Id`: ISO 639-3 code
#' - `Part2B`: ISO 639-2 B code (if it exists)
#' - `Part2T`: ISO 639-2 T code (if it exists)
#' - `Part1`: `ISO 639-1 code (if it exists)
#' - `Scope`: Scope
#'     - `I`: Individual
#'     - `M`: Macrolanguage
#'     - `S`: Special
#' - `Language_Type`: Tyep of language
#'     - `A`: Ancient
#'     - `C`: Constructed
#'     - `E`: Extinct
#'     - `H`: Historical
#'     - `L`: Living
#'     - `S`: Special
#' - `Ref_Name`: Language Reference Name
#'
#' See [original documentation](http://www-01.sil.org/iso639-3/download.asp)
#'
#' Keep only living languages

read_iso_langs <- function(x) {
  read_tsv(INPUTS$iso_langs,
           col_names =
             c("Id", "Part2B", "Part2T", "Part1", "Scope", "Language_Type",
               "Ref_Name", "Comment"),
           col_types =
             cols(
               Id = col_character(),
               Part2B = col_character(),
               Part2T = col_character(),
               Part1 = col_character(),
               Scope = col_character(),
               Language_Type = col_character(),
               Ref_Name = col_character(),
               Comment = col_character()
             ), na = "") %>%
    filter(Language_Type == "L") %>%
    select(-Language_Type, -Comment)
}
iso_langs <- read_iso_langs()

#'
#' Get the data for mappings from individual languages to macro languages
#'
#' `M_Id`: Identifier for a macrolanguage
#' `I_Id`: Identifier for individual language that is a member
#' `I_Status`: `A` is active, `R` is retired.
#'
#' See [original documentation](http://www-01.sil.org/iso639-3/download.asp)
read_iso_macrolangs <- function() {
  read_tsv(INPUTS$iso_macrolangs,
           col_types = cols(
             M_Id = col_character(),
             I_Id = col_character(),
             I_Status = col_character()
           ), na = "") %>%
  filter(I_Status == "A") %>%
  select(-I_Status)
}
iso_macrolangs <- read_iso_macrolangs()

read_afrobarometer_to_iso <- function() {
  yaml.load_file(INPUTS$afrobarometer_to_iso) %>%
    map(compact) %>%
    map_df(function(.x) {
      out <- tidyr::crossing(question = .x[["question"]],
                      iso_639_3 = .x[["iso_639_3"]])
      out[["lang_id"]] <- .x$lang_id
      out[["note"]] <- .x$note
      out
    })
}
afrobarometer_to_iso <- read_afrobarometer_to_iso()

#' Add any ISO macro-languages associated with matched ISO languages:
afrobarometer_to_iso_macros <-
  inner_join(afrobarometer_to_iso, iso_macrolangs,
            by = c("iso_639_3" = "I_Id")) %>%
  select(-iso_639_3) %>%
  rename(iso_639_3 = M_Id) %>%
  distinct()

#' combine individual and macro-languages
afrobarometer_to_iso %<>%
  bind_rows(afrobarometer_to_iso_macros) %>%
  distinct()

#' add ISO information
afrobarometer_to_iso %<>%
  left_join(select(iso_langs,
                   iso_639_3 = Id,
                   iso_scope = Scope,
                   iso_ref_name = Ref_Name,
                   iso_639_2B = Part2B,
                   iso_639_3T = Part2T,
                   iso_639_1 = Part1
                   ),
            by = "iso_639_3")

#' Add
afrobarometer_to_iso %<>%
  left_join(select(afrobarometer_langs, question, lang_id, lang_name),
            by = c("question", "lang_id")) %>%
  select(question, lang_id, lang_name, iso_639_3, iso_ref_name,
         iso_scope, note) %>%
  arrange(question, lang_id, iso_639_3)

# Check that there are no languages that are unaccounted for
nonmatches <-
  anti_join(afrobarometer_langs, afrobarometer_to_iso,
          by = c("question", "lang_id"))
stopifnot(nrow(nonmatches) == 0)

# Write final output
write_afroarometer_to_iso <- function(x, path) {
  write_csv(x, path = path, na = "")
}
write_afroarometer_to_iso(afrobarometer_to_iso, OUTPUT)

metadata <- list(
  variables = list(
    question = str_c("Afrobarometer question number"),
    lang_id = str_c("Afrobarometer language id (variable value)"),
    lang_name = str_c("Afrobarometer language name (variable label)"),
    iso_639_3 = str_c("ISO-639-3 language code"),
    iso_ref_name = "ISO 639-3 language reference name",
    iso_scope = "I = individual language, M = macrolanguage"    )
)
