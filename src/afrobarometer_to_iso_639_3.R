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
  afrobarometer_r1_to_iso = list("data-raw", "afrobarometer_to_iso", "r1.yml"),
  afrobarometer_r2_to_iso = list("data-raw", "afrobarometer_to_iso", "r2.yml"),
  afrobarometer_r3_to_iso = list("data-raw", "afrobarometer_to_iso", "r3.yml"),
  afrobarometer_r4_to_iso = list("data-raw", "afrobarometer_to_iso", "r4.yml"),
  afrobarometer_r5_to_iso = list("data-raw", "afrobarometer_to_iso", "r5.yml"),
  afrobarometer_r6_to_iso = list("data-raw", "afrobarometer_to_iso", "r6.yml"),
  ethnologue = list("external", "ethnologue", "Language_Code_Data_20170221",
                    "LanguageIndex.tab")
  ) %>%
  {setNames(map(., function(x) invoke(find_rstudio_root_file, x)),
            names(.))}

# Read Afrobarometer Languages
read_afrobarometer_langs <- function() {
  read_csv(INPUTS$afrobarometer_langs,
           col_types = cols(
             round = col_character(),
             question = col_character(),
             value = col_integer(),
             label = col_character(),
             countries = col_character()
           )) %>%
    rename(lang_id = value, lang_name = name)
}
afrobarometer_langs <- read_afrobarometer_langs()

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

read_afrobarometer_to_iso <- function(filename) {
  ab_round <- tools::file_path_sans_ext(basename(filename))
  yaml.load_file(filename) %>%
    map(compact) %>%
    map_df(function(.x) {
      out <- tidyr::crossing(question = .x[["question"]],

                      iso_639_3 = .x[["iso_639_3"]])
      out[["lang_id"]] <- .x$lang_id
      out[["round"]] <- ab_round
      out
    })
}

read_afrobarometer_to_iso_all <- function() {
  INPUTS[str_subset(names(INPUTS), "afrobarometer_r\\d+_to_iso")] %>%
    map_df(read_afrobarometer_to_iso)
}

afrobarometer_to_iso <- read_afrobarometer_to_iso_all()

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

#' Add Additional info from Afrobarometer
afrobarometer_to_iso %<>%
  left_join(select(afrobarometer_langs, round, question, lang_id, lang_name),
            by = c("round", "question", "lang_id")) %>%
  arrange(round, question, lang_id, iso_639_3) %>%
  select(round, question, lang_id, lang_name,
         iso_639_3, iso_ref_name, iso_scope)

#' Checks
#'
#' All Afrobarometer Languages Should be Accounted For
afrobarometer_lang_nonmatches <-
  anti_join(afrobarometer_langs, afrobarometer_to_iso,
          by = c("question", "lang_id"))
stopifnot(nrow(afrobarometer_lang_nonmatches) == 0)

#' All ISO-Codes should be valid
#' not missing
stopifnot(all(!is.na(afrobarometer_to_iso$iso_639_3)))

#' A valid ISO code
SPECIAL_ISO_CODES <- c("mis", "mul", "und", "zxx")
iso_lang_nonmatches <-
  anti_join(filter(afrobarometer_to_iso, !(iso_639_3 %in% SPECIAL_ISO_CODES)),
            iso_langs,
            by = c("iso_639_3" = "Id"))
stopifnot(nrow(iso_lang_nonmatches) == 0)

# TODO: check languages against countries in Ethnologue and Afrobarometer
read_ethnologue <- function() {
  read_tsv(INPUTS$ethnologue,
           col_types = cols(
             LangID = col_character(),
             CountryID = col_character(),
             NameType = col_character(),
             Name = col_character()
           ),
           na = "") %>%
    select(LangID, CountryID) %>%
    group_by(LangID) %>%
    summarise(countries = list(sort(unique(CountryID))))
}
ethnologue_langidx <- read_ethnologue()

afrobarometer_langs_countries <-
  afrobarometer_langs %>%
  select(round, question, lang_id, countries) %>%
  filter(!is.na(countries)) %>%
  mutate(countries = str_split(countries, " +"))

afrobarometer_to_iso %>%
  filter(!iso_639_3 %in% c("mis", "und", "mul")) %>%
  filter(iso_scope %in% c("I")) %>%
  inner_join(afrobarometer_langs_countries,
             by = c("round", "question", "lang_id")) %>%
  inner_join(rename(ethnologue_langidx, ethnologue_countries = countries),
            by = c(iso_639_3 = "LangID")) %>%
  mutate(country_overlap =
            map2_lgl(countries, ethnologue_countries, ~ any(.x %in% .y))) %>%
  filter(!country_overlap) %>%
  mutate(countries = map_chr(countries, paste, collapse = " "),
         ethnologue_countries = map_chr(ethnologue_countries, paste,
                                        collapse = " ")) %>%
  print(width = 10000, n = 100)


# Write final output
write_afroarometer_to_iso <- function(x, path) {
  write_csv(x, path = path, na = "")
}
write_afroarometer_to_iso(afrobarometer_to_iso, OUTPUT)
