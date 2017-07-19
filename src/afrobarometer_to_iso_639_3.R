#' ---
#' title: "Match Afrobarometer Languages to ISO 639-3 Language Codes"
#' ---
#'
#' Match Afrobarometer languages to standard ISO 639-3 Language code.
#' This script creates a data frame which maps (Question number, Language ID) to
#' the ISO 639-3 code of the language. The mapping is many to many.
#'
#' I define the following custom ISO 639 codes, using the fact that
#' the range qaa-qzz are reserved for applications to use. This includes both
#' missing values and languages that are unknown. I use these to be able to
#' distinguish between known non-matches, and non-matches that have not yet
#' been accounted for.
#'
#' ---- ----- --------------------- -------------
#' qra  9998  Refused to Answern     Q2,Q103
#' qdk  9999  Don't know             Q2,Q103
#' qna  -1    Missing                Q2,Q103
#' qai  710   Asian/Indian           Q2
#' ---- ----- --------------------- -------------
suppressPackageStartupMessages({
  library("tidyverse")
  library("stringr")
  library("rprojroot")
  library("purrr")
  library("magrittr")
  library("UnidecodeR")
  library("yaml")
})

OUTPUT <- find_rstudio_root_file("data", "afrobarometer_to_iso_639_3.csv")

INPUTS <- list(
  afrobarometer_langs = list("data", "afrobarometer_langs.csv"),
  iso_langs = list("external",
                   "iso-639-3",
                   "iso-639-3_Code_Tables_20160525",
                   "iso-639-3_20160525.tab"),
  iso_macrolangs = list("external",
                            "iso-639-3",
                            "iso-639-3_Code_Tables_20160525",
                            "iso-639-3-macrolanguages_20160725.tab"),
  afrobarometer_countries = list("data-raw", "afrobarometer_countries.csv"),
  afrobarometer_to_iso = list("data-raw", "afrobarometer_to_iso.yml"),
  ethnologue_languages_index = list("external",
                                    "ethnologue",
                                    "LanguageIndex.tab")
  ) %>%
  {setNames(map(., function(x) invoke(find_rstudio_root_file, x)),
            names(.))}

#'
#' # Utility functions
#'
#' This function cleans language names to make them easier to match.
#'
#' - convert Unicode to the best ASCII match
#' - standardize to lower
#' - remove any non-letters
#' - Convert Malagache alternatives in the Afrobarometer to Malagache
#'
deaccent <- function(x) {
  ## see gensim function deaccent
  x <- iconv(x, to = "UTF-8")
  x <- stringi::stri_trans_nfd(x)
  x <- stringi::stri_replace_all_charclass(x, "\\p{Mn}", "")
  x <- stringi::stri_trans_nfc(x)
  x
}

afrob_clean_lang_names <- function(x) {
  x <- deaccent(x)
  x[x %in% c("Berber Language")] <- "Berber"
  x[x %in% c("Nubian Language")] <- "Nubian"
  x[x %in% c("Anglais")] <- "English"
  x[x %in% c("Francais")] <- "French"
  x[x %in% c("Portugues")] <- "Portuguese"
  x <- iconv(x, to = "UTF-8")
  x <- str_to_lower(str_replace(x, "[^A-Za-z]", ""))
  x
}

clean_lang_names <- function(x) {
  x <- str_to_lower(x)
  x <- str_replace(x, "[^A-Za-z]", "")
  x <- iconv(x, to = "UTF-8")
  x
}

#'
#' # Afrobarometer countries
#'
#' Get the list of countries in the Afrobarometer; when matching we only want to concern ourselves with languages spoken in these countries.
#' We need the ISO-3166-1 2-letter country code for each of these, since that is what is used in the Ethnologue and ISO datasets.
#'
read_afrobarometer_countries <- function(x) {
  read_csv(INPUTS$afrobarometer_countries,
           col_types = cols(
             country_name = col_character(),
             afrob_abbv = col_character(),
             iso_alpha3 = col_character(),
             iso_alpha2 = col_character()
           ), na = "")
  # Important to indicate na = "" since Namibia's alpha-2 code is NA
}
afrobarometer_countries <- read_afrobarometer_countries()

# Read Afrobarometer Languages
read_afrobarometer_langs <- function() {
  read_csv(INPUTS$afrobarometer_langs,
           col_types = cols(
             qlang_id = col_character(),
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

afrobarometer_patterns <-
  afrobarometer_langs %>%
  filter(is_language) %>%
  select(-is_language) %>%
  mutate(countries = str_split(countries, " +"),
         languages = str_split(languages, fixed(";"))) %>%
  unnest(countries, .drop = FALSE) %>%
  unnest(languages, .drop = FALSE) %>%
  select(question,
         lang_id,
         lang_name,
         afrob_abbv = countries,
         standardized_name = languages) %>%
  unnest(standardized_name) %>%
  mutate(standardized_name = afrob_clean_lang_names(standardized_name)) %>%
  left_join(select(afrobarometer_countries, afrob_abbv, iso_alpha2),
            by = c("afrob_abbv" = "afrob_abbv"))


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
    filter(Language_Type == "L", Scope == "I") %>%
    select(-Language_Type, -Comment)
}
iso_langs <- read_iso_langs()

#'
#' The Ethnologue provides alternate names for each language.
#' We will try all these alternative names in order to match to the Afrobarometer languages
#' See the original [documentation](https://www.ethnologue.com/codes/code-table-structure) for the table `LanguageIndex`.
#'
#' - `LangID`: ISO 639-3 language code
#' - `CountryID`: Country where the language name is used
#' - `NameType`: Type of language name
#'    - `L`: langauge
#'    - `LA`: Language alternate
#'    - `D`: Dialect
#'    - `DA`: Dialect alternate
#'    - `LP`,`DP`: Language (dialect) prejorative alternative
#' - `Name`: name
#'
#'
#' Get all names for ISO languages, but filter to the subset languages spoken in at least one of the Afrobarometer countries.
read_ethnologue_language_index <- function() {
  read_tsv(INPUTS$ethnologue_languages_index,
           col_types = cols(
             LangID = col_character(),
             CountryID = col_character(),
             NameType = col_character(),
             Name = col_character()
           ), na = "")
}
ethnologue_language_index <- read_ethnologue_language_index()


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

# Iso languages to match against
iso_lang_patterns <-
  semi_join(ethnologue_language_index, iso_langs, by = c("LangID" = "Id")) %>%
  # Ignore sign languages
  filter(!str_detect(Name, regex("\\bsign\\b", ignore.case = TRUE))) %>%
  mutate(
    Name = deaccent(Name),
    # clean language names
    standardized_name = clean_lang_names(str_replace(Name, ",.*$", "")))

#'
#' # Merge
#'
#' Merge the Afrobarometer Languages with the ISO/Ethnologue languages by their cleaned names.
#'
#' In this I match by countries. This is more conservative than taking any match. (With matching by country approx 450 langs, if expanded, then approx 1640 matches)
#'
read_afrobarometer_to_iso <- function() {
  yaml.load_file(INPUTS$afrobarometer_to_iso) %>%
    map(compact) %>%
    map_df(function(.x) {
      out <- tidyr::crossing(question = .x[["question"]],
                      iso_639_3 = .x[["iso_639_3"]])
      out[["lang_id"]] <- .x$lang_id
      out
    })
}
afrobarometer_to_iso_manual <- read_afrobarometer_to_iso()

# any Afrobarometer languages
afrobarometer_to_iso_auto <-
  anti_join(afrobarometer_patterns,
            distinct(afrobarometer_to_iso_manual, question, lang_id),
            by = c("question", "lang_id")) %>%
  inner_join(select(iso_lang_patterns, LangID, CountryID, standardized_name),
             by = c("standardized_name", c("iso_alpha2" = "CountryID"))) %>%
  ungroup() %>%
  rename(iso_639_3 = LangID) %>%
  select(question, lang_id, iso_639_3) %>%
  distinct()

afrobarometer_to_iso <-
  bind_rows(afrobarometer_to_iso_manual, afrobarometer_to_iso_auto) %>%
  arrange(question, lang_id)


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
                   iso_ref_name = Ref_Name
                   #iso_639_2B = Part2B,
                   #iso_639_3T = Part2T,
                   #iso_639_1 = Part1
                   ),
            by = "iso_639_3")

#' Add
afrobarometer_to_iso %<>%
  left_join(select(afrobarometer_langs, question, lang_id, lang_name),
            by = c("question", "lang_id")) %>%
  select(question, lang_id, lang_name, iso_639_3, iso_ref_name,
         iso_scope) %>%
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
