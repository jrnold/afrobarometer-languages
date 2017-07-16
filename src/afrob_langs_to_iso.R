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
#' 
#' 
suppressPackageStartupMessages({
  library("UnidecodeR")
})
filter <- dplyr::filter

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
## ----clean_lang_name-----------------------------------------------------
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
## ----afrobarometer_countries---------------------------------------------
afrob_countries <-
  readRDS(project_path("data/afrob_countries.rds")) %>%
  select(`ISO3166-1-Alpha-2`, afrobarometer_id) %>%
  dplyr::filter(!is.na(afrobarometer_id))

#' 
#' # Afrobarometer Languages
#' 

afrob_langs <- read_rds(project_path("data", "afrob_langs.rds"))

#' 
#' Create a dataset that will be used match names.
#' Often the Afrobarometer lists multiple names for languages,
#' these are seperated into multiple rows.
#' Additionally the names are standardized to be more likely to match: 
#' all lower ASCII, non-letters removed.
## ----afrobarometer_langs_matcher-----------------------------------------

afrob_langs_matcher <- 
  afrob_langs %>%
  unnest(countries, .drop = FALSE) %>%
  select(ab_question = question,
         ab_lang_id = lang_id,
         ab_lang_name = lang_name,
         ab_country_id = countries,
         standardized_name = languages) %>%
  unnest(standardized_name) %>%
  mutate(standardized_name = afrob_clean_lang_names(standardized_name)) %>%
  left_join(afrob_countries, 
            by = c("ab_country_id" = "afrobarometer_id")) %>%
  rename(iso_country_id = `ISO3166-1-Alpha-2`)


#' 
#' # ISO 639-3 / Ethnologue Languages
#' 
#' Now, combine the ISO 639-3 and Ethnologue datasets. These both are maintained by [SIL](https://www.sil.org), and use the ISO 639-3 code.
#' 

iso_langs <-
  readRDS(project_path("data", "iso_langs.rds")) %>%
  extract2("altnames") %>%
  filter(!str_detect(Name, regex("\\bsign\\b", ignore.case = TRUE))) %>%
  mutate(
    Name = deaccent(Name),
    standardized_name = clean_lang_names(str_replace(Name, ",.*$", "")))



#' 
#' # Merge
#' 
#' Merge the Afrobarometer Languages with the ISO/Ethnologue languages by their cleaned names.
#' 
#' In this I match by countries. This is more conservative than taking any match. (With matching by country approx 450 langs, if expanded, then approx 1640 matches)
#' 
#' Since an inner join is used, any non matches will not appear in this data frame.
## ----afrobarometer_to_iso_matches----------------------------------------
automatches <- 
  inner_join(afrob_langs_matcher,
             select(iso_langs, LangID,
                    CountryID, standardized_name),
              by = c("standardized_name",
                     c("iso_country_id" = "CountryID"))) %>%
  ungroup() %>%
  rename(iso_639_3 = LangID) %>%
  select(ab_question, ab_lang_id, iso_639_3) %>%
  distinct()

#' 
#' Since not all Afrobarometer languages are matched by name, supplement with manual matches.
#' 
#' The original set of matches was generated with this code, and then 
#' edited by hand.
# anti_join(afrobarometer_langs,
#           automatches,
#           by = c(qlang_id = "ab_qlang_id")) %>%
#   arrange(lang_id, question) %>%
#   mutate(country = map_chr(country, str_c, collapse = " ")) %>%
#   write_csv(path = "afrobarometer_to_iso.csv")

#' 
#' 
#' If an Afrobarometer language cannot be matched, give it the special ISO 639-3 code "und" for "Undetermined".
#' This is basically a missing value indicator, but will distinguish between languages that have a missing match because we forgot to match them and those that we attempted to match, but could not.
#' 
manual_match_file <-
  project_path("data-raw",
              "afrobarometer_languages",
              "afrob_to_iso.csv")
manual_matches <-
  read_csv(manual_match_file,
           col_types = cols_only(
           question = col_character(),
           lang_id = col_integer(),
           iso_639_3 = col_character()
          )) %>%
  mutate(iso_639_3 = str_split(iso_639_3, " +")) %>%
  unnest(iso_639_3) %>%
  rename(ab_question = question,
         ab_lang_id = lang_id)

afrob_langs_to_iso <-
  bind_rows(automatches, manual_matches) %>%
  distinct()

#' 
#' Arabic match filter. Keep only standard Arabic when the value is 5
#' 
afrob_langs_to_iso %<>%
  filter(ab_lang_id != 5 | iso_639_3 %in% c("arb"))


#' 
#' Add any ISO macro-languages associated with matched ISO languages:
## ------------------------------------------------------------------------

iso_macrolangs <-
  readRDS(project_path("data", "iso_langs.rds")) %>%
  extract2("macro")

macro_lang_matches <-
  inner_join(afrob_langs_to_iso,
             iso_macrolangs,
             by = c(iso_639_3 = "I_Id")) %>%
  select(-iso_639_3) %>%
  rename(iso_639_3 = M_Id)

afrob_langs_to_iso %<>% 
  bind_rows(macro_lang_matches) %>% 
  distinct()


comments <- list(
  NULL = str_c("Mapping from Afrobarometer languages and ISO-639-3 ",
               "language codes. These are for languages in Q2 and Q103 of ",
               "Afrobarometer round 6"),
  question = str_c("Question number in Afrobarometer r6"),
  lang_id = str_c("Language number in that question"),
  lang_name = str_c("Afrobarometer language name"),
  iso_639_3 = str_c("ISO-639-3 language code")
)

afrob_langs_to_iso %<>% add_description(comments)

#' Merge back with original Afrobarometer data
afrob_langs_to_iso <-
  left_join(select(afrob_langs, lang_id, question, lang_name),
            afrob_langs_to_iso,
            by = c("lang_id" = "ab_lang_id",
                   "question" = "ab_question"))

#' 
#' ## Tests 
#' 
#' It's a data frame with data and comments
data_exists(afrob_langs_to_iso)

#' There should be no missing values of `iso_639_3`:
stopifnot(!any(is.na(afrob_langs_to_iso$iso_639_3)))


#' 
#' ## Saving
#' 
#' Save the matches to a new file
## ------------------------------------------------------------------------
saveRDS(afrob_langs_to_iso, 
        file = project_path("data", "afrob_langs_to_iso.rds"))
