#' ---
#' title: Checks the consistency of the language data
#' ---
library("assertr")
source(here::here("src", "R", "init.R"))

language_values <- read_csv("data/language_values.csv", na = "",
         col_types = cols(
           round = col_integer(),
           variable = col_character(),
           type = col_character(),
           value = col_integer(),
           label = col_character(),
           country = col_character(),
           lang_name = col_character(),
           iso_639_3 = col_character(),
           glottocode = col_character(),
           wals = col_character(),
           n_resp = col_integer(),
           prop = col_double()
         ))


TESTDATA <- yaml::read_yaml(here::here("data-raw", "tests.yml"))

#' No duplicates
lang_dups <- language_values[duplicated(language_values), ]
if (nrow(lang_dups)) {
  cat("Duplicated language values\n")
  print(lang_dups)
}

#' Check that ISO 639-3 values are non-missing
missing_iso <- language_values %>%
  filter(is.na(iso_639_3))
if (nrow(missing_iso)) {
  cat("ISO 639-3 codes are missing:\n")
  print(missing_iso)
}


#' Check that glottolog is only missing if iso_639_3 is missing
missing_glottocodes <- language_values %>%
  filter(!iso_639_3 %in% TESTDATA$iso$special,
         is.na(glottocode))
if (nrow(missing_glottocodes)) {
  cat("Glottocodes are missing:\n")
  print(missing_glottocodes)
}

#' Check that WALS is only missing if glottolog is missing
missing_wals <- language_values %>%
  filter(!is.na(glottocode), is.na(wals))
if (nrow(missing_wals)) {
  cat("WALS codes are missing:\n")
  print(missing_wals)
}
#' ## ISO 639-3 Matches

#' Check that all ISO 639-3 codes are valid
iso_639_3_db <- src_sqlite(here::here("external/lingdata/iso_639_3.db"))

lang_iso <- language_values %>%
  filter(!iso_639_3 %in% TESTDATA$iso$special) %>%
  select(round, variable, lang_name, country, iso_639_3) %>%
  mutate(iso_639_3 = str_split(iso_639_3, " ")) %>%
  unnest() %>%
  left_join(tbl(iso_639_3_db, "iso_639_3"),
            by = c("iso_639_3" = "Id"), copy = TRUE)

#' All ISO 639-3 should
#'
#' - exist
#' - be an individual or macrolanguage
#' - be a living language
bad_iso_match <- filter(lang_iso,
                        !(Language_Type %in% "L" & Scope %in% c("I", "M")))
if (nrow(bad_iso_match)) {
  print("Missing or non-living ISO 639-3 language codes found")
  print(distinct(bad_iso_match, iso_639_3, Language_Type, Scope))
}


#' Check that for ISO 639-3 matches they are either all individual languages
#' or one macrolanguage
bad_iso_scope <-
  lang_iso %>%
  group_by(round, variable, lang_name, country) %>%
  summarise(n_macro = sum(Scope == "M"),
            n_indiv = sum(Scope == "I")) %>%
  filter(n_macro > 1 |
           n_macro >= 1 & n_indiv >= 1)
if (nrow(bad_iso_scope)) {
  print("Must match either one or more individual ISO 639-3 languages OR one macrolanguage.")
  print(bad_iso_scope, n = 100)
}

iso_countries <- tbl(ethnologue_db, "LanguageCodes") %>%
  distinct(LangID, CountryID) %>%
  rename(iso_639_3 = LangID, country = CountryID)

# anti_join(distinct(lang_iso, iso_639_3, lang_name, country),
#           iso_countries, by = c("iso_639_3", "country"), copy = TRUE)

#' ## Glottolog Matches
glottolog_db <- src_sqlite(here::here("external/lingdata/glottolog.db"))

lang_glottolog <- language_values %>%
  filter(!is.na(glottocode)) %>%
  select(round, variable, lang_name, country, glottocode) %>%
  left_join(tbl(glottolog_db, "languoids"),
            by = c("glottocode" = "glottocode"), copy = TRUE)

#' Check that all Glottolog codes should exist
glotto_bad_codes <- lang_glottolog %>% filter(is.na(name))
if (nrow(glotto_bad_codes)) {
  print("Glottolog languages with invalid codes.")
  print(distinct(glotto_bad_codes, glottocode))
}

#' Glottolog languages should be in the African macroarea unless known not to be
glotto_bad_macroareas <- language_values %>%
  filter(!is.na(glottocode)) %>%
  select(round, variable, lang_name, country, glottocode) %>%
  left_join(tbl(glottolog_db, "macroareas"),
            by = c("glottocode" = "glottocode"), copy = TRUE) %>%
  filter(macroarea != "Africa") %>%
  distinct(glottocode, macroarea) %>%
  left_join(select(tbl(glottolog_db, "languoids"), "glottocode", "name"),
            by = "glottocode", copy = TRUE) %>%
  filter(!glottocode %in% TESTDATA[["glottolog"]][["non_african"]])
if (nrow(glotto_bad_macroareas)) {
  print("Glottolog languages not in the African macroarea were found.")
  print(distinct(bad_glotto_macroareas, glottocode))
}

# anti_join(filter(language_values, !is.na(glottocode)),
#           tbl(glottolog_db, "countries"),
#           by = c("glottocode", "country" = "country_code"), copy = TRUE)

#' ## WALS Matches
wals_db <- src_sqlite(here::here("external/lingdata/wals.db"))

lang_wals <- language_values %>%
  filter(!is.na(wals)) %>%
  select(round, variable, lang_name, country, wals) %>%
  mutate(wals = str_split(wals, " ")) %>%
  unnest() %>%
  left_join(tbl(wals_db, "languages"), by = c("wals" = "wals_code"), copy = TRUE)

#' All WALS codes should exist
bad_wals_code <- filter(lang_wals, is.na(Name))
if (nrow(bad_wals_code)) {
  print("Bad WALS codes were found")
  print(distinct(bad_wals_code, wals))
}

#' WALS languages should be in the African macroarea or acounted for
wals_nonafrican <- filter(lang_wals, macroarea != "Africa") %>%
  # remove known non-African languages
  filter(!wals %in% TESTDATA[["wals"]][["non_african"]])
if (nrow(wals_nonafrican)) {
  print("WALS codes not in the African macroarea found")
  print(distinct(wals_nonafrican, wals, lang_name, Name, macroarea))
}

#' Whitelist the Language Families that appear
wals_bad_family <- filter(lang_wals,
                          !family %in% TESTDATA[["wals"]][["families"]])
if (nrow(wals_bad_family)) {
  print("WALS codes not in the African macroarea found")
  print(distinct(wals_bad_family, wals, lang_name, Name, family))
}

#' All matches must be in the same genus
wals_bad_genus <-
  lang_wals %>%
  group_by(round, variable, lang_name, country) %>%
  summarise(n_genus = length(unique(genus)),
            genus = str_c(sort(unique(genus)), collapse = " "),
            languages = str_c(sort(unique(wals)), collapse = " ")) %>%
  filter(n_genus > 1 )
if (nrow(wals_bad_genus)) {
  print("Matches WALS languages must all be in the same genus.")
  print(wals_bad_genus, n = 100, width = 10000)
}


#' Don't test countries since WALS languages may not correspond to exact
#' languages in the Afrobarometer. There would be many false-negatives.
