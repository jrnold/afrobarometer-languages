source("src/init.R")
library(tidyverse)
library(xml2)
library(stringr)

UNICODE_CLDR <- "http://unicode.org/repos/cldr/trunk/common/supplemental/supplementalData.xml"
langdata <- read_xml(url(UNICODE_CLDR))

xml_attrs_df <- function(x) {
  as_tibble(as.list(xml_attrs(x)))
}

process_territory <- function(x) {
  map_df(xml_find_all(x, ".//languagePopulation"), process_language_pop) %>%
    mutate(territory = UQ(xml_attr(x, "type")))
}

process_language_pop <- function(x) {
  xml_attrs_df(x) %>%
    rename(language = type)
}

extract_languages <- function(x) {
  map_df(xml_find_all(x, ".//territory"), process_territory)
}


extract_languages <- function(x) {
  map_df(xml_find_all(x, ".//territory"), process_territory)
}

process_reference <- function(x) {
  out <- xml_attrs_df(x)
  out$text <- xml_text(x)
  out
}

extract_references <- function(x) {
  map_df(xml_find_all(x, ".//reference"), process_reference)
}

references <- extract_references(langdata) %>%
  mutate(reference_text = text, reference_uri = uri)

cldr_language_pops <- extract_languages(langdata) %>%
  mutate_at(vars(populationPercent, writingPercent, literacyPercent),
            as.numeric) %>%
  mutate(language = str_replace(language, "_.*$", "")) %>%
  select(territory, language, populationPercent) %>%
  left_join(select(IO$iso_639_3_codes, iso_639_3 = Id, language = Part1),
            by = "language") %>%
  mutate(iso_639_3 = coalesce(iso_639_3, language)) %>%
  left_join(select(IO$iso_639_3_codes, iso_639_3 = Id, Ref_Name),
            by = "iso_639_3")

IO$afrobarometer_countries %>%
  left_join(cldr_language_pops, by = c("iso_alpha2" = "territory")) %>%
  anti_join(IO$afrobarometer_to_iso, by = c("round", "variable", "iso_alpha2", "iso_639_3"))

# get macrolangs for all individual languages
ab_to_iso_I <- inner_join( IO$iso_639_3_macrolanguages,
                           IO$afrobarometer_to_iso,
                          by = c("M_Id" = "iso_639_3")) %>%
  select(-M_Id) %>%
  rename(iso_639_3 = I_Id) %>%
  distinct()

# get individual langs for all macrolangs
ab_to_iso_M <- inner_join( IO$iso_639_3_macrolanguages,
                           IO$afrobarometer_to_iso,
                          by = c("I_Id" = "iso_639_3")) %>%
  select(-I_Id) %>%
  rename(iso_639_3 = M_Id) %>%
   distinct()
ab2iso <-
  bind_rows(IO$afrobarometer_to_iso, ab_to_iso_M, ab_to_iso_I) %>%
  select(round, iso_alpha2, variable, iso_639_3) %>%
  distinct()


# Proportions for major languages in Afrobarometer Countries
# as reported in the Unicode CLDR
IO$afrobarometer_countries %>%
  left_join(cldr_language_pops, by = c("iso_alpha2" = "territory")) %>%
  anti_join(ab2iso, by = c("round", "iso_alpha2", "iso_639_3")) %>%
  filter(! language %in% c("ar", "en", "fr")) %>%
  select( populationPercent, iso_639_3, Ref_Name, language, iso_alpha2, round) %>%
  arrange(-populationPercent) %>%
  write_csv(path = "unicode_cldr.csv", na = "") %>%
  print(n = 1000, width = 1000)

# Proportions for major languages in Afrobarometer Countries
# as reported in the Unicode CLDR
cldr_language_pops %>%
  filter(territory %in% unique(IO$afrobarometer_countries$iso_alpha2)) %>%
  select(territory, language, populationPercent)

# Proportions of Afrobarometer Languages

IO$afrobarometer_to_iso %>%
  group_by(round, variable, lang_id, lang_name, iso_alpha2) %>%
  summarise(iso_639_3 = str_c(sort(unique(iso_639_3)), collapse = " ")) %>%
  inner_join(select(IO$afrobarometer_lang_variables,
                    round, variable = name, type),
             by = c("round", "variable")) %>%
  filter(type == "respondent") %>%
  select(-type) %>%
  left_join(select(IO$afrobarometer_langs,
                   round, variable, lang_id = value, iso_alpha2,
                   n_resp, prop),
            by = c("round", "variable", "lang_id", "iso_alpha2")) %>%
  select(iso_alpha2, round, lang_name, iso_639_3, prop, n_resp) %>%
  arrange(iso_alpha2, round, -prop)

