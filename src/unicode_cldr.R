library(tidyverse)
library(xml2)
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

language_pops <- extract_languages(langdata) %>%
  mutate_at(vars(populationPercent, writingPercent, literacyPercent),
            as.numeric)
  
references <- extract_references(langdata) %>%
  mutate(reference_text = text, reference_uri = uri)


