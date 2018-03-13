library("readr")
library("tidyverse")
library("stringr")

# comment:

iso <- read_csv("data/afrobarometer_to_iso_639_3.csv", na = "") %>%
  mutate(lang_name = str_to_lower(lang_name)) %>%
  group_by(lang_name, iso_alpha2) %>%
  summarise(iso_639_3 = str_c(sort(unique(iso_639_3)), collapse = " "))

iso_other <- read_csv("data/afrobarometer_other_to_iso_639_3.csv", na = "") %>%
  mutate(lang_name = str_to_lower(lang_name)) %>%
  anti_join(iso, by = c("lang_name", "iso_alpha2")) %>%
  group_by(lang_name, iso_alpha2) %>%
  summarise(iso_639_3 = str_c(sort(unique(iso_639_3)), collapse = " "))

glottolog <- read_csv("data/afrobarometer_to_glottolog.csv", na = "") %>%
  mutate(lang_name = str_to_lower(lang_name)) %>%
  filter(!is.na(glottocode)) %>%
  group_by(lang_name, iso_alpha2) %>%
  summarise(glottocode = str_c(sort(unique(glottocode)), collapse = " "))

glottolog_other <- read_csv("data/afrobarometer_other_to_glottolog.csv", na = "") %>%
  mutate(lang_name = str_to_lower(lang_name)) %>%
  filter(!is.na(glottocode)) %>%
  anti_join(iso, by = c("lang_name", "iso_alpha2")) %>%
  group_by(lang_name, iso_alpha2) %>%
  summarise(glottocode = str_c(sort(unique(glottocode)), collapse = " "))

full_join(bind_rows(glottolog, glottolog_other),
          bind_rows(iso, iso_other),
          by = c("lang_name", "iso_alpha2")) %>%
  group_by(lang_name, glottocode, iso_639_3) %>%
  summarise(countries = list(sort(iso_alpha2))) %>%
  ungroup() %>%
  arrange(lang_name) %>%
  mutate(iso_639_3 = str_split(iso_639_3, " "),
         glottocode = str_split(glottocode, " ")) %>%
  split(seq_len(nrow(.))) %>%
  unname() %>%
  yaml::as.yaml() %>%
  cat(file = "data-raw/links.yml")

