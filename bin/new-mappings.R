library("readr")

iso639 <- read_csv("data/afrobarometer_to_iso_639_3.csv", na = "") %>%
  mutate(lang_name = str_to_lower(lang_name)) %>%
  group_by(lang_name, iso_alpha2) %>%
  summarise(iso_639_3 = str_c(sort(unique(iso_639_3)), collapse = " "))

iso_639_3_other <- read_csv("data/afrobarometer_other_to_iso_639_3.csv", na = "") %>%
  mutate(lang_name = str_to_lower(lang_name)) %>%
  filter(!is.na(iso_639_3)) %>%
  anti_join(iso639, by = c("lang_name", "iso_alpha2")) %>%
  group_by(lang_name, iso_alpha2) %>%
  summarise(iso_639_3 = if_else(all(is.na(iso_639_3)), NA_character_,
                                 str_c(sort(unique(iso_639_3)), collapse = " ")))



glottolog <-  read_csv("data/afrobarometer_to_glottolog.csv", na = "") %>%
  mutate(lang_name = str_to_lower(lang_name)) %>%
  filter(!is.na(glottocode)) %>%
  group_by(lang_name, iso_alpha2) %>%
  summarise(glottocode = if_else(all(is.na(glottocode)), NA_character_,
                                 str_c(sort(unique(glottocode)), collapse = " ")))

glottolog_other <- read_csv("data/afrobarometer_other_to_glottolog.csv", na = "") %>%
  mutate(lang_name = str_to_lower(lang_name)) %>%
  filter(!is.na(glottocode)) %>%
  anti_join(glottolog, by = c("lang_name", "iso_alpha2")) %>%
  group_by(lang_name, iso_alpha2) %>%
  summarise(glottocode = if_else(all(is.na(glottocode)), NA_character_,
                                 str_c(sort(unique(glottocode)), collapse = " ")))

full_join(bind_rows(iso639, iso_639_3_other),
          bind_rows(glottolog, glottolog_other),
          by = c("lang_name", "iso_alpha2")) %>%
  group_by(lang_name, iso_639_3, glottocode) %>%
  summarise(countries = list(sort(iso_alpha2))) %>%
  ungroup() %>%
  mutate(glottocode = str_split(glottocode, " "),
         iso_639_3 = str_split(iso_639_3, " ")) %>%
  arrange(lang_name) %>%
  split(seq_len(nrow(.))) %>%
  unname() %>%
  yaml::as.yaml() %>%
  cat(file= "data-raw/afrobarometer-mappings.yml")
