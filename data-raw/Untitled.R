library("tidyverse")
library("yaml")

IGNORE <- c("arb", "eng", "fra")

ethnologue <- read_yaml(here::here("data-raw", "ethnologue.yml")) %>%
  imap(~ unname(flatten_chr(map(.x, ~ names(.x))))) %>%
  enframe() %>%
  rename(country = name, expected = value) %>%
  mutate(expected = map(expected, ~ keep(.x, ~ !.x %in% IGNORE)))


lang_values <- read_csv(here::here("data", "language_values.csv"),
                        na = "", col_types = cols(
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
                        )) %>%
  filter(type %in% c("respondent", "interview")) %>%
  mutate(ido_639_3 = str_split(iso_639_3, " ")) %>%
  group_by(round, country) %>%
  summarise(isocodes = list(sort(unique(c(iso_639_3))))) %>%
  right_join(ethnologue, by = "country") %>%
  mutate(missing = map2(expected, isocodes, ~ setdiff(.x, .y))) %>%
  filter(map_int(missing, length) > 0) %>%
  select(round, country, missing) %>%
  mutate(missing = map_chr(missing, str_c, collapse = " "))


