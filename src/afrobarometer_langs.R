#' ---
#' title: "Match Afrobarometer Languages to ISO 639-3 Language Codes"
#' ---
#'
#' Write Dataset of Afrobarometer Languages
#'
#'
library("tidyverse")
library("rprojroot")
library("stringr")
library("magrittr")
library("assertthat")

OUTPUT <- find_rstudio_root_file("data", "afrobarometer_langs.csv")

INPUT <-
  list("r1" = list("external", "afrobarometer", "merged_r1_data.sav"),
    "r2" = list("external", "afrobarometer", "merged_r2_data.sav"),
    "r3" = list("external", "afrobarometer", "merged_r3_data.sav"),
    "r4" = list("external", "afrobarometer", "merged_r4_data.sav"),
    "r5" = list("external", "afrobarometer", "merged-round-5-data-34-countries-2011-2013-last-update-july-2015.sav"),
    "r6" = list("external", "afrobarometer", "merged_r6_data_2016_36countries2.sav"),
    "countries" = list("data-raw", "afrobarometer_countries.csv")) %>%
  map_chr(function(x) invoke(find_rstudio_root_file, x))

library("rlang")
lang_summary <- function(lang_var, country_var, .data) {
  langs <- enframe(attr(.data[[lang_var]], "labels")) %>%
    mutate(question = UQ(lang_var))
  countries <- transmute(.data,
                         countries = as.integer(UQ(sym(country_var))),
                         value = as.integer(UQ(sym(lang_var)))) %>%
    distinct()
  left_join(langs, countries, by = "value")
}

afrobarometer_langs <- function(filename, rnd) {
  lang_vars <- switch(rnd,
    r1 = "language",
    r2 = c("q83", "q97", "q110"),
    r3 = c("q3", "q103", "q114"),
    r4 = c("Q3", "Q103", "Q114"),
    r5 = ,
    r6 = c("Q2", "Q103", "Q116")
  )
  country_var <- switch(rnd,
    r1 = ,
    r2 = ,
    r3 = "country",
    r4 = ,
    r5 = ,
    r6 = "COUNTRY"
  )
  map_df(lang_vars,
         lang_summary, .data = haven::read_sav(filename),
         country_var = country_var) %>%
    mutate(round = rnd)
}

write_afrobarometer_langs <- function(x, dst) {
  x %>%
  mutate(countries = map_chr(countries, function(x) str_c(x, collapse = " "))) %>%
  select(round, question, lang_id, lang_name, countries) %>%
  arrange(round, question, lang_id) %>%
  write_csv(path = dst)
}

afrobarometer_countries <-
  read_csv(INPUT[["countries"]],
           col_types = cols_only(
             round = col_character(),
             value = col_integer(),
             iso_alpha2 = col_character()
           ),
           na = "")

afrobarometer_langs <-
  INPUT[str_subset(names(INPUT), "^r")] %>%
  {map2_df(., names(.), afrobarometer_langs)} %>%
  left_join(afrobarometer_countries,
            by = c("round", "countries" = "value")) %>%
  mutate(value = as.integer(value)) %>%
  group_by(round, question, value, name) %>%
  summarise(countries = list(sort(unique(iso_alpha2)))) %>%
  arrange(round, question, value) %>%
  mutate(countries = map_chr(countries, paste, collapse = " "),
         countries = if_else(str_trim(countries) == "",
                             NA_character_,
                             countries))

ROUNDS <- paste0("r", 1:6)

seteq <- function(x, y) {
  !length(setdiff(x, y)) && !length(setdiff(y, x))
}

with(afrobarometer_langs, {
  assert_that(all(!is.na(round)))
  assert_that(is.character(round))
  assert_that(seteq(unique(round), ROUNDS))

  assert_that(is.character(question))
  assert_that(all(!is.na(question)))

  # Lang Id
  assert_that(is.integer(value))
  assert_that(all(value >= -1 & value <= 9999))

  # Lang Name
  assert_that(all(!is.na(name)))
  assert_that(is.character(name))

  # Iso Code
  assert_that(is.character(countries))
  assert_that(all(str_detect(na.omit(countries), "[A-Z]{2}( [A-Z]{2})*")))
})

#' Ouptut
afrobarometer_langs %>%
  write_csv(path = OUTPUT, na = "")
