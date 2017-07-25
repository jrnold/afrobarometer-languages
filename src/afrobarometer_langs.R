#' ---
#' title: "Match Afrobarometer Languages to ISO 639-3 Language Codes"
#' ---
#'
#' Write Dataset of Afrobarometer Languages
#'
#'
source("src/init.R")

#' For an Afrobarometer Dataset summarize the languages
lang_summary <- function(lang_var, country_var, .data) {
  langs <- enframe(attr(.data[[lang_var]], "labels")) %>%
    mutate(question = UQ(lang_var))
  countries <- transmute(.data,
                         countries = as.integer(UQ(sym(country_var))),
                         value = as.integer(UQ(sym(lang_var)))) %>%
    distinct()
  left_join(langs, countries, by = "value")
}

#' Create a dataset of all Afrobarometer datasets
afrobarometer_langs <- function(.round) {
  misc_data <- misc_data()
  map_df(lang_vars,
         lang_summary,
         .data = afrobarometer(.round),
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
