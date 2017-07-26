#' ---
#' title: "Match Afrobarometer Languages to ISO 639-3 Language Codes"
#' ---
#'
#' Write Dataset of Afrobarometer Languages
#'
#'
source("src/init.R")

OUTPUT <- project_path("data", "afrobarometer_langs.csv")

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

# Miscellaneous data
misc_data <- IO$misc_data

#' Country Abbrevs
afrobarometer_countries <- IO$afrobarometer_countries %>%
  select(round, value, iso_alpha2)

#' Create a dataset of all Afrobarometer datasets
afrobarometer_langs_r <- function(.round) {
  misc_data <- IO$misc_data
  lang_vars <- map_chr(misc_data$afrobarometer$language_variables$values[[.round]], "name")
  country_var <-
    misc_data$afrobarometer$country_variables$values[[.round]]
  map_df(lang_vars,
         lang_summary,
         .data = IO$afrobarometer(.round),
         country_var = country_var) %>%
    mutate(round = .round)
}


afrobarometer_langs <- map_df(IO$misc_data$afrobarometer$rounds, afrobarometer_langs_r) %>%
  left_join(afrobarometer_countries,
            by = c("round", "countries" = "value")) %>%
  mutate(value = as.integer(value)) %>%
  group_by(round, question, value, name) %>%
  summarise(countries = list(sort(unique(iso_alpha2)))) %>%
  mutate(countries = map_chr(countries, paste, collapse = " "),
         countries = if_else(str_trim(countries) == "",
                             NA_character_,
                             countries)) %>%
  select(round, question, value, name, countries) %>%
  arrange(round, question, value)

with(afrobarometer_langs, {
  assert_that(all(!is.na(round)))
  assert_that(is.character(round))
  assert_that(seteq(unique(round), misc_data$afrobarometer$rounds))

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
  assert_that(all(str_detect(na.omit(countries),
                             "[A-Z]{2}( [A-Z]{2})*")))
})

#' Ouptut
afrobarometer_langs %>%
  write_csv(path = OUTPUT, na = "")
