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
  select(.data, one_of(lang_var, country_var)) %>%
    transmute(country = as.integer(UQ(sym(country_var))),
              value = as.integer(UQ(sym(lang_var))),
              name = as.character(haven::as_factor(UQ(sym(lang_var)))),
              variable = UQ(lang_var)
              ) %>%
    distinct()
}

# Miscellaneous data
misc_data <- IO$misc_data

#' Country Abbrevs
afrobarometer_countries <- IO$afrobarometer_countries %>%
  select(round, value, iso_alpha2)

#' Create a dataset of all Afrobarometer datasets
afrobarometer_langs_r <- function(.round) {
  lang_vars <- IO$afrobarometer_lang_variables %>%
    filter(!as.logical(other), round == UQ(.round)) %>%
    `[[`("name")
  country_var <- IO$afrobarometer_country_variables %>%
    filter(round == UQ(.round)) %>%
    `[[`("name")
  map_df(lang_vars,
         lang_summary,
         .data = IO$afrobarometer(.round),
         country_var = country_var) %>%
    mutate(round = .round)
}

afrobarometer_langs <- map_df(IO$misc_data$afrobarometer$rounds,
                              afrobarometer_langs_r) %>%
  left_join(afrobarometer_countries,
            by = c("round", "country" = "value")) %>%
  mutate(value = as.integer(value)) %>%
  select(round, variable, value, name, country, iso_alpha2) %>%
  arrange(round, variable, value, iso_alpha2)

with(afrobarometer_langs, {
  assert_that(all(!is.na(round)))
  assert_that(is_integerish(round))
  assert_that(seteq(unique(round), misc_data$afrobarometer$rounds))

  assert_that(is.character(variable))
  assert_that(all(!is.na(variable)))

  # Lang Id
  assert_that(is.integer(value))
  assert_that(all(value >= -1 & value <= 9999))

  # Lang Name
  assert_that(all(!is.na(name)))
  assert_that(is.character(name))

  # country
  assert_that(is.integer(country))
  assert_that(all(!is.na(country)))

  # ISO Country
  assert_that(is.character(iso_alpha2))
  assert_that(all(!is.na(iso_alpha2)))
  assert_that(all(str_detect(iso_alpha2, "[A-Z]{2}")))

})
# unique keys are unique
assert_that(nrow(distinct(afrobarometer_langs, round,
                          variable, value, country)) ==
              nrow(afrobarometer_langs))

#' Ouptut
afrobarometer_langs %>%
  write_csv(path = OUTPUT, na = "")
