#' ---
#' title: "Match Afrobarometer Languages to ISO 639-3 Language Codes"
#' ---
#'
#' Write Dataset of Afrobarometer Languages
#'
#'
source("src/init.R")

OUTPUT <- project_path("data", "afrobarometer_langs.csv")

weight_vars <- IO$misc_data$afrobarometer$weight_vars %>%
  map_df(as_tibble)

#' For an Afrobarometer Dataset summarize the languages
lang_summary <- function(lang_var, country_var, weight_var, .data) {
  select(.data,
         lang = UQ(sym(lang_var)),
         country = UQ(sym(country_var)),
         withinwt = UQ(sym(weight_var))) %>%
  mutate(country = as.integer(country),
         value = as.integer(lang),
         name = as.character(haven::as_factor(lang)),

  ) %>%
  group_by(country, value, name) %>%
  summarise(n_resp = n(), prop = sum(withinwt)) %>%
  group_by(country) %>%
  mutate(prop = prop / sum(prop)) %>%
  ungroup() %>%
  mutate(variable = UQ(lang_var))
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
  weight_var <- weight_vars %>%
    filter(round == UQ(.round)) %>%
    `[[`("name")

  map_df(lang_vars,
         lang_summary,
         .data = IO$afrobarometer(.round),
         country_var = country_var,
         weight_var = weight_var) %>%
    mutate(round = .round)
}

afrobarometer_langs <- map_df(IO$misc_data$afrobarometer$rounds,
                              afrobarometer_langs_r) %>%
  left_join(afrobarometer_countries,
            by = c("round", "country" = "value")) %>%
  mutate(value = as.integer(value)) %>%
  select(round, variable, value, name, country, iso_alpha2,
         n_resp, prop) %>%
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

  assert_that(is.integer(n_resp))
  assert_that(all(!is.na(n_resp)))
  assert_that(all(n_resp > 0))

  assert_that(is.numeric(prop))
  assert_that(all(!is.na(prop)))
  assert_that(all(prop >= 0 & prop <= 1))

})

prop_sums_to_one <-
  group_by(afrobarometer_langs, round, country, variable) %>%
  summarise(prop = sum(prop)) %>%
  # why is there no element wise tolerance?
  filter(abs(log(prop)) > 0.00001)
assert_that(!nrow(prop_sums_to_one))


# unique keys are unique
assert_that(nrow(distinct(afrobarometer_langs, round,
                          variable, value, country)) ==
              nrow(afrobarometer_langs))

#' Ouptut
afrobarometer_langs %>%
  write_csv(path = OUTPUT, na = "")
