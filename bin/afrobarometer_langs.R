#' ---
#' title: "Match Afrobarometer Languages to ISO 639-3 Language Codes"
#' ---
#'
#' Write Dataset of Afrobarometer Languages
#'
#'
source(here::here("src", "R", "init.R"))

OUTPUT <- project_path("data", "afrobarometer_langs.csv")

#' For an Afrobarometer Dataset summarize the languages
lang_summary <- function(lang_var, country_var, weight_var, .data) {
  select(.data,
         lang = UQ(sym(lang_var)),
         country = UQ(sym(country_var)),
         withinwt = UQ(sym(weight_var))) %>%
  mutate(country = as.integer(country),
         lang_id = as.integer(lang),
         lang_name = as.character(haven::as_factor(lang)),
  ) %>%
  group_by(country, lang_id, lang_name) %>%
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
  select(round, value, iso_alpha2) %>%
  rename(country = value)

#' Create a dataset of all Afrobarometer datasets
afrobarometer_langs_r <- function(.round) {
  lang_vars <- IO$afrobarometer_lang_variables %>%
    filter(!as.logical(other), round == UQ(.round)) %>%
    `[[`("name")
  country_var <- IO$afrobarometer_variables %>%
    filter(round == UQ(.round)) %>%
    `[[`("country")
  weight_var <- IO$afrobarometer_variables  %>%
    filter(round == UQ(.round)) %>%
    `[[`("withinwt")

  map_df(lang_vars,
         lang_summary,
         .data = IO$afrobarometer(.round),
         country_var = country_var,
         weight_var = weight_var) %>%
    mutate(round = .round)
}

afrobarometer_langs <- map_df(IO$misc_data$afrobarometer$rounds,
                              afrobarometer_langs_r) %>%
  left_join(afrobarometer_countries, by = c("round", "country")) %>%
  mutate(lang_id = as.integer(lang_id)) %>%
  select(round, variable, lang_id, lang_name, country, iso_alpha2,
         n_resp, prop) %>%
  arrange(round, variable, lang_id, iso_alpha2)

with(afrobarometer_langs, {
  assert_that(all(!is.na(round)))
  assert_that(is_integerish(round))
  assert_that(seteq(unique(round), misc_data$afrobarometer$rounds))

  assert_that(is.character(variable))
  assert_that(all(!is.na(variable)))

  # Lang Id
  assert_that(is.integer(lang_id))
  assert_that(all(lang_id >= -1 & lang_id <= 9999))

  # Lang Name
  assert_that(all(!is.na(lang_name)))
  assert_that(is.character(lang_name))

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
assert_that(nrow(distinct(afrobarometer_langs, round, variable,
                          lang_id, country)) ==
              nrow(afrobarometer_langs))

#' Ouptut
afrobarometer_langs %>%
  write_csv(path = OUTPUT, na = "")
