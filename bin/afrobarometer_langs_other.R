#' ---
#' title:  Afrobarometer "Other" Languages
#' ---
#'
#' List of all values of variables with free text fields for
#' entering values of the language when "other" is selected.
#'
source(here::here("src", "R", "init.R"))

OUTPUT <- project_path("data", "afrobarometer_langs_other.csv")

afrobarometer_countries <- IO$afrobarometer_countries %>%
  select(round, country = value, iso_alpha2)


#' For an Afrobarometer Dataset summarize the languages
lang_summary <- function(lang_var, country_var, weight_var, .data) {
  select(.data,
         value = UQ(sym(lang_var)),
         country = UQ(sym(country_var)),
         withinwt = UQ(sym(weight_var))) %>%
    filter(value != "") %>%
    mutate(country = as.integer(country)) %>%
    group_by(country, value) %>%
    summarise(n_resp = n(), prop = sum(withinwt)) %>%
    group_by(country) %>%
    mutate(prop = prop / sum(prop)) %>%
    ungroup() %>%
    mutate(variable = UQ(lang_var))
}

#' Create a dataset of all Afrobarometer datasets
afrobarometer_langs_other_r <- function(.round) {
  lang_vars <- IO$afrobarometer_lang_variables %>%
    filter(as.logical(other), round == UQ(.round)) %>%
    `[[`("name")

  if (length(lang_vars)) {
    country_var <- IO$afrobarometer_variables %>%
      filter(round == UQ(.round)) %>%
      `[[`("country")
    weight_var <- IO$afrobarometer_variables %>%
      filter(round == UQ(.round)) %>%
      `[[`("withinwt")

    map_df(lang_vars,
           lang_summary,
           .data = IO$afrobarometer(.round),
           country_var = country_var,
           weight_var = weight_var) %>%
      mutate(round = .round)
  }
}


afrobarometer_langs_other <- map_df(IO$misc_data$afrobarometer$rounds,
                                    afrobarometer_langs_other_r) %>%
  left_join(afrobarometer_countries, by = c("round", "country")) %>%
  select(round, variable, country, value, iso_alpha2, n_resp, prop) %>%
  arrange(round, variable, value)

afrobarometer_langs_other %>%
  write_csv(OUTPUT, na = "")
