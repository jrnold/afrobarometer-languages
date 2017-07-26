#' ---
#' title:  Afrobarometer "Other" Languages
#' ---
#'
#' List of all values of variables with free text fields for
#' entering values of the language when "other" is selected.
#'
source("src/init.R")

OUTPUT <- project_path("data", "afrobarometer_langs_other.csv")

misc_data <- IO$misc_data

afrob_rounds <- misc_data$afrobarometer$rounds

afrobarometer_langs_other_r <- function(.round) {
  misc_data <- IO$misc_data
  lang_vars <- misc_data$afrobarometer$lang_other_variables$values[[.round]]
  if (length(lang_vars)) {
    country_var <-
      misc_data$afrobarometer$country_variables$values[[.round]]
    IO$afrobarometer(.round) %>%
      select(one_of(lang_vars), country = UQ(sym(country_var))) %>%
      mutate(country = as.integer(country)) %>%
      mutate_at(vars(one_of(lang_vars)),
                funs(as.character)) %>%
      gather(question, value, -country) %>%
      filter(value != "") %>%
      count(country, question, value) %>%
      mutate(round = !!.round)
  }
}

afrobarometer_langs_other <- map_df(IO$misc_data$afrobarometer$rounds, afrobarometer_langs_other_r) %>%
  left_join(afrobarometer_countries,
            by = c("round", "country" = "value")) %>%
  select(round, question, country, value, iso_alpha2) %>%
  arrange(round, question, value)

afrobarometer_langs_other %>%
  write_csv(OUTPUT, na = "")
