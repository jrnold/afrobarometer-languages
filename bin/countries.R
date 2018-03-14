#' ---
#' title: "Create data/countries.csv"
#' ---
#'
source(here::here("src", "R", "init.R"))

OUTPUT <- project_path("data", "countries.csv")


#' For an Afrobarometer Dataset summarize the languages
#'
#' - round: Afrobarometer round
#' - value: integer. numeric value of the country variable
#' - value: integer. numeric value of the country variable
#'
summarize_countries <- function(round, country, ...) {
  IO$afrobarometer(round) %>%
    select(country = UQ(country)) %>%
    distinct() %>%
    transmute(round = as.integer(UQ(round)),
              value = as.integer(country),
              label = as.character(haven::as_factor(country)))
}

countries <- IO$afrobarometer_variables %>%
  pmap_dfr(summarize_countries) %>%
  arrange(round, value) %>%
  left_join(IO$countries_raw, by = c("label" = "name")) %>%
  select(-iso_alpha3)

write_csv(countries, path = OUTPUT, na = "")

