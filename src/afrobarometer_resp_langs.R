#' ---
#' title: "Get languages for all Afrobarometer languages"
#' ---
#'
source("src/init.R")

afrobarometer_lang_variables <- IO$afrobarometer_lang_variables

afrobarometer_country_variables <- IO$afrobarometer_country_variables

# Afrobarometer respondent ID variables
response_variables <-
  tribble(
    ~round, ~name,
    1, "refnumb",
    2, "respno",
    3, "respno",
    4, "RESPNO",
    5, "RESPNO",
    6, "RESPNO"
  )

afrobarometer_filenames <- list("merged_r1_data.sav",
            "merged_r2_data.sav",
            "merged_r3_data.sav",
            "merged_r4_data.sav",
            "merged-round-5-data-34-countries-2011-2013-last-update-july-2015.sav",
            "merged_r6_data_2016_36countries2.sav") %>%
  map_chr(~ project_path("external", "afrobarometer", .x))

#' Create a dataset of all Afrobarometer datasets
afrobarometer_langs_r <-
  function(.round, filenames, afrob_to_wals,
           resp_variables, language_variables, country_variables,
           other = FALSE) {
  .data <- haven::read_sav(filenames[.round])
  afrob_to_wals <- if (other) {
    select(afrob_to_wals, round, variable = question,
           country, value, wals_code)
  } else {
    select(afrob_to_wals, round, variable = question,
           country, value = lang_id, wals_code)
  } %>%
    filter(round == UQ(.round))

  respno_var <- resp_variables %>%
    filter(round == UQ(.round)) %>%
    `[[`("name")
  lang_vars <- language_variables %>%
    filter(other == UQ(other), round == UQ(.round)) %>%
    `[[`("name")
  country_var <- country_variables %>%
    filter(round == UQ(.round)) %>%
    `[[`("name")
  if (length(lang_vars)) {
    out <- select(.data, one_of(c(respno_var, country_var, lang_vars))) %>%
      rename(country = UQ(sym(country_var))) %>%
      mutate(country = as.integer(country)) %>%
      mutate_at(vars(one_of(lang_vars)),
                function(.x) {
                  attributes(.x) <- NULL
                  .x
                }) %>%
      gather(variable, value, -UQ(sym(respno_var)), -country) %>%
      filter(is.integer(value) | value != "") %>%
      filter(is.character(value) | (value > 0 & value < 9000)) %>%
      left_join(afrob_to_wals,
                by = c("variable", "value", "country"))
  }
}

resp_langs <-
  map(1:6,
      afrobarometer_langs_r,
      filenames = afrobarometer_filenames,
      afrob_to_wals = read_csv("data/afrobarometer_to_wals.csv", na = ""),
      resp_variables = response_variables,
      country_variables = IO$afrobarometer_country_variables,
      language_variables = IO$afrobarometer_lang_variables,
      other = FALSE)

resp_langs_other <-
  map(4:6,
      afrobarometer_langs_r,
      filenames = afrobarometer_filenames,
      afrob_to_wals = read_csv("data/afrobarometer_other_to_wals.csv", na = ""),
      resp_variables = response_variables,
      country_variables = IO$afrobarometer_country_variables,
      language_variables = IO$afrobarometer_lang_variables,
      other = TRUE)
