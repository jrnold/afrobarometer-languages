source("src/init.R")
filter <- dplyr::filter

OUTPUT <- project_path("data", "afrobarometer_respno_to_langs.csv")

#' WALS language info
language_variables <- IO$afrobarometer_lang_variables

country_variables <- IO$afrobarometer_country_variables

afrobarometer_to_wals <- IO$afrobarometer_to_wals

afrobarometer_other_to_wals <- IO$afrobarometer_other_to_wals

afrobarometer_to_iso <- IO$afrobarometer_to_iso

afrobarometer_other_to_iso <- IO$afrobarometer_other_to_iso

# Afrobarometer respondent ID variables
respondent_variables <- IO$afrobarometer_respno_variables

map_lang_vars <- function(.data, x, mappings, lang_vars,
                          country_var, respno_var) {
  if (length(lang_vars)) {
    out <- select(.data, one_of(c(respno_var, country_var,
                                  names(lang_vars)))) %>%
      rename(country = UQ(sym(country_var)),
             respno = UQ(sym(respno_var))) %>%
      mutate(country = as.integer(country),
             respno = as.character(respno)) %>%
      # need this to drop attributes and avoid warnings
      mutate_at(vars(one_of(names(lang_vars))),
                function(.x) {
                  attributes(.x) <- NULL
                  .x
                }) %>%
      # conver to long
      gather(variable, value, -respno, -country) %>%
      filter(is.integer(value) | value != "") %>%
      # add WALS codes
      left_join(mappings,
                by = c("variable", "value", "country")) %>%
      select(respno, country, variable, UQ(sym(x))) %>%
      filter(!is.na(UQ(sym(x)))) %>%
      # convert variable names to type (interview, respondent)
      mutate(variable = UQ(lang_vars)[variable])
    out
  }
}

respno_to_langs_round <- function(.round) {
  .data <- IO$afrobarometer(.round)

  lang_vars <- language_variables %>%
    filter(round == UQ(.round))
  lang_vars <- list("default" =
                      deframe(select(filter(lang_vars, !other), name, type)),
                    "other" =
                      deframe(select(filter(lang_vars, other), name, type)))

  respno_var <- respondent_variables %>%
    filter(round == UQ(.round)) %>%
    `[[`("name")

  country_var <- country_variables %>%
    filter(round == UQ(.round)) %>%
    `[[`("name")

  f <- partial(map_lang_vars,
               country_var = country_var,
               respno_var = respno_var,
               .data = .data)

  to_wals <- f(
    x = "wals_code",
    mapping =
      select(filter(afrobarometer_to_wals, round == UQ(.round)),
             variable = question, country, value = lang_id, wals_code),
    lang_vars = lang_vars[["default"]]
  )

  to_iso <- f(
    x = "iso_639_3",
    mapping =
     select(filter(afrobarometer_to_iso, round == UQ(.round)),
            variable = question, country,
            value = lang_id, iso_639_3),
    lang_vars = lang_vars[["default"]]
  )

  if (length(lang_vars[["other"]])) {
    to_iso_other <- f(
      x = "iso_639_3",
      mapping =
        select(filter(afrobarometer_other_to_iso,
                      round == UQ(.round)),
               variable = question, country, value, iso_639_3),
      lang_vars = lang_vars[["other"]]
    )

    if (!is.null(to_iso_other)) {
      to_iso <- bind_rows(
        to_iso,
        anti_join(to_iso_other, to_iso, by = "respno")
      )
    }

    to_wals_other <- f(
      x = "wals_code",
      mapping =
        select(filter(afrobarometer_other_to_wals, round == UQ(.round)),
               variable = question, country, value, wals_code),
      lang_vars = lang_vars[["other"]]
    )

    if (!is.null(to_wals_other)) {
      to_wals <- bind_rows(
        to_wals,
        anti_join(to_wals_other, to_wals, by = "respno")
      )
    }
  }

  full_join(
    to_wals %>%
      group_by(respno, variable) %>%
      summarise(wals_code = str_c(wals_code, collapse = " ")),
    to_iso %>%
      group_by(respno, variable) %>%
      summarise(iso_639_3 = str_c(iso_639_3, collapse = " ")),
    by = c("respno", "variable")
  ) %>%
    mutate(round = UQ(.round))
}

respno_to_langs <- function() {
  map_df(1:6, respno_to_langs_round) %>%
    select(round, respno, variable, iso_639_3, wals_code) %>%
    arrange(round, variable, respno)
}

afrobarometer_respno_to_langs <- respno_to_langs()

#' Check Data

#' check primary key
assert_that(nrow(distinct(afrobarometer_respno_to_langs,
                          round, respno, variable)) ==
              nrow(afrobarometer_respno_to_langs))

#' there should be data
assert_that(nrow(afrobarometer_respno_to_langs) > 100000)

rlang::eval_tidy(quo({
  assert_that(all(!is.na(.data$round)))
  assert_that(is.integer(.data$round))
  assert_that(all(.data$round %in% 1:6))

  assert_that(all(!is.na(variable)))
  assert_that(is.character(variable))
  assert_that(all(variable %in% c("respondent", "interview", "interviewer")))

  assert_that(all(!is.na(respno)))
  assert_that(is.character(respno))
  # pattern that matches all the respno
  assert_that(all(str_detect(respno, "^([0-9]+|([A-Z]|[a-z]){3}[0-9]{3,4}[`.N]?)$")))

  assert_that(all(!is.na(iso_639_3)))
  assert_that(is.character(iso_639_3))
  assert_that(all(str_detect(iso_639_3, "^[a-z]{3}( [a-z]{3})*$")))

  assert_that(is.character(wals_code))
  assert_that(all(str_detect(na.omit(wals_code), "^[a-z]{2,3}( [a-z]{2,3})*$")))

}), data = afrobarometer_respno_to_langs)


#' Write to output
afrobarometer_respno_to_langs %>%
   write_csv(path = OUTPUT, na = "")
