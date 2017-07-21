#' ---
#' title: "Match Afrobarometer Languages to ISO 639-3 Language Codes"
#' ---
#'
#' Write Dataset of Afrobarometer Languages
#'
#'
library("tidyverse")
library("rprojroot")
library("stringr")
library("magrittr")

OUTPUT <- find_rstudio_root_file("data", "afrobarometer_langs.csv")

INPUT <-
  c("merged_r1_data.sav",
    "merged_r2_data.sav",
    "merged_r3_data.sav",
    "merged_r4_data.sav",
    "merged-round-5-data-34-countries-2011-2013-last-update-july-2015.sav",
    "merged_r6_data_2016_36countries2.sav") %>%
  map(function(x) find_rstudio_root_file("external", "afrobarometer", x))


split_lang_names <- function(x) {
  x <- str_split(x, "[(/]")
  x <- map(x, str_replace_all, "[)]", "")
  x
}

metadata <-
  list(
    lang_id = str_c("Language number, as in the Afrobarometer documentation"),
    question = str_c("Question number in Afrobarometer"),
    lang_name = str_c("Language name, as in the Afrobarometer documentation"),
    countries = str_c("Countries in which the language appears on the ",
                      "questionnaire."),
    languages = str_c("Distinct language names. ",
                      "This variable splits `lang_name` when there are multiple",
                      " languages listed."),
    is_language = str_c("Does the value refer to a language",
                        "as opposed to values like \"Other\", \"No response\""),
    NULL = str_c("Languages appearing in Afrobarometer round 6.\n",
                 "This data frame include the language numbers, and names of ",
                 "languages in Q2 (speaker language) and ",
                 "Q103 (survey language)")
  )

afrobarometer_langs <- function(x, rnd) {
  x <- haven::read_sav(x)
  if (rnd %in% 1) {
    transmute(x,
              lang_id = as.integer(language),
              lang_name = as.character(haven::as_factor(language)),
              question = "language",
              country = str_sub(casenumb, 1, 3)) %>%
      distinct() %>%
      group_by(question, lang_id, lang_name) %>%
      summarise(countries = list(country)) %>%
      mutate(round = paste0("r", rnd)) %>%
      ungroup()
  } else if (rnd %in% 2) {
    x %>% {
      bind_rows(
        transmute(.,
                  lang_id = as.integer(q83),
                  lang_name = as.character(haven::as_factor(q83)),
                  question = "q83",
                  country = str_sub(respno, 1, 3)
        ),
        transmute(.,
                  lang_id = as.integer(q97),
                  lang_name = as.character(haven::as_factor(q97)),
                  question = "q97",
                  country = str_sub(respno, 1, 3)
        )
      )
    } %>%
    distinct() %>%
    group_by(question, lang_id, lang_name) %>%
    summarise(countries = list(country)) %>%
    mutate(round = paste0("r", rnd)) %>%
    ungroup()
  } else if (rnd %in% 3) {
    x %>% {
      bind_rows(
        transmute(.,
                  lang_id = as.integer(q3),
                  lang_name = as.character(haven::as_factor(q3)),
                  question = "q3",
                  country = str_sub(respno, 1, 3)
        ),
        transmute(.,
                  lang_id = as.integer(q103),
                  lang_name = as.character(haven::as_factor(q103)),
                  question = "q103",
                  country = str_sub(respno, 1, 3)
        )
      )
    } %>%
    distinct() %>%
    group_by(question, lang_id, lang_name) %>%
    summarise(countries = list(country)) %>%
    mutate(round = paste0("r", rnd)) %>%
    ungroup()
  } else if (rnd %in% 4) {
    x %>% {
      bind_rows(
        transmute(.,
                  lang_id = as.integer(Q3),
                  lang_name = as.character(haven::as_factor(Q3)),
                  question = "Q3",
                  country = str_sub(RESPNO, 1, 3)
        ),
        transmute(.,
                  lang_id = as.integer(Q103),
                  lang_name = as.character(haven::as_factor(Q103)),
                  question = "Q103",
                  country = str_sub(RESPNO, 1, 3)
        )
      )
    } %>%
      distinct() %>%
      group_by(question, lang_id, lang_name) %>%
      summarise(countries = list(country)) %>%
      mutate(round = paste0("r", rnd)) %>%
      ungroup()
  } else if (rnd %in% c(5, 6)) {
    x %>% {
      bind_rows(
        transmute(.,
          lang_id = as.integer(Q2),
          lang_name = as.character(haven::as_factor(Q2)),
          question = "Q2",
          country = str_sub(RESPNO, 1, 3)
        ),
        transmute(.,
          lang_id = as.integer(Q103),
          lang_name = as.character(haven::as_factor(Q103)),
          question = "Q103",
          country = str_sub(RESPNO, 1, 3)
        )
      )
    } %>%
    distinct() %>%
    group_by(question, lang_id, lang_name) %>%
    summarise(countries = list(country)) %>%
    mutate(round = paste0("r", rnd)) %>%
    ungroup()
  }
}

write_afrobarometer_langs <- function(x, dst) {
  x %>%
  mutate(countries = map_chr(countries, function(x) str_c(x, collapse = " "))) %>%
  select(round, question, lang_id, lang_name, countries) %>%
  arrange(round, question, lang_id) %>%
  write_csv(path = dst)
}

INPUT %>%
  {map2_df(., seq_along(.), afrobarometer_langs)} %>%
  write_afrobarometer_langs(OUTPUT)
