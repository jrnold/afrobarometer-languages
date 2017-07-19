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
  find_rstudio_root_file("external", "afrobarometer",
                         "merged_r6_data_2016_36countries2.sav")


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

afrobarometer_langs <- function(input, output) {

  afrobarometer <- haven::read_sav(input)

  q2 <-
    transmute(
      afrobarometer,
      lang_id = as.integer(Q2),
      lang_name = as.character(haven::as_factor(Q2)),
      question = "Q2",
      country = str_sub(RESPNO, 1, 3)
    ) %>%
    distinct()

  q103 <-
    transmute(
      afrobarometer,
      lang_id = as.integer(Q103),
      lang_name = as.character(haven::as_factor(Q103)),
      question = "Q103",
      country = str_sub(RESPNO, 1, 3)
    ) %>%
    distinct()

  bind_rows(q2, q103) %>%
    group_by(lang_id, question, lang_name) %>%
    summarise(countries = list(country)) %>%
    ungroup() %>%
    #split languages
    mutate(languages = split_lang_names(lang_name),
           is_language = lang_id > 0 & lang_id < 9000)
}

write_afroarometer_langs <- function(x, dst) {
  x %>%
  mutate(countries = map_chr(countries, function(x) str_c(x, collapse = " ")),
         languages = map_chr(languages, function(x) {
           str_c(str_trim(x), collapse = ";")
         })) %>%
  select(question, lang_id, lang_name, everything()) %>%
  arrange(question, lang_id) %>%
  write_csv(path = dst)
}

afrobarometer_langs(INPUT) %>%
  write_afroarometer_langs(OUTPUT)
