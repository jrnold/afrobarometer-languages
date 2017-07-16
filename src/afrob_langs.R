#' ---
#' title: "Match Afrobarometer Languages to ISO 639-3 Language Codes"
#' ---
#' 
#' This script creates a frame of Afrobarometer Round 6 languages.
#' 
#' 
filter <- dplyr::filter

#' 
#' # Afrobarometer Languages
#' 
#' Generate a dataset with an observation for each unique combination of (country, language id number, langauge name).
#' In the country-level datasets it is unclear whether the names and ids of languages are consistent. So, make matches
#' for each country. Additionally, keeping the country in which the language is spoken is useful for merging since
#' the Ethnologue indicates which countries the language is spoken.
#' 
#' All matching is done by **country** and **language name**.
#' The numeric language identifier is ignored because it not not consistent even **within** country and question---meaning that the same numeric language id can refer to different languages in Q2 (language of respondant) and Q103 of the survey.
#' 
## ----afrob_langs-------------------------------------------------
AFROBAROMETER_FILE <- project_path("data-raw", "external", "afrobarometer",
                                   "merged_r6_data_2016_36countries2.sav")
afrobarometer <- haven::read_sav(AFROBAROMETER_FILE)


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

afrobarometer_split_lang_names <- function(x) {
  x <- str_split(x, "[(/]")
  x <- map(x, str_replace_all, "[)]", "")
  x
}

afrob_langs <-
  bind_rows(q2, q103) %>%
  group_by(lang_id, question, lang_name) %>%
  summarise(countries = list(country)) %>%
  ungroup() %>%
  # remove missing languages 
  unite(qlang_id, question, lang_id, remove = FALSE) %>%
  #split languages
  mutate(languages = afrobarometer_split_lang_names(lang_name),
         is_language = lang_id > 0 | lang_id < 9000)

comments <- 
  list(
    qlang_id = str_c("Unique language identifier combining Afrobarometer ",
                     "question number and the language number. "),
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
afrob_langs %<>% add_description(comments)


#' Testing
data_exists(afrob_langs)


#' 
#' ## Saving
#' 
#' Save the matches to a new file
## ------------------------------------------------------------------------
saveRDS(afrob_langs,
        file = project_path("data", "afrob_langs.rds"))
