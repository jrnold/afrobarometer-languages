#' ---
#' title: "Match Afrobarometer Languages to ISO 639-3 Language Codes"
#' ---
#'
#' Write Dataset of Afrobarometer Languages
#'
#'
source(here::here("src", "R", "init.R"))

OUTPUT <- project_path("data", "languages_respno.csv")


clean_lang <- function(x) {
  str_replace_all(str_to_lower(x), "\\s*/\\s*", "/") %>%
    str_replace_all("\\s*-\\s*", "-")
}

#' For an Afrobarometer Dataset summarize the languages
summarize_lang <- function(vartype, varname, x, country, withinwt) {
  if (is.na(varname)) {
    NULL
  } else {
    select(x,
           lang = UQ(sym(varname)),
           country = UQ(sym(country)),
           withinwt = UQ(sym(withinwt))) %>%
    mutate(country = as.integer(country),
           value = if (!haven::is.labelled(lang)) {
             NA_integer_
             } else  {
               as.integer(lang)
             },
           label = if (!haven::is.labelled(lang)) {
             as.character(lang)
           } else {
             as.character(haven::as_factor(lang))
           }) %>%
    group_by(country, value, label) %>%
    summarise(n_resp = n(), prop = sum(withinwt)) %>%
    group_by(country) %>%
    mutate(prop = prop / sum(prop)) %>%
    ungroup() %>%
    mutate(variable = UQ(varname),
           type = UQ(vartype),
           lang_name = clean_lang(label))
  }
}

#' Create a dataset of all Afrobarometer datasets
process_round <- function(.l) {
  dat <- IO$afrobarometer(.l[["round"]])
  lang_vars <-
    .l[c("respondent", "interview", "interviewer",
         "respondent_other", "interview_other", "interviewer_other")]

  out <-
    # this will rename the variables as well
    select(dat, UQS(.l["respno"]), UQS(.l["country"]),
           UQS(as.list(lang_vars)[!is.na(lang_vars)]))
  for (i in names(lang_vars)) {
    if (i %in% names(out)) {
      if (haven::is.labelled(out[[i]])) {
        out[[i]] <- as.character(haven::as_factor(out[[i]]))
      } else {
        out[[i]] <- as.character(out[[i]])
      }
    } else {
      out[[i]] <- NA_character_
    }
  }

  out %>%
    mutate(respno = as.character(respno),
           country = as.integer(country)) %>%
    gather(type, value, -respno, -country) %>%
    mutate(value = str_to_lower(value),
           other = if_else(str_detect(type, "_other$"),
                           "lang_other", "lang_name"),
           type = str_replace(type, "_other$", "")) %>%
    spread(other, value) %>%
    mutate(is_other = str_detect(lang_name, "^others?$"),
           lang_name = if_else(is_other, lang_other, lang_name)) %>%
    select(-lang_other) %>%
    filter(!is.na(lang_name)) %>%
    mutate(round = .l[["round"]])

}

languages_respno <- IO$afrobarometer_variables %>%
  split(seq_len(nrow(.))) %>%
  unname() %>%
  map_df(process_round) %>%
  # join countries
  left_join(select(IO$countries, round, country = value, iso_alpha2),
            by = c("country", "round")) %>%
  select(-country) %>%
  rename(country = iso_alpha2) %>%
  # Add linguistic IDs
  mutate(lang_name = if_else(is.na(lang_name), "", lang_name)) %>%
  left_join(IO$language_names, by = c("country", "lang_name" = "name")) %>%
  select(round, variable = type, respno, country, lang_name, is_other,
         iso_639_3, glottocode, wals) %>%
  arrange(round, variable, respno)

write_csv(languages_respno, path = OUTPUT, na = "")
