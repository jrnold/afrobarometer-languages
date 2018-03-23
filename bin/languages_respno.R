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

#' Create a dataset of all Afrobarometer datasets
process_round <- function(.l) {
  dat <- IO$afrobarometer(.l[["round"]])
  other_vars <- c("respno", "country", "district", "region",
                  "withinwt")
  other_varnames <- as.list(keep(.l[c(other_vars)], negate(is.na)))
  lang_vars <- c("respondent", "interview", "interviewer",
                 "respondent_other", "interview_other", "interviewer_other")
  lang_varnames <- as.list(keep(.l[lang_vars], negate(is.na)))
  out <- select(dat, UQS(c(other_varnames, lang_varnames)))
  for (i in lang_vars) {
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
  for (i in c("district", "region")) {
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
           country = as.integer(country),
           region = str_to_lower(region),
           district = str_to_lower(district)) %>%
    gather(type, value, UQS(as.list(lang_vars))) %>%
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
  left_join(filter(IO$language_names,
                   lang_number == 1L) %>%
              select(-lang_number),
            by = c("country", "lang_name" = "name")) %>%
  select(round, variable = type, respno, country, lang_name, is_other,
         iso_639_3, glottocode, wals, district, region, withinwt) %>%
  arrange(round, variable, respno)

write_csv(languages_respno, path = OUTPUT, na = "")
