#' ---
#' title: "Match Afrobarometer Language Values to ISO 639-3 Language Codes"
#' ---
#'
#' Write Dataset of Afrobarometer Languages
#'
#'
source(here::here("src", "R", "init.R"))

OUTPUT <- project_path("data", "language_values.csv")

clean_lang <- function(x) {
  x %>%
  str_replace_all( "\\s*/\\s*", "/") %>%
    str_replace_all("\\s*-\\s*", "-") %>%
    # only lowercase everything
    str_to_lower()
    # a more general transformation would convert everything to ASCII
    # stringi::stri_trans_general("NFKD; Lower; Any-Latin; Latin-ASCII")
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
  lang_vars <- str_subset(names(.l), "^(interview(er)?|respondent)(_other)?$")

  map2_dfr(lang_vars,
           flatten_chr(.l[lang_vars]),
           summarize_lang,
           x = dat,
           country = .l[["country"]],
           withinwt = .l[["withinwt"]]) %>%
    mutate(round = .l[["round"]],
           label = if_else(is.na(label), "", label),
           lang_name = if_else(is.na(lang_name), "", lang_name))
}

language_values <- IO$afrobarometer_variables %>%
  split(seq_len(nrow(.))) %>%
  unname() %>%
  map_df(process_round) %>%
  # join countries
  left_join(select(IO$countries, round, country = value, iso_alpha2),
            by = c("country", "round")) %>%
  select(-country) %>%
  rename(country = iso_alpha2) %>%
  # Add linguistic IDs
  left_join(IO$language_names, by = c("country", "lang_name" = "name")) %>%
  select(round, variable, type, value, label, country, lang_name, iso_639_3,
         glottocode, wals, n_resp, prop) %>%
  arrange(round, variable, country, value) %>%
  distinct()

write_csv(language_values, OUTPUT, na = "")
