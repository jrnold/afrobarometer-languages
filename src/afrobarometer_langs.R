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
  list("r1" = list("external", "afrobarometer", "merged_r1_data.sav"),
    "r2" = list("external", "afrobarometer", "merged_r2_data.sav"),
    "r3" = list("external", "afrobarometer", "merged_r3_data.sav"),
    "r4" = list("external", "afrobarometer", "merged_r4_data.sav"),
    "r5" = list("external", "afrobarometer", "merged-round-5-data-34-countries-2011-2013-last-update-july-2015.sav"),
    "r6" = list("external", "afrobarometer", "merged_r6_data_2016_36countries2.sav"),
    "countries" = list("data-raw", "afrobarometer_countries.csv")) %>%
  map_chr(function(x) invoke(find_rstudio_root_file, x))

afrobarometer_langs <- function(filename, rnd) {
  x <- haven::read_sav(filename)
  if (rnd %in% "r1") {
    langs <- enframe(attr(x$language, "labels")) %>%
      mutate(question = "language")
    countries <-
      transmute(x,
                countries = as.integer(country),
                language = as.integer(language),
                question = "language") %>%
      distinct()
  } else if (rnd %in% "r2") {
    langs <- bind_rows(
      enframe(attr(x$q83, "labels")) %>%
        mutate(question = "q83"),
      enframe(attr(x$q97, "labels")) %>%
        mutate(question = "q97")
    )
    countries <- bind_rows(
      transmute(x,
                countries = as.integer(country),
                language = as.integer(q83),
                question = "q83") %>%
        distinct(),
      transmute(x,
                countries = as.integer(country),
                language = as.integer(q97),
                question = "q97") %>%
        distinct()
    )
  } else if (rnd %in% "r3") {
    langs <- bind_rows(
      enframe(attr(x$q3, "labels")) %>%
        mutate(question = "q3"),
      enframe(attr(x$q103, "labels")) %>%
        mutate(question = "q103")
    )
    countries <- bind_rows(
      transmute(x,
                countries = as.integer(country),
                language = as.integer(q3),
                question = "q3") %>%
        distinct(),
      transmute(x,
                countries = as.integer(country),
                language = as.integer(q103),
                question = "q103") %>%
        distinct()
    )
  } else if (rnd %in% "r4") {
    langs <- bind_rows(
      enframe(attr(x$Q3, "labels")) %>%
        mutate(question = "Q3"),
      enframe(attr(x$Q103, "labels")) %>%
        mutate(question = "Q103")
    )
    countries <- bind_rows(
      transmute(x,
                countries = as.integer(COUNTRY),
                language = as.integer(Q3),
                question = "Q3") %>%
        distinct(),
      transmute(x,
                countries = as.integer(COUNTRY),
                language = as.integer(Q103),
                question = "Q103") %>%
        distinct()
    )
  } else if (rnd %in% c("r5", "r6")) {
    langs <- bind_rows(
      enframe(attr(x$Q2, "labels")) %>%
        mutate(question = "Q2"),
      enframe(attr(x$Q103, "labels")) %>%
        mutate(question = "Q103")
    )
    countries <- bind_rows(
      transmute(x,
                countries = as.integer(COUNTRY),
                language = as.integer(Q2),
                question = "Q2") %>%
        distinct(),
      transmute(x,
                countries = as.integer(COUNTRY),
                language = as.integer(Q103),
                question = "Q103") %>%
        distinct()
    )
  }
  left_join(langs, countries,
            by = c("question", value = "language")) %>%
    mutate(round = rnd)
}

write_afrobarometer_langs <- function(x, dst) {
  x %>%
  mutate(countries = map_chr(countries, function(x) str_c(x, collapse = " "))) %>%
  select(round, question, lang_id, lang_name, countries) %>%
  arrange(round, question, lang_id) %>%
  write_csv(path = dst)
}

afrobarometer_countries <-
  read_csv(INPUT[["countries"]],
           col_types = cols_only(
             round = col_character(),
             value = col_integer(),
             iso_alpha2 = col_character()
           ),
           na = "")

INPUT[str_subset(names(INPUT), "^r")] %>%
  {map2_df(., names(.), afrobarometer_langs)} %>%
  left_join(afrobarometer_countries,
            by = c("round", "countries" = "value")) %>%
  mutate(value = as.integer(value)) %>%
  group_by(round, question, value, name) %>%
  summarise(countries = list(sort(unique(iso_alpha2)))) %>%
  arrange(round, question, value) %>%
  mutate(countries = map_chr(countries, paste, collapse = " ")) %>%
  write_csv(path = OUTPUT, na = "")
