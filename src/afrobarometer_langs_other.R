#' ---
#' title:  Afrobarometer Languages
#' ---
#'
#' Write Dataset of Afrobarometer Languages.
#'
OUTPUT <- find_rstudio_root_file("data", "afrobarometer_langs_other.csv")

INPUT <- list(
       "r4" = list("external", "afrobarometer", "merged_r4_data.sav"),
       "r5" = list("external", "afrobarometer", "merged-round-5-data-34-countries-2011-2013-last-update-july-2015.sav"),
       "r6" = list("external", "afrobarometer", "merged_r6_data_2016_36countries2.sav"),
       "countries" = list("data-raw", "afrobarometer_countries.csv")) %>%
  map_chr(function(x) invoke(find_rstudio_root_file, x))

get_afrobarometer_langs_other <- function(filename, rnd) {
  lang_vars <- switch(rnd,
                      r4 = c("Q3OTHER"),
                      r5 = ,
                      r6 = c("Q2OTHER", "Q103OTHER", "Q116OTHER")
  )
  haven::read_sav(filename) %>%
    select(one_of(lang_vars), COUNTRY) %>%
    mutate(COUNTRY = as.integer(COUNTRY)) %>%
    mutate_at(vars(one_of(lang_vars)), funs(as.character)) %>%
    gather(question, value, -COUNTRY) %>%
    mutate(value = str_to_lower(value)) %>%
    filter(value != "") %>%
    distinct() %>%
    arrange(question, COUNTRY, value) %>%
    mutate(round = rnd) %>%
    rename(country = COUNTRY)
}

afrobarometer_langs_other <-
  INPUT[str_subset(names(INPUT), "^r")] %>%
  {map2_df(., names(.), get_afrobarometer_langs_other)} %>%
  left_join(select(afrobarometer_countries,
                   round, value, country_iso = iso_alpha2),
            by = c("round", "country" = "value"))

afrobarometer_langs_other %>%
  write_csv(OUTPUT, na = "")
