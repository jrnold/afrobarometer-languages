source("src/R/init.R")

OUTPUT <- project_path("data", "afrobarometer_other_to_glottolog.csv")

glottolog_languoids <- IO$glottolog_languoids

glottolog_resourcemap <- IO$glottolog_resourcemap

glottolog_lang_geo <- IO$glottolog_lang_geo

afrobarometer_langs_other <-
  IO$afrobarometer_langs_other %>%
  mutate(lang_name = str_to_lower(value))

# Manual Glottocode Matches from the mappings
to_glottocodes <-
  IO$afrobarometer_other_mappings %>%
  map_df(function(.x) {
    if (!is.null(.x[["glottocode"]])) {
      # it shouldn't be empty but if it is, continue
      tibble(glottocode = as.character(.x[["glottocode"]]),
             lang_name = as.character(.x[["lang_name"]]),
             iso_alpha2 = .x[["country"]])
    }
  }) %>%
  left_join(afrobarometer_langs_other, by = c("lang_name", "iso_alpha2")) %>%
  select(round, variable, value, iso_alpha2, glottocode)

# All matches
afrobarometer_to_glottolog <-
  IO$afrobarometer_langs_other %>%
  left_join(to_glottocodes,
            by = c("round", "variable", "value", "iso_alpha2")) %>%
  select(round, variable, value, iso_alpha2, country, glottocode) %>%
  arrange(round, variable, value, country)

#'
#' # Test data
#'
assert_that(nrow(afrobarometer_to_glottolog) ==
              nrow(IO$afrobarometer_langs_other))

#' check primary key
assert_that(nrow(distinct(afrobarometer_to_glottolog,
                          round, variable, value, country))
            == nrow(afrobarometer_to_glottolog))

#' all glottolog langs should be valid
invalid_glottocode <-
  afrobarometer_to_glottolog %>%
  filter(!is.na(glottocode)) %>%
  anti_join(glottolog_languoids, by = c("glottocode" = "id"))
if (nrow(invalid_glottocode)) {
  print(glottolog_glottocode)
  stop("Unaccounted for non-matches found")
}

#' All Afrobarometer languages should be valid
to_glottolog_langmiss <-
  afrobarometer_to_glottolog %>%
  anti_join(IO$afrobarometer_langs_other,
            by = c("round", "variable", "value", "country"))
if (nrow(to_glottolog_langmiss)) {
  print(to_glottolog_langmiss)
  stop("Invalid afrobaromter languages found")
}

#' All Afrobarometer codes should be acounted for
to_glottolog_nonmatches <-
  IO$afrobarometer_langs_other %>%
  anti_join(filter(IO$afrobarometer_other_to_iso, iso_scope == "S"),
            by = c("round", "variable", "value", "country")) %>%
  anti_join(afrobarometer_to_glottolog,
            by = c("round", "variable", "value", "country"))
if (nrow(to_glottolog_nonmatches)) {
  print(to_glottolog_nonmatches)
  stop("Unaccounted for non-matches found")
}

#' All Glottocode languages should be accounted for
glottolog_non_african <-
  afrobarometer_to_glottolog %>%
    filter(!is.na(glottocode)) %>%
    left_join(select(IO$glottolog_lang_geo, glottocode, macroarea),
         by = "glottocode") %>%
    filter(macroarea != "Africa") %>%
    filter(!glottocode %in% IO$misc_data$glottolog$non_african)
if (nrow(glottolog_non_african)) {
  print(select(glottolog_non_african, glottocode, value, macroarea))
  stop("Non-African Glottolog languages found")
}

#' Write data
write_csv(afrobarometer_to_glottolog, OUTPUT, na = "")
