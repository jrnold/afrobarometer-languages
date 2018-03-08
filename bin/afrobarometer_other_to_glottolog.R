source("src/R/init.R")

OUTPUT <- project_path("data", "afrobarometer_other_to_glottolog.csv")

glottolog_languoids <- IO$glottolog_languoids

glottolog_resourcemap <- IO$glottolog_resourcemap

glottolog_lang_geo <- IO$glottolog_lang_geo

afrobarometer_langs_other <-
  IO$afrobarometer_langs_other

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
  left_join(afrobarometer_langs_other,
            by = c("lang_name", "iso_alpha2")) %>%
  select(round, variable, lang_name, iso_alpha2, glottocode)

# All matches
afrobarometer_to_glottolog <-
  IO$afrobarometer_langs_other %>%
  select(round, variable, country, lang_name, iso_alpha2) %>%
  left_join(to_glottocodes,
            by = c("round", "variable", "lang_name", "iso_alpha2")) %>%
  left_join(select(IO$glottolog_languoids, glottocode, level,
                   glottolog_name = name),
            by = "glottocode") %>%
  select(round, lang_name, variable, iso_alpha2, country,
         glottocode, glottolog_name, level) %>%
  arrange(round, variable, lang_name, country)

#'
#' # Test data
#'
assert_that(nrow(afrobarometer_to_glottolog) ==
              nrow(IO$afrobarometer_langs_other))

#' check primary key
assert_that(nrow(distinct(afrobarometer_to_glottolog,
                          round, variable, lang_name, country))
            == nrow(afrobarometer_to_glottolog))

#' all glottolog langs should be valid
invalid_glottocodes <-
  afrobarometer_to_glottolog %>%
  filter(!is.na(glottocode)) %>%
  anti_join(glottolog_languoids, by = c("glottocode" = "glottocode"))

if (nrow(invalid_glottocodes)) {
  print(invalid_glottocodes)
  stop("Unaccounted for non-matches found")
}

#' All Afrobarometer languages should be valid
to_glottolog_langmiss <-
  afrobarometer_to_glottolog %>%
  anti_join(IO$afrobarometer_langs_other,
            by = c("round", "variable", "lang_name", "country"))
if (nrow(to_glottolog_langmiss)) {
  print(to_glottolog_langmiss)
  stop("Invalid afrobaromter languages found")
}

#' All Glottocode languages should be accounted for
glottolog_non_african <-
  afrobarometer_to_glottolog %>%
  filter(!is.na(glottocode)) %>%
  anti_join(filter(IO$glottolog_macroareas, macroarea == "Africa"),
            by = "glottocode") %>%
  filter(!glottocode %in% IO$misc_data$glottolog$non_african)
if (nrow(glottolog_non_african)) {
  print(select(glottolog_non_african, glottocode, lang_name, macroarea))
  stop("Non-African Glottolog languages found")
}

#' Write data
write_csv(afrobarometer_to_glottolog, OUTPUT, na = "")
