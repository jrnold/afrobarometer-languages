source("src/R/init.R")

OUTPUT <- project_path("data", "afrobarometer_to_glottolog.csv")

glottolog_languoids <- IO$glottolog_languoids
glottolog_macroareas <- IO$glottolog_macroareas

# Manual Glottocode Matches from the mappings
to_glottocodes <-
  IO$afrobarometer_mappings %>%
  map_df(function(.x) {
    if (!is.null(.x[["glottocodes"]])) {
      # it shouldn't be empty but if it is, continue
      if (is.null(names(.x[["glottocodes"]]))) {
        # if no names, then all countries
        out <- tidyr::crossing(variable = .x[["variables"]],
                               glottocode = as.character(.x[["glottocodes"]]))
        out$valid_country <- NA_character_
      } else {
        glottocodes <- .x[["glottocodes"]]
        out <- map2_df(names(glottocodes),
                       glottocodes,
                       ~ tibble(glottocode = .y,
                                valid_country = as.character(.x)))
        out <- tidyr::crossing(out, variable = .x[["variables"]])
      }
    } else {
      out <- tidyr::crossing(variable = .x[["variables"]])
      out[["valid_country"]] <- NA_character_
      out[["glottocode"]] <- NA_character_
    }
    out[["lang_id"]] <- .x$lang_id
    out[["round"]] <- .x$round
    out
  }) %>%
  left_join(select(IO$afrobarometer_langs,
                    round, variable, lang_id = value, iso_alpha2),
             by = c("round", "variable", "lang_id")) %>%
  filter(is.na(valid_country) | (valid_country == iso_alpha2)) %>%
  select(-valid_country)

# All matches
afrobarometer_to_glottolog <-
  left_join(
    select(IO$afrobarometer_langs, round,
           lang_id = value, variable, iso_alpha2, country, lang_name = name),
    to_glottocodes,
    by = c("round", "variable", "lang_id", "iso_alpha2")) %>%
  select(round, variable, lang_id, lang_name, country, iso_alpha2, glottocode) %>%
  arrange(round, variable, lang_id, country)

#'
#' # Test data
#'
assert_that(nrow(afrobarometer_to_glottolog) ==
              nrow(IO$afrobarometer_langs))

#' check primary key
assert_that(nrow(distinct(afrobarometer_to_glottolog,
                          round, variable, lang_id, country))
            == nrow(afrobarometer_to_glottolog))

#' all glottolog langs should be valid
invalid_glottocode <-
  afrobarometer_to_glottolog %>%
  filter(!is.na(glottocode)) %>%
  anti_join(glottolog_languoids, by = c("glottocode"))
if (nrow(invalid_glottocode)) {
  print(invalid_glottocode)
  stop("Unaccounted for non-matches found")
}

#' All Afrobarometer codes should be acounted for
to_glottolog_langmiss <-
  afrobarometer_to_glottolog %>%
  anti_join(IO$afrobarometer_langs,
            by = c("round", "variable", "lang_id" = "value", "country"))
if (nrow(to_glottolog_langmiss)) {
  print(to_glottolog_langmiss)
  stop("Invalid Afrobarometer languages found")
}

#' If ISO code is non-missing, then the Glottocode should also be non-missing
to_glottolog_nonmatches <-
  IO$afrobarometer_langs %>%
  anti_join(filter(IO$afrobarometer_to_iso, iso_scope == "S"),
            by = c("round", "variable", "value" = "lang_id", "country")) %>%
  anti_join(afrobarometer_to_glottolog,
            by = c("round", "variable", "value" = "lang_id", "country"))
if (nrow(to_glottolog_nonmatches)) {
  print(to_glottolog_nonmatches)
  stop("Unaccounted for non-matches found")
}

#' Check that all combinations of (country, language name)
#' match the same Glottocodes across rounds
inconsistent_mappings <-
  afrobarometer_to_glottolog %>%
  #' Arabic is handled differently
  filter(!is.na(glottocode),
         lang_name != "Arabic") %>%
  group_by(round, variable, lang_name, iso_alpha2) %>%
  summarise(glottocode = str_c(sort(unique(glottocode)), collapse = " ")) %>%
  group_by(iso_alpha2, lang_name) %>%
  filter(length(unique(glottocode)) > 1) %>%
  arrange(lang_name, iso_alpha2)
if (nrow(inconsistent_mappings)) {
  print(inconsistent_mappings)
  stop("There are inconsistent mappings across rounds in afrobaromter_to_glottolog")
}

#' All languages should be in the African Macroarea
glottolog_non_african <-
  afrobarometer_to_glottolog %>%
    filter(!is.na(glottocode)) %>%
    anti_join(filter(IO$glottolog_macroareas, macroarea == "Africa"),
              by = "glottocode") %>%
    filter(!glottocode %in% IO$misc_data$glottolog$non_african)
if (nrow(glottolog_non_african)) {
  print(select(macroarea, glottolog_non_african, glottocode,  lang_name,
               round, variable, lang_id))
  stop("Non-African Glottolog languages found")
}

col_types <- cols(
  iso_alpha2 = col_character(),
  east = col_double(),
  west = col_double(),
  north = col_double(),
  south = col_double(),
  languages = col_character(),
  longitude = col_double(),
  latitude = col_double()
)
country_geo <- read_csv("data-raw/country_geo_info.csv", col_types = col_types,
                        na = "")
col_types <- cols(
  iso_alpha2 = col_character(),
  glottocode = col_character()
)
known_country_non_matches <- read_csv("data-raw/afrobarometer_to_glottocode_country_non_matches.csv", col_types = col_types, na = "")

#' Check that language location is either inside the bounding box of the country
#' or accounted for
COMMON_LANGS <- c("stan1293", "port1283", "stan1290")
country_non_matches <- left_join(afrobarometer_to_glottolog,
                      select(country_geo, iso_alpha2, east, west, north, south),
                      by = "iso_alpha2") %>%
  left_join(select(IO$glottolog, glottocode, longitude, latitude),
            by = "glottocode") %>%
  filter(longitude < west | longitude > east |
           latitude > north | latitude < south) %>%
  filter(!glottocode %in% COMMON_LANGS) %>%
  anti_join(known_country_non_matches, by = c("iso_alpha2", "glottocode")) %>%
  select(lang_name, iso_alpha2, glottocode) %>%
  distinct()
if (nrow(country_non_matches)) {
  print(country_non_matches)
  stop("Language matches outside the country in afrobarometer_to_glottocode")
}

#' Write data
write_csv(afrobarometer_to_glottolog, OUTPUT, na = "")
