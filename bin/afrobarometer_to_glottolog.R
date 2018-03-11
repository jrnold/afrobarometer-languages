source(here::here("src", "R", "init.R"))

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
                    round, variable, lang_id, iso_alpha2),
             by = c("round", "variable", "lang_id")) %>%
  filter(is.na(valid_country) | (valid_country == iso_alpha2)) %>%
  select(-valid_country)

# All matches
afrobarometer_to_glottolog <-
  IO$afrobarometer_langs %>%
  select(round, variable, lang_id, lang_name, country, iso_alpha2) %>%
  left_join(to_glottocodes,
            by = c("round", "variable", "lang_id", "iso_alpha2")) %>%
  left_join(select(IO$glottolog_languoids, glottocode, level,
                   glottolog_name = name),
            by = "glottocode") %>%
  select(round, lang_id, variable, iso_alpha2, country,
         lang_name, glottocode, glottolog_name, level) %>%
  arrange(round, variable, lang_id, country)

#'
#' # Test data
#'
assert_that(nrow(afrobarometer_to_glottolog) == nrow(IO$afrobarometer_langs))

#' check primary key
assert_that(nrow(distinct(afrobarometer_to_glottolog,
                          round, variable, lang_id, country))
            == nrow(afrobarometer_to_glottolog))

rlang::eval_tidy(quo({
  assert_that(all(!is.na(.data$round)))
  assert_that(is.integer(.data$round))
  assert_that(all(.data$round %in% 1:6))

  assert_that(all(!is.na(.data$lang_id)))
  assert_that(is.integer(.data$lang_id))
  assert_that(all(.data$lang_id >= -1 & .data$lang_id <= 9999))

  assert_that(all(!is.na(.data$iso_alpha2)))
  assert_that(is.character(.data$iso_alpha2))
  assert_that(all(str_detect(.data$iso_alpha2, "^[A-Z]{2}$")))

  assert_that(all(!is.na(.data$country)))
  assert_that(is.integer(.data$country))
  assert_that(all(.data$country >= 1))

  assert_that(all(!is.na(.data$lang_name)))
  assert_that(is.character(.data$lang_name))

  assert_that(is.character(glottocode))
  assert_that(all(str_detect(na.omit(glottocode), "^[a-z0-9]{4}[0-9]{4}$")))

  assert_that(is.character(glottolog_name))

  assert_that(is.character(.data$level))
  assert_that(all(na.omit(.data$level) %in% c("language", "dialect", "family")))

}), data = afrobarometer_to_glottolog)


#' all glottolog langs should be valid
invalid_glottocode <-
  afrobarometer_to_glottolog %>%
  filter(!is.na(glottocode)) %>%
  anti_join(glottolog_languoids, by = c("glottocode"))
if (nrow(invalid_glottocode)) {
  print(invalid_glottocode, n = 1000)
  stop("Unaccounted for non-matches found")
}

#' All Afrobarometer codes should be acounted for
to_glottolog_langmiss <-
  afrobarometer_to_glottolog %>%
  anti_join(IO$afrobarometer_langs,
            by = c("round", "variable", "lang_id", "country"))
if (nrow(to_glottolog_langmiss)) {
  print(to_glottolog_langmiss)
  stop("Invalid Afrobarometer languages found")
}


#' No non-missing values
to_glottolog_missing_vals <-
  afrobarometer_to_glottolog %>%
  filter(is.na(glottocode)) %>%
  anti_join(IO$afrobarometer_to_glottolog_nonmatches,
            by = c("round", "variable", "iso_alpha2", "lang_name"))

if (nrow(to_glottolog_missing_vals)) {
  print(to_glottolog_missing_vals)
  stop("Missing glottocode values found")
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
  print(inconsistent_mappings, n = 100)
  stop("There are inconsistent mappings across rounds in afrobaromter_to_glottolog")
}

#' Write data
write_csv(afrobarometer_to_glottolog, OUTPUT, na = "")
