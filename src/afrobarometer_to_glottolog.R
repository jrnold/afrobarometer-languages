source("src/init.R")

OUTPUT <- project_path("data", "afrobarometer_to_glottolog.csv")

glottolog_languoids <- IO$glottolog_languoids

glottolog_lang_geo <- IO$glottolog_lang_geo

# Manual Glottocode Matches from the mappings
to_glottocodes <-
  IO$afrobarometer_mappings %>%
  map_df(function(.x) {
    if (!is.null(.x[["glottocodes"]])) {
      # it shouldn't be empty but if it is, continue
      if (is.null(names(.x[["glottocodes"]]))) {
        # if no names, then all countries
        out <- tidyr::crossing(question = .x[["variables"]],
                               glottocode = as.character(.x[["glottocodes"]]))
        out$valid_country <- NA_character_
      } else {
        glottocodes <- .x[["glottocodes"]]
        out <- map2_df(names(glottocodes),
                       glottocodes,
                       ~ tibble(glottocode = .y,
                                valid_country = as.character(.x)))
        out <- tidyr::crossing(out, question = .x[["variables"]])
      }
    } else {
      print(.x)
      out <- tidyr::crossing(question = .x[["variables"]])
      out[["valid_country"]] <- NA_character_
      out[["glottocode"]] <- NA_character_
    }
    out[["lang_id"]] <- .x$lang_id
    out[["round"]] <- .x$round
    out
  }) %>%
  left_join(select(IO$afrobarometer_langs,
                    round, question, lang_id = value, iso_alpha2),
             by = c("round", "question", "lang_id")) %>%
  filter(is.na(valid_country) | (valid_country == iso_alpha2)) %>%
  select(-valid_country)

# All matches
afrobarometer_to_glottolog <-
  left_join(
    select(IO$afrobarometer_langs, round,
           lang_id = value, question, iso_alpha2, country, lang_name = name),
    to_glottocodes,
    by = c("round", "question", "lang_id", "iso_alpha2")) %>%
  select(round, question, lang_id, lang_name, country, iso_alpha2, glottocode) %>%
  arrange(round, question, lang_id, country)

#'
#' # Test data
#'
assert_that(nrow(afrobarometer_to_glottolog) ==
              nrow(IO$afrobarometer_langs))

#' check primary key
assert_that(nrow(distinct(afrobarometer_to_glottolog,
                          round, question, lang_id, country))
            == nrow(afrobarometer_to_glottolog))

#' all glottolog langs should be valid
invalid_glottocode <-
  afrobarometer_to_glottolog %>%
  filter(!is.na(glottocode)) %>%
  anti_join(glottolog_languoids, by = c("glottocode" = "id"))
if (nrow(invalid_glottocode)) {
  print(invalid_glottocode)
  stop("Unaccounted for non-matches found")
}

#' All Afrobarometer codes should be acounted for
to_glottolog_langmiss <-
  afrobarometer_to_glottolog %>%
  anti_join(IO$afrobarometer_langs,
            by = c("round", "question", "lang_id" = "value", "country"))
if (nrow(to_glottolog_langmiss)) {
  print(to_glottolog_langmiss)
  stop("Invalid Afrobarometer languages found")
}

#' All Afrobarometer codes should be acounted for
to_glottolog_nonmatches <-
  IO$afrobarometer_langs %>%
  anti_join(filter(IO$afrobarometer_to_iso, iso_scope == "S"),
            by = c("round", "question", "value" = "lang_id", "country")) %>%
  anti_join(afrobarometer_to_glottolog,
            by = c("round", "question", "value" = "lang_id", "country"))
if (nrow(to_glottolog_nonmatches)) {
  print(to_glottolog_nonmatches)
  stop("Unaccounted for non-matches found")
}

#' All Glottocode languages should be accounted
glottolog_non_african <-
  afrobarometer_to_glottolog %>%
    filter(!is.na(glottocode)) %>%
    left_join(select(IO$glottolog_lang_geo, glottocode, macroarea),
         by = "glottocode") %>%
    filter(macroarea != "Africa") %>%
    filter(!glottocode %in% IO$misc_data$glottolog$non_african)
if (nrow(glottolog_non_african)) {
  print(select(macroarea, glottolog_non_african, glottocode,  lang_name,
               round, question, lang_id))
  stop("Non-African Glottolog languages found")
}

#' Write data
write_csv(afrobarometer_to_glottolog, OUTPUT, na = "")
