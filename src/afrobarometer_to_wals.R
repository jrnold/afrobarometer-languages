#' ---
#' title: "Mapping from Afrobarometer "Other" Languages to WALS"
#' ----
#'
#' This uses the Afrobarometer to ISO 639-3 matches and the ISO 639-3 to
#' WALS mappings to generate a Afrobarometer language to WALS mapping.
#'
#'
source("src/init.R")

OUTPUT <- project_path("data", "afrobarometer_to_wals.csv")

misc_data <- IO$misc_data

wals <- IO$wals

to_wals_manual <- IO$afrobarometer_mappings %>%
  map_df(function(.x) {
    if (!is.null(.x[["wals_codes"]])) {
      wals_codes <- .x[["wals_codes"]]
        # it shouldn't be empty but if it is, continue
        if (is.null(names(wals_codes))) {
          # if no names, then all countries
          out <- tidyr::crossing(variable = .x[["variables"]],
                                 wals_code = wals_codes)
          out$valid_country <- NA_character_
        } else {
          out <- map2_df(names(wals_codes), wals_codes,
                         ~ tibble(wals_code = .x, valid_country = .y))
          out <- tidyr::crossing(out, variable = .x[["variables"]])
        }
        out[["lang_id"]] <- .x$lang_id
        out[["round"]] <- .x$round
        out
      }
  }) %>%
  inner_join(IO$afrobarometer_langs,
             afrobarometer_to_wals,
             by = c("round", "variable", lang_id = "value")) %>%
  # keep only valid matches
  filter(is.na(valid_country) | valid_country == iso_alpha2) %>%
  select(round, variable, lang_id, country, wals_code) %>%
  mutate(auto = 0L)

# Filters to filter automatic glottocode mappings
to_wals_filters <- IO$afrobarometer_mappings %>%
  map_df(function(.x) {
    if (!is.null(.x[["wals_code"]])) {
      wals_codes <- .x[["wals_code"]]
      if (!(is.null(wals_codes[["filter"]]))) {
        tibble(
          variable = .x[["variables"]],
          filter_exprs = map(wals_codes[["filter"]], rlang::parse_expr),
          lang_id = .x$lang_id,
          round = .x$round
        )
      }
    }
  })

# Use the Afrobarometer to Glottolog and Glottolog to WALS
# mappings to fill in remaining Afrobarometer to WALS mappings
to_wals_auto <-
  IO$glottolog %>%
  select(glottocode, wals_codes) %>%
  inner_join(select(IO$afrobarometer_to_glottolog,
                    round, variable, lang_id, country, iso_alpha2,
                    glottocode),
             by = "glottocode") %>%
  anti_join(to_wals_manual, by = c("round", "variable", "lang_id", "country")) %>%
  mutate(wals_codes = str_split(wals_codes, " "), auto = 1L) %>%
  unnest(wals_codes) %>%
  rename(wals_code = wals_codes) %>%
  # Filter by countries
  left_join(select(IO$wals, wals_code, countrycodes, genus),
            by = "wals_code") %>%
  # I should always handle Creoles separately
  filter(genus != "Creoles and Pidgins") %>%
  # Prefer matches within country
  # mutate(in_country = str_detect(countrycodes, iso_alpha2)) %>%
  # group_by(round, variable, lang_id, country) %>%
  # filter(in_country == max(in_country)) %>%
  ungroup() %>%
  select(round, variable, lang_id, country, wals_code) %>%
  mutate(auto = 1)

#' Combine the auto and manual matches
afrobarometer_to_wals <- bind_rows(to_wals_manual, to_wals_auto)

#' Outer join to ensure that all Afrobarometer Languages are accounted for
afrobarometer_to_wals <-
  left_join(IO$afrobarometer_langs,
            afrobarometer_to_wals,
            by = c("round", "variable", "country",
                   value = "lang_id")) %>%
  rename(lang_name = name, lang_id = value)

#' Add WALS info
afrobarometer_to_wals %<>%
  left_join(select(wals, wals_code, wals_name = Name, family),
            by = "wals_code")

#' Prep dataset
afrobarometer_to_wals %<>%
  select(round, variable, lang_id, lang_name, country, iso_alpha2,
         wals_code, wals_name, auto) %>%
  arrange(round, variable, lang_id, country, wals_code)

with(afrobarometer_to_wals, {
  assert_that(all(!is.na(round)))
  assert_that(is_integerish(round))

  assert_that(is.character(variable))
  assert_that(all(!is.na(variable)))

  # Lang Id
  assert_that(is.integer(lang_id))
  assert_that(all(lang_id >= -1 & lang_id <= 9999))

  # Lang Name
  assert_that(all(!is.na(lang_name)))
  assert_that(is.character(lang_name))

  # Wals code
  assert_that(is.character(wals_code))
  assert_that(all(str_detect(na.omit(wals_code), misc_data$wals$code_pattern)))

  # Wals name
  assert_that(is.character(wals_name))
})

#' Check that all WALS codes are valid
#'
#' wals_codes can be missing, but if non-missing must appear in WALS dataset
#'
wals_nonmatches <- afrobarometer_to_wals %>%
  filter(!is.na(wals_code)) %>%
  anti_join(wals, by = c("wals_code"))

if (nrow(wals_nonmatches) > 0) {
  print(wals_nonmatches)
  stop("There exist invalid WALS codes")
}

# all afrobarometer langs should appear
afrobarometer_lang_nonmatches <-
  anti_join(IO$afrobarometer_langs,
            afrobarometer_to_wals,
            by = c("round", "variable", value = "lang_id", "country"))
stopifnot(nrow(afrobarometer_lang_nonmatches) == 0)

#' All WALS languages should be from the Africa Macrolanguage unless accounted
#' for.
wals_non_african <-
  inner_join(afrobarometer_to_wals,
             select(wals, wals_code, macroarea),
             by = "wals_code") %>%
  filter(!(wals_code %in% misc_data$wals$non_african$values)) %>%
  filter(!(macroarea %in% "Africa"))
if (nrow(wals_non_african) > 0) {
  print(wals_non_african, n = 100, width = 10000)
  stop("There exist unaccounted for non-African languages in the data:\n")
}



#' Check that there aren't any non-related matches appearing
#' While Afrobarometer can be one to many they should all be in the same genus at least.
matches_unrelated_langs <-
  afrobarometer_to_wals %>%
  # remove WALS non-matches
  filter(!is.na(wals_code)) %>%
  left_join(select(wals, wals_code, genus), by = "wals_code") %>%
  group_by(round, variable, lang_id, lang_name, iso_alpha2) %>%
  summarise(n_genus = length(unique(genus))) %>%
  filter(n_genus > 1)

if (nrow(matches_unrelated_langs) > 0) {
  stop("Unrelated WALS languages detected")
}

consistent_mappings <-
  afrobarometer_to_wals %>%
  group_by(round, variable, lang_name, iso_alpha2) %>%
  summarise(wals_code = str_c(wals_code, collapse = " ")) %>%
  group_by(iso_alpha2, lang_name, wals_code) %>%
  filter(length(unique(wals_code)) > 1) %>%
  arrange(lang_name, iso_alpha2)
if (nrow(consistent_mappings)) {
  print(consistent_mappings)
  stop("There are inconsistent mappings in the WALS mappings")
}


#' Write output
write_afrobarometer_to_wals <- function(x, path) {
  write_csv(x, path = path, na = "")
}
write_afrobarometer_to_wals(afrobarometer_to_wals, OUTPUT)
