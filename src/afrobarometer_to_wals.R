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

afrobarometer_to_wals_manual <- IO$afrobarometer_mappings %>%
  map_df(function(.x) {
    if (!is.null(.x[["wals_code"]])) {
      # it shouldn't be empty but if it is, continue
        if (is.null(names(.x[["wals_code"]]))) {
          # if no names, then all countries
          out <- tidyr::crossing(question = .x[["question"]],
                                 wals_code = .x[["wals_code"]])
        } else {
          wals_codes <- .x[["wals_code"]]
          out <- map2_df(names(wals_codes), wals_codes,
                         ~ tibble(wals_code = .x, valid_country = .y))
          out <- tidyr::crossing(out, question = .x[["question"]])
        }
        out[["lang_id"]] <- .x$lang_id
        out[["round"]] <- .x$round
        out
    }
  }) %>%
  mutate(auto = 0L, distance = 0L)

afrobarometer_to_iso <- IO$afrobarometer_to_iso

wals <- IO$wals %>%
  # make countrycodes into list to aid searching
  mutate(countrycodes = str_split(countrycodes, " +"))

iso_to_wals <- IO$iso_to_wals %>%
  select(iso_639_3 = iso_code, wals_code, distance, same_country)

#' For any Afrobarometer languages without manual matches, the
#' WALS code is found by
#'
#' - taking the ISO Matches from afrobarometer_to_iso
#' - match all codes of an anfrobarometer_language with the closest WALS languages
#'    using the table previously constructed
#' - for each Afrobarometer language, keep the "closest" WALS language(s)
afrobarometer_to_wals_auto <-
  afrobarometer_to_iso %>%
  # keep only inidividual langs (remove special & macrolangs)
  filter(iso_scope == "I") %>%
  select(round, question, lang_id, valid_country = iso_alpha2, iso_639_3) %>%
  # remove any that are already matched
  anti_join(afrobarometer_to_wals_manual,
            by = c("round", "question", "lang_id")) %>%
  # Find closest WALS language from ISO code
  inner_join(iso_to_wals, by = "iso_639_3") %>%
  select(-iso_639_3) %>%
  # Keep WALS codes with the closest linguistic distance
  group_by(round, question, valid_country, lang_id) %>%
  filter(distance == min(distance)) %>%
  # Prefer those in the same country
  left_join(select(wals, wals_code, wals_countries = countrycodes),
            by = "wals_code") %>%
  mutate(in_wals_country = map2_int(valid_country, wals_countries,
                                    ~ as.integer(.x %in% .y))) %>%
  select(-wals_countries) %>%
  group_by(round, question, valid_country, lang_id) %>%
  filter(in_wals_country == max(in_wals_country)) %>%
  # Prefer those in same country as the spoken lang
  group_by(round, question, valid_country, lang_id) %>%
  filter(same_country == max(same_country)) %>%
  ungroup() %>%
  distinct() %>%
  mutate(auto = 1L)

#' Combine the auto and manual matches
afrobarometer_to_wals <-
  bind_rows(afrobarometer_to_wals_manual, afrobarometer_to_wals_auto)

#' Outer join to ensure that all Afrobarometer Languages are accounted for
afrobarometer_to_wals <-
  left_join(IO$afrobarometer_langs,
            afrobarometer_to_wals,
            by = c("round", "question", value = "lang_id")) %>%
  rename(lang_name = name, lang_id = value) %>%
  # keep only valid matches
  filter(is.na(valid_country) | valid_country == iso_alpha2) %>%
  select(-valid_country)

#' Add WALS info
afrobarometer_to_wals %<>%
  left_join(select(wals, wals_code, wals_name = Name),
            by = "wals_code")

#' Prep dataset
afrobarometer_to_wals %<>%
  select(round, question, lang_id, lang_name, country, iso_alpha2,
         wals_code, wals_name, auto, distance, same_country,
         in_wals_country) %>%
  arrange(round, question, lang_id, country, wals_code)

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
            by = c("round", "question", value = "lang_id", "country"))
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

# Should be no missing WALS codes other than those from missing  ISO
missing_wals_codes <-
  afrobarometer_to_wals %>%
  filter(is.na(wals_code)) %>%
  anti_join(filter(afrobarometer_to_iso, iso_scope == "S"),
            by = c("round", "question", "lang_id", "country"))
if (nrow(missing_wals_codes)) {
  print(select(missing_wals_codes, round, question, lang_id, lang_name,
               iso_alpha2))
  stop("There are unaccounted for missing WALS codes")
}

# I've tested the ISO languages for countries with ISO matches, so
# only test manual WALS matches
afrobarometer_to_wals_country_nonmatches <-
  afrobarometer_to_wals %>%
  filter(!is.na(wals_code)) %>%
  filter(!auto) %>%
  filter(!wals_code %in% c("por", "fre", "eng", "ger")) %>%
  anti_join(IO$afrobarometer_to_wals_country_nonmatches,
            by = c("round", "question", "lang_id",
                   "iso_alpha2", "wals_code")) %>%
  anti_join(unnest(select(wals, wals_code, countrycodes)),
            by = c("wals_code", "iso_alpha2" = "countrycodes"))
if (nrow(afrobarometer_to_wals_country_nonmatches)) {
  print(select(afrobarometer_to_wals_country_nonmatches,
               round, question, lang_id, lang_name,iso_alpha2, wals_code),
        n = 1000)
  stop("There are unaccounted for non-country matches in WALS")
}

#' Check that there aren't any non-related matches appearing
#' While Afrobarometer can be one to many they should all be in the same genus at least.
#'
matches_unrelated_langs <-
  afrobarometer_to_wals %>%
  # remove WALS non-matches
  filter(!is.na(wals_code)) %>%
  left_join(select(wals, wals_code, genus), by = "wals_code") %>%
  group_by(round, question, lang_id, lang_name, iso_alpha2) %>%
  summarise(n_genus = length(unique(genus))) %>%
  filter(n_genus > 1)

if (nrow(matches_unrelated_langs) > 0) {
  print(matches_unrelated_langs)
  stop("Unrelated WALS languages detected")
}

with(afrobarometer_to_wals, {
  assert_that(all(!is.na(round)))
  assert_that(is_integerish(round))

  assert_that(is.character(question))
  assert_that(all(!is.na(question)))

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

#' Write output
write_afrobarometer_to_wals <- function(x, path) {
  write_csv(x, path = path, na = "")
}
write_afrobarometer_to_wals(afrobarometer_to_wals, OUTPUT)
