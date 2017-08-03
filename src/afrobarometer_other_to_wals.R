#' ---
#' title: "Mapping from Afrobarometer "Other" Languages to WALS"
#' ----
#'
#' This uses the Afrobarometer to ISO 639-3 matches and the ISO 639-3 to
#' WALS mappings to generate a Afrobarometer language to WALS mapping.
#'
#'
source("src/init.R")

OUTPUT <- project_path("data", "afrobarometer_other_to_wals.csv")

misc_data <- IO$misc_data

iso_to_wals <- IO$iso_to_wals %>%
  select(iso_639_3 = iso_code, wals_code, distance, same_country)

wals <- IO$wals %>%
  mutate(countrycodes = if_else(countrycodes == "", list(NULL),
                                str_split(countrycodes, " +")))

# Read Afrobarometer Other Languages
afrobarometer_langs_other <-
  IO$afrobarometer_langs_other %>%
  # add variable to merge on
  mutate(lang_name = str_to_lower(value))

#' Afrobarometer Mappings
afrobarometer_other_to_wals_manual <-
  IO$afrobarometer_other_mappings %>%
  map_df(function(.x) {
    if (!is.null(.x[["wals_code"]])) {
      out <- tidyr::crossing(country = .x[["country"]],

                             wals_code = .x[["wals_code"]])
      out[["lang_name"]] <- .x$lang_name
      out
    }
  }) %>%
  select(country, lang_name, wals_code) %>%
  distinct() %>%
  mutate(auto = 0L, distance = 0L)

afrobarometer_other_to_wals_auto <-
  IO$afrobarometer_other_to_iso %>%
  filter(iso_scope %in% "I") %>%
  mutate(lang_name = str_to_lower(value)) %>%
  select(country = iso_alpha2, lang_name, iso_639_3) %>%
  # there can multiples of the above
  distinct() %>%
  # remove those already accounted for
  anti_join(afrobarometer_other_to_wals_manual,
            by = c("country", "lang_name")) %>%
  inner_join(iso_to_wals, by = "iso_639_3") %>%
  select(-iso_639_3) %>%
  distinct() %>%
  # minimize linguistic dist
  group_by(country, lang_name) %>%
  filter(distance == min(distance)) %>%
  # prefer within country
  left_join(select(wals, wals_code, wals_countries = countrycodes),
            by = "wals_code") %>%
  mutate(in_wals_country = map2_int(country, wals_countries,
                                    ~ if_else(is.null(.y), 0L,
                                              as.integer(.x %in% .y)))) %>%
  select(-wals_countries) %>%
  group_by(country, lang_name) %>%
  filter(in_wals_country == max(in_wals_country)) %>%
  # prefer same country as orig
  group_by(country, lang_name) %>%
  filter(same_country == max(same_country)) %>%
  ungroup() %>%
  select(country, lang_name, wals_code, distance, same_country,
         in_wals_country) %>%
  distinct() %>%
  mutate(auto = 1L)

#' Combine the auto and manual matches
afrobarometer_other_to_wals <-
  bind_rows(afrobarometer_other_to_wals_manual,
            afrobarometer_other_to_wals_auto) %>%
  distinct()

#' Add WALS names
afrobarometer_other_to_wals %<>%
  left_join(select(wals, wals_code, wals_name = Name),
            by = "wals_code")

#' Merge with Original
afrobarometer_other_to_wals <-
  # Outer join to ensure that all Afrobarometer Languages are accounted for
  left_join(afrobarometer_langs_other,
             afrobarometer_other_to_wals,
             by = c(iso_alpha2 = "country", "lang_name")) %>%
  select(round, question, country, value, iso_alpha2, wals_code,
         wals_name, auto, distance, same_country, in_wals_country) %>%
  arrange(round, question, country, value)

#'
#' # Tests
#'

# check primary key
assert_that(
  nrow(distinct(afrobarometer_other_to_wals,
                round, question, country, value, wals_code)) ==
    nrow(afrobarometer_other_to_wals)
)

#' Check columns
with(afrobarometer_other_to_wals, {

  assert_that(all(!is.na(round)))
  assert_that(is_integerish(round))

  assert_that(is.character(question))
  assert_that(all(!is.na(question)))

  # Lang Id
  assert_that(all(!is.na(country)))
  assert_that(is.integer(country))
  assert_that(all(country >= 1 & country <= 36))

  # Lang Id
  assert_that(all(!is.na(value)))
  assert_that(is.character(value))

  # Wals code
  assert_that(is.character(wals_code))
  assert_that(all(str_detect(na.omit(wals_code), misc_data$wals$code_pattern)))

  assert_that(all(!is.na(iso_alpha2)))
  assert_that(is.character(iso_alpha2))
  assert_that(all(is.na(iso_alpha2) | str_detect(iso_alpha2, "^[A-Z]{2}$")))

  # Wals name
  assert_that(is.character(wals_name))
  assert_that(all(is.na(wals_code) | !is.na(auto)))
  assert_that(all(na.omit(auto) %in% c(0, 1)))

  assert_that(all(is.na(distance) | distance >= 0))
  assert_that(all(is.na(auto) | auto %in% c(1, 0)))

})

#' Check that all WALS codes are matched
missing_wals <-
  afrobarometer_other_to_wals %>%
  filter(is.na(wals_code)) %>%
  # missing ISO shouldn't match
  anti_join(filter(IO$afrobarometer_other_to_iso, iso_scope == "S"),
            by = c("country", "value"))
if (nrow(missing_wals) > 0) {
  print(missing_wals)
  stop("Missing WALS codes found")
}

#' Check that all WALS codes are valid
#'
#' wals_codes can be missing, but if non-missing must appear in WALS dataset
#'
wals_nonmatches <- afrobarometer_other_to_wals %>%
  filter(!is.na(wals_code)) %>%
  anti_join(wals, by = c("wals_code"))

if (nrow(wals_nonmatches) > 0) {
  print(wals_nonmatches)
  stop("There exist invalid WALS codes")
}

#' All WALS languages should be from the African Macrolanguage unless accounted
#' for.
wals_non_african <-
  inner_join(afrobarometer_other_to_wals,
             select(wals, wals_code, macroarea),
             by = "wals_code") %>%
  filter(!is.na(macroarea)) %>%
  filter(!wals_code %in% misc_data$wals$non_african$values) %>%
  filter(!(macroarea %in% "Africa"))
if (nrow(wals_non_african) > 0) {
  print(wals_non_african, n = 100, width = 10000)
  stop("There exist unaccounted for non-African languages in the data:\n")
}

matches_unrelated_langs <-
  afrobarometer_other_to_wals %>%
  # remove WALS non-matches
  filter(!is.na(wals_code)) %>%
  left_join(select(wals, wals_code, genus), by = "wals_code") %>%
  group_by(round, question, country, value) %>%
  summarise(n_genus = length(unique(genus))) %>%
  filter(n_genus > 1)

#' All manual matches should be in the correct country
country_nonmatches <-
  afrobarometer_other_to_wals %>%
  filter(!is.na(wals_code)) %>%
  filter(!auto) %>%
  anti_join(IO$afrobarometer_other_to_wals_country_nonmatches,
            by = c("value", "iso_alpha2", "wals_code")) %>%
  anti_join(unnest(select(wals, wals_code, countrycodes)),
            by = c("wals_code", iso_alpha2 = "countrycodes")) %>%
  select(value, iso_alpha2, wals_code, wals_name) %>%
  distinct()

if (nrow(country_nonmatches)) {
  print(country_nonmatches)
  stop("Afrobarometer-to-WALS bad country matches found")
}

#' Write output
afrobarometer_other_to_wals %>%
  write_csv(path = OUTPUT, na = "")
