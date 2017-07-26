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

iso_to_wals <- ungroup(IO$iso_to_wals)

wals <- IO$wals

# Read Afrobarometer Other Languages
afrobarometer_langs_other <- IO$afrobarometer_langs_other %>%
  # add variable to merge on
  mutate(lang_name = str_to_lower(value))

#' Afrobarometer Mappings
afrobarometer_other_to_wals_manual <- IO$afrobarometer_other_mappings %>%
  map_df(function(.x) {
    if (!is.null(.x[["wals_code"]])) {
      out <- tidyr::crossing(country = .x[["country"]],

                             wals_code = .x[["wals_code"]])
      out[["lang_name"]] <- .x$lang_name
      out
    }
  }) %>%
  distinct()

afrobarometer_other_to_wals_auto <- IO$afrobarometer_other_to_iso %>%
  filter(iso_scope %in% "I") %>%
  mutate(lang_name = str_to_lower(value)) %>%
  select(country = iso_alpha2, lang_name, iso_639_3) %>%
  # there can multiples of the above
  distinct() %>%
  anti_join(afrobarometer_other_to_wals_manual,
            by = c("country", "lang_name")) %>%
  inner_join(select(ungroup(iso_to_wals), iso_639_3 = iso_code, wals_code, distance),
            by = "iso_639_3") %>%
  # Keep distinct WALS codes
  group_by(country, lang_name, wals_code) %>%
  summarise(distance = min(distance)) %>%
  ungroup()

#' Combine the auto and manual matches
afrobarometer_other_to_wals <-
  bind_rows(mutate(afrobarometer_other_to_wals_manual, auto = FALSE),
            mutate(afrobarometer_other_to_wals_auto, auto = TRUE)) %>%
  distinct()

#' Add WALS names
afrobarometer_other_to_wals %<>%
  left_join(select(wals, wals_code, wals_name = Name),
            by = "wals_code")

afrobarometer_other_to_wals <-
  # Outer join to ensure that all Afrobarometer Languages are accounted for
  left_join(afrobarometer_langs_other,
             afrobarometer_other_to_wals,
             by = c(iso_alpha2 = "country", "lang_name")) %>%
  select(round, question, country, value, iso_alpha2, wals_code,
         wals_name, auto, distance) %>%
  arrange(round, question, country, value)

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

#' All WALS languages should be from the Africa Macrolanguage unless accounted
#' for.
wals_non_african <-
  inner_join(afrobarometer_other_to_wals,
             select(wals, wals_code, macroarea),
             by = "wals_code") %>%
  filter(!wals_code %in% misc_data$wals$other_non_african$values) %>%
  filter(!(macroarea %in% "Africa"))
if (nrow(wals_non_african) > 0) {
  print(wals_non_african, n = 100, width = 10000)
  stop("There exist unaccounted for non-African languages in the data:\n")
}

with(afrobarometer_other_to_wals, {

  #assert_that(all(!is.na(round)))
  #assert_that(is.character(round))

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
  assert_that(is.logical(auto))

  assert_that(all(is.na(distance) | distance >= 0))

})



#' Write output
afrobarometer_other_to_wals %>%
  write_csv(path = OUTPUT, na = "")
