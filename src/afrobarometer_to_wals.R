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
      out <- tidyr::crossing(question = .x[["question"]],

                             wals_code = .x[["wals_code"]])
      out[["lang_id"]] <- .x$lang_id
      out[["round"]] <- .x$round
      out
    }
  }) %>%
  mutate(auto = 0L, distance = 0L)

afrobarometer_to_iso <- IO$afrobarometer_to_iso

wals <- IO$wals

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
  # remove values from special
  filter(iso_scope == "I") %>%
  select(round, question, lang_id, iso_639_3) %>%
  anti_join(afrobarometer_to_wals_manual,
            by = c("round", "question", "lang_id")) %>%
  inner_join(iso_to_wals, by = "iso_639_3") %>%
  select(-iso_639_3) %>%
  # Keep WALS codes with best matches
  group_by(round, question, lang_id) %>%
  filter(distance == min(distance)) %>%
  group_by(round, question, lang_id) %>%
  filter(same_country == max(same_country)) %>%
  ungroup() %>%
  distinct() %>%
  mutate(auto = 1L)

#' Combine the auto and manual matches
afrobarometer_to_wals <-
  bind_rows(select(afrobarometer_to_wals_manual, round, question, lang_id, wals_code, auto),
            afrobarometer_to_wals_auto) %>%
  distinct()

#' Add WALS names
afrobarometer_to_wals %<>%
  left_join(select(wals, wals_code, wals_name = Name),
            by = "wals_code") %>%
  # Outer join to ensure that all Afrobarometer Languages are accounted for
  right_join(select(IO$afrobarometer_langs, round, question,
                    lang_id = value, lang_name = name),
             by = c("round", "question", "lang_id")) %>%
  # Add countries
  left_join(select(IO$afrobarometer_to_wals_countries,
                   round, question, lang_id, wals_code,
                   countries),
            by = c("round", "question", "lang_id", "wals_code")) %>%
  select(round, question, countries, lang_id, lang_name, wals_code, wals_name,
         auto, distance, same_country) %>%
  arrange(round, question, lang_id)


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
#'
matches_unrelated_langs <-
  afrobarometer_to_wals %>%
  # remove WALS non-matches
  filter(!is.na(wals_code)) %>%
  # remove known errors
  anti_join(map_df(misc_data$wals$matches_unrelated_langs$values,
                   as_tibble),
            by = c("round", "question", "lang_id")) %>%
  left_join(select(wals, wals_code, genus), by = "wals_code") %>%
  group_by(round, question, lang_id, lang_name) %>%
  summarise(n_genus = length(unique(genus))) %>%
  filter(n_genus > 1)

# Code to generate yaml to add to misc.yml for non-matches to ignore
# matches_unrelated_langs %>%
#   select(round, question, lang_id, lang_name) %>%
#   yaml::as.yaml(column.major = FALSE) %>%
#   cat()



if (nrow(matches_unrelated_langs) > 0) {
  print(matches_unrelated_langs)
  stop("Unrelated WALS languages detected")
}

with(afrobarometer_to_wals, {
  assert_that(all(!is.na(round)))
  assert_that(is.character(round))

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
