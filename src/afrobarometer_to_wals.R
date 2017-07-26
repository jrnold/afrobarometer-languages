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
  })

afrobarometer_to_iso <- IO$afrobarometer_to_iso

wals <- IO$wals

iso_to_wals <- IO$iso_to_wals

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
  select(round, question, lang_id, iso_639_3, iso_scope) %>%
  anti_join(afrobarometer_to_wals_manual,
            by = c("round", "question", "lang_id")) %>%
  inner_join(select(ungroup(iso_to_wals), iso_639_3 = iso_code, wals_code, distance),
            by = "iso_639_3") %>%
  # Keep distinct WALS codes
  group_by(round, question, lang_id, wals_code) %>%
  summarise(distance = min(distance)) %>%
  ungroup()

#' Combine the auto and manual matches
afrobarometer_to_wals <-
  bind_rows(mutate(select(afrobarometer_to_wals_manual, round, question, lang_id, wals_code),
                   auto = FALSE),
            mutate(afrobarometer_to_wals_auto, auto = TRUE)) %>%
  distinct()

#' Add WALS names
afrobarometer_to_wals %<>%
  left_join(select(wals, wals_code, wals_name = Name),
            by = "wals_code") %>%
  # Outer join to ensure that all Afrobarometer Languages are accounted for
  right_join(select(IO$afrobarometer_langs, round, question,
                    lang_id = value, lang_name = name),
             by = c("round", "question", "lang_id")) %>%
  select(round, question, lang_id, lang_name, wals_code, wals_name,
         auto, distance) %>%
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
