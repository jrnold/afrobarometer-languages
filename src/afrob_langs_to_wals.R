#' ---
#' title: "Mapping from Afrobarometer Languages to WALS"
#' ----
#' 
#' This uses the Afrobarometer to ISO 639-3 matches and the ISO 639-3 to 
#' WALS mappings to generate a Afrobarometer language to WALS mapping.
#' 

#' load ISO to WALS mappings
iso_to_wals <- readRDS(project_path("data", "iso_to_wals.rds"))

#' 
#' load the Afrobarometer to ISO mappings
afrob_langs_to_iso <- 
  readRDS(project_path("data", "afrob_langs_to_iso.rds")) %>%
  rename(iso_code = iso_639_3)

#' 
#' Merge Afrobarometer -> ISO with WALS -> ISO
#' 
#' - keep distinct matches to drop duplicate matches
#' - drop any with missing WALS codes
#' 
#' This dataset will not include any Afrobarometer languages that do not have
#' a match.
#' 
afrob_langs_to_wals <-
  left_join(afrob_langs_to_iso,
            select(iso_to_wals, iso_code, wals_code),
            by = "iso_code") %>%
  select(-iso_code) %>%
  dplyr::filter(!is.na(wals_code)) %>%
  distinct() %>%
  group_by(lang_id, question) %>%
  mutate(n_matches = n())

#' Documentation
comments <- list(
  NULL = str_c("Mapping from Afrobarometer r6 languages in Q2 and Q3 to WALS ",
               "(World Atlas of Language Structures) languages."),
  question = c("Afrobarometer r6 question number (Q2 or Q103)"),
  lang_id = c("Afrobarometer r6 language number. ",
                    "Languages can have different names and numbers across ",
                    "the questions, which is why question and id are needed."),
  lang_name = "Afrobarometer language name",
  wals_code = c("WALS language code")
)
afrob_langs_to_wals %<>% add_description(comments)

#' 
#' # Tests
#' 
data_exists(afrob_langs_to_wals)
with(afrob_langs_to_wals, {
  assert_that(all(question %in% c("Q2", "Q103")))
  assert_that(all(lang_id %in% c(-1:10000)))
  assert_that(all(str_detect(wals_code, "^[a-z]{2,3}$")))
})

#' 
## ------------------------------------------------------------------------
saveRDS(afrob_langs_to_wals,
        project_path("data", "afrob_langs_to_wals.rds"))
