#' ---
#' title: "Mapping from Afrobarometer Languages to WALS"
#' ----
#'
#' This uses the Afrobarometer to ISO 639-3 matches and the ISO 639-3 to
#' WALS mappings to generate a Afrobarometer language to WALS mapping.
#'
#'



ISO_SPECIAL_CODES <- c("mul", "mis", "und", "zxx")

OUTPUT <- find_rstudio_root_file("data", "afrobarometer_to_wals.csv")

INPUTS <- list(
  afrobarometer_langs = list("data", "afrobarometer_langs.csv"),
  afrobarometer_countries = list("data-raw", "afrobarometer_countries.csv"),
  afrobarometer_to_wals = list("data-raw", "afrobarometer_to_wals"),
  misc_data = list("data-raw", "misc.yml"),
  afrobarometer_to_iso = list("data", "afrobarometer_to_iso_639_3.csv"),
  wals = list("external", "wals", "language.csv"),
  iso_to_wals = list("data", "iso_to_wals.csv")
) %>%
{setNames(map(., function(x) invoke(find_rstudio_root_file, x)),
          names(.))}

misc_data <- yaml.load_file(INPUTS$misc_data)

afrobarometer_langs_other_to_wals <-
  yaml.load_file(filename) %>%
    map(compact) %>%
    map(function(.x) {
      if (is.null(.x$wals_code)) {
        NULL
      } else {
        out <- tidyr::crossing(question = .x[["question"]],
                               wals_code = .x[["wals_code"]])
        out[["lang_id"]] <- .x$lang_id
        out[["round"]] <- ab_round
        out
      }
    }) %>%
    bind_rows()
}

afrobarometer_to_wals_manual <- dir(INPUTS[["afrobarometer_to_wals"]], pattern = "*.yml", full.names = TRUE) %>%
    map_df(read_afrobarometer_to_wals)


wals <- read_csv(INPUTS$wals,
                   col_types = cols(
                     .default = col_character(),
                     latitude = col_double(),
                     longitude = col_double()
                   )) %>%
    select(wals_code, Name, latitude, longitude, genus,
           family, macroarea, countrycodes)

iso_to_wals <- read_csv(INPUTS[["iso_to_wals"]], na = "",
                        col_types = cols(
                          iso_code = col_character(),
                          wals_code = col_character(),
                          iso_code_to = col_character(),
                          distance = col_integer()
                        ))



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
  select(round, question, lang_id, iso_code, iso_scope) %>%
  anti_join(afrobarometer_to_wals_manual,
            by = c("round", "question", "lang_id")) %>%
  inner_join(select(iso_to_wals, iso_code, wals_code, distance),
            by = "iso_code") %>%
  # Keep distinct WALS codes
  group_by(round, question, lang_id, wals_code) %>%
  summarise(distance = min(distance)) %>%
  ungroup()

#' Combine the auto and manual matches
afrobarometer_to_wals <-
  bind_rows(mutate(select(afrobarometer_to_wals_manual, round, question, lang_id, wals_code),
                   auto = FALSE),
            mutate(afrobarometer_to_wals_auto, auto = TRUE))

#' Add WALS names
afrobarometer_to_wals %<>%
  left_join(select(wals, wals_code, wals_name = Name),
            by = "wals_code") %>%
  # Outer join to ensure that all Afrobarometer Languages are accounted for
  right_join(select(afrobarometer_langs, round, question, lang_id, lang_name),
             by = c("round", "question", "lang_id")) %>%
  select(round, question, lang_id, lang_name, wals_code, wals_name,
         auto, distance) %>%
  arrange(round, question, lang_id)

