#' ---
#' title: "Mapping from Afrobarometer Languages to WALS"
#' ----
#'
#' This uses the Afrobarometer to ISO 639-3 matches and the ISO 639-3 to
#' WALS mappings to generate a Afrobarometer language to WALS mapping.
#'
#'

library("tidyverse")
library("rprojroot")
library("magrittr")
library("yaml")
library("jsonlite")
library("stringr")
library("igraph")

OUTPUT <- find_rstudio_root_file("data", "afrobarometer_to_wals.csv")

INPUTS <- list(
  afrobarometer_langs = list("data", "afrobarometer_langs.csv"),
  iso_langs = list("external",
                   "iso-639-3",
                   "iso-639-3_Code_Tables_20170217",
                   "iso-639-3_20170202.tab"),
  afrobarometer_countries = list("data-raw", "afrobarometer_countries.csv"),
  afrobarometer_to_wals = list("data-raw", "afrobarometer_to_wals"),
  afrobarometer_to_wals_tests = list("data-raw", "afrobarometer_to_wals_tests.yml"),
  afrobarometer_to_iso = list("data", "afrobarometer_to_iso_639_3.csv"),
  wals = list("external", "wals", "language.csv"),
  wals_update = list("data-raw", "wals-updates.csv"),
  ethnologue_tree = list("data-raw", "ethnologue-tree.json")
) %>%
{setNames(map(., function(x) invoke(find_rstudio_root_file, x)),
          names(.))}

read_afrobarometer_langs <- function() {
  read_csv(INPUTS$afrobarometer_langs, na = "",
           col_types = cols_only(
             round = col_character(),
             question = col_character(),
             value = col_integer(),
             name = col_character(),
             countries = col_character()
           )) %>%
    mutate(lang_id = value,
           lang_name = name)
}
afrobarometer_langs <- read_afrobarometer_langs()

read_afrobarometer_to_wals <- function(filename) {
  ab_round <- tools::file_path_sans_ext(basename(filename))
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

read_afrobarometer_to_wals_all <- function() {
  dir(INPUTS[["afrobarometer_to_wals"]], pattern = "*.yml", full.names = TRUE) %>%
    map_df(read_afrobarometer_to_wals)
}

afrobarometer_to_wals_manual <- read_afrobarometer_to_wals_all()

read_wals <- function() {
  wals <- read_csv(INPUTS$wals,
                   col_types = cols(
                     .default = col_character(),
                     latitude = col_double(),
                     longitude = col_double()
                   )) %>%
    select(wals_code, iso_code, Name, latitude, longitude, genus,
           family, macroarea, countrycodes)

  wals_updates <- read_csv(INPUTS$wals_update,
                          col_types =
                            cols_only(
                              wals_code = col_character(),
                              iso_code = col_character()
                            )) %>%
    select(wals_code, iso_code_new = iso_code)

  # update iso_codes
  left_join(wals, wals_updates, by = "wals_code") %>%
    mutate(iso_code = coalesce(iso_code_new, iso_code)) %>%
    select(-iso_code_new)
}
wals <- read_wals()

read_iso_langs <- function() {
    read_tsv(INPUTS$iso_langs,
             col_names =
               c("Id", "Part2B", "Part2T", "Part1", "Scope", "Language_Type",
                 "Ref_Name", "Comment"),
             col_types =
               cols_only(
                 Id = col_character(),
                 Scope = col_character(),
                 Language_Type = col_character(),
                 Ref_Name = col_character()
               ), na = "") %>%
      filter(Language_Type == "L", Scope == "I") %>%
      select(-Language_Type, -Scope)
}
iso_langs <- read_iso_langs()

get_edges <- function(x, name) {
  subgroup_edges <- if (length(x[["subgroups"]])) {
    map(names(x[["subgroups"]]), ~ tibble(from = name, to = .x))
  } else {
    NULL
  }
  lang_edges <- if (length(x[["languages"]])) {
    map(names(x[["languages"]]), ~ tibble(from = name, to = .x))
  } else {
    NULL
  }
  if (length(x[["subgroups"]])) {
    lower_edges <- map2_df(x[["subgroups"]],
                           names(x[["subgroups"]]),
                           get_edges)
  } else {
    lower_edges <- NULL
  }
  bind_rows(subgroup_edges, lang_edges, lower_edges)
}

read_ethnologue_tree <- function() {
  fromJSON(INPUTS$ethnologue_tree) %>%
   {map2(., names(.), get_edges)}
}
ethnologue_tree <- read_ethnologue_tree()

make_distances <- function(x) {
  g <- graph_from_data_frame(x)
  as_tibble(rownames_to_column(as.data.frame(distances(g)), "from")) %>%
    gather(to, distance, -from) %>%
    dplyr::filter(str_detect(to, "/language"),
                  str_detect(from, "/language")) %>%
    dplyr::mutate(to = str_replace(to, "/language/(.*)/\\d+", "\\1"),
                  from = str_replace(from, "/language/(.*)/\\d+", "\\1"))
}

#' ISO to WALS
#'
#' For all ISO 639-3 languages in Ethnologue find the closest ISO-639-3
#' language(s) associated with a WALS language (within the Ethnologue family).
#'
iso_to_wals <-
  map_df(ethnologue_tree, make_distances) %>%
  inner_join(select(wals, wals_code, iso_code),
            by = c("to" = "iso_code")) %>%
  rename(iso_code = from, iso_code_to = to) %>%
  group_by(iso_code) %>%
  filter(distance == min(distance))

read_afrobarometer_to_iso <- function() {
  read_csv(INPUTS$afrobarometer_to_iso, na = "",
    col_types = cols_only(
      round = col_character(),
      question = col_character(),
      iso_639_3 = col_character(),
      lang_id = col_integer()
    )) %>%
    rename(iso_code = iso_639_3)
}
afrobarometer_to_iso <- read_afrobarometer_to_iso()

#' For any Afrobarometer languages without manual matches, the
#' WALS code is found by
#'
#' - taking the ISO Matches from afrobarometer_to_iso
#' - match all codes of an anfrobarometer_language with the closest WALS languages
#'    using the table previously constructed
#' - for each Afrobarometer language, keep the "closest" WALS language(s)
afrobarometer_to_wals_auto <-
  select(afrobarometer_to_iso, round, question, lang_id, iso_code) %>%
  # remove values from special
  filter(!iso_code %in% c("mul", "mis", "und", "zxx")) %>%
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
  bind_rows(mutate(afrobarometer_to_wals_manual,
                   auto = FALSE),
            mutate(afrobarometer_to_wals_auto,
                   auto = TRUE))

#' Add some of the original values
afrobarometer_to_wals %<>%
  left_join(select(wals, wals_code, wals_name = Name,
                   latitude, longitude, genus, family, countrycodes, macroarea),
            by = "wals_code") %>%
  left_join(select(afrobarometer_langs, round, question, lang_id, lang_name),
            by = c("round", "question", "lang_id")) %>%
  # ensure that all empty wals_codes are missing
  mutate(wals_code = if_else(str_trim(wals_code) == "",
                             NA_character_, wals_code)) %>%
  select(round, question, lang_id, lang_name, wals_code, wals_name,
         everything()) %>%
  arrange(round, question, lang_id)

#' Check that everything matches or is accounted for
# afrob_nonmatches <- anti_join(afrobarometer_langs,
#                               afrobarometer_to_wals,
#                         by = c("question", "lang_id"))
# stopifnot(nrow(afrob_nonmatches) == 0L)

#' Check that all WALS codes are valid
#'
#' wals_codes can be missing, but if non-missing must appear in WALS dataset
#'
wals_nonmatches <- anti_join(filter(afrobarometer_to_wals, !is.na(wals_code)),
                             wals, by = c("wals_code"))
stopifnot(nrow(wals_nonmatches) == 0L)

#' All WALS languages should be from the Africa Macrolanguage unless accounted
#' for.
tests_data <- yaml.load_file(INPUTS$afrobarometer_to_wals_tests)
wals_non_african <-
  afrobarometer_to_wals %>%
  filter(!(wals_code %in% tests_data$non_african$values)) %>%
  filter(!(macroarea %in% "Africa"))
stopifnot(nrow(wals_non_african) == 0L)


#' Write output
write_afrobarometer_to_wals <- function(x, path) {
  write_csv(x, path = path, na = "")
}
write_afrobarometer_to_wals(afrobarometer_to_wals, OUTPUT)
