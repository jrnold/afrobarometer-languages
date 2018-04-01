#' ---
#' title: Language names to codes mappings
#' ---
source(here::here("src", "R", "init.R"))

OUTPUT <- here::here("data", "language_names.csv")

glottolog_db <- src_sqlite(here::here("external", "lingdata", "glottolog.db"))

glotto2wals <- tbl(glottolog_db, "wals_codes") %>%
  collect() %>%
  group_by(glottocode) %>%
  summarise(wals_code = str_c(sort(unique(wals_code)), collapse = " ")) %>%
  deframe()

process_lang <- function(x) {
  x[["country"]] <- sort(unique(x[["country"]]))
  x[["name"]] <- sort(unique(x[["name"]]))
  x[["iso_639_3"]] <- str_c(x[["iso_639_3"]], collapse = " ")
  if (is.null(x[["glottocode"]])) {
    x[["glottocode"]] <- NA_character_
  }
  #  try(!is.na(x[["glottocode"]]))
  x[["wals"]] <- if (is.null(x[["wals"]])) {
    if (!is.na(x[["glottocode"]])) {
      if (!x[["glottocode"]] %in% names(glotto2wals)) {
        print(x)
      }
      str_c(glotto2wals[[x[["glottocode"]]]], collapse = " ")
    } else {
      NA_character_
    }
  } else {
    x[["wals"]] <- str_c(x[["wals"]], collapse = " ")
  }
  cross_df(x[c("country", "name", "iso_639_3", "glottocode", "wals")])

}

language_names <-  IO$languages_raw %>%
  map_dfr(process_lang)

# Create multiple-language data entries
multi_languages <- read_yaml(here::here("data-raw", "multiple-languages.yml")) %>%
  enframe() %>%
  unnest() %>%
  group_by(name) %>%
  mutate(lang_number = row_number()) %>%
  left_join(rename(language_names, value = name), by = "value") %>%
  select(-value) %>%
  rename(lang_name = name)

bind_rows(mutate(language_names, lang_number = 1L),
          multi_languages) %>%
  # in case I accidentally add extra lang data this will keep most mistakes
  # from propogating
  distinct() %>%
  write_csv(OUTPUT, na = "")
