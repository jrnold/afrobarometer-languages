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
  if (is.null(x[["wals"]])) {
    x[["wals"]] <- if (!is.na(x[["glottocode"]])) {
      str_c(glotto2wals[[x[["glottocode"]]]], collapse = " ")
    } else {
      NA_character_
    }
  } else {
    x[["wals"]] <- str_c(x[["wals"]], collapse = " ")
  }
  cross_df(x[c("country", "name", "iso_639_3", "glottocode", "wals")])

}

IO$languages_raw %>%
  map_dfr(process_lang) %>%
  write_csv(OUTPUT, na = "")
