source("src/R/init.R")

OUTFILE <- project_path("data", "afraborometer_lang_dists.csv.gz")

glottolog_distances <- function() {
  glottolog_db <- src_sqlite("external/lingdata/glottolog.db")

  ab_to_glottolog <- IO$afrobarometer_to_glottolog %>%
    select(round, country, variable, lang_id, country, glottocode, iso_alpha2) %>%
    filter(!is.na(glottocode))

  glottolog_languoids <- tbl(glottolog_db, "languages") %>%
    filter(!bookkeeping) %>%
    collect()
  max_depth <- glottolog_languoids %>%
    filter(level == "language") %>%
    pluck("depth") %>%
    max(na.rm = TRUE)

  # Dialects to languages
  ab_to_glottolog_dialects <-
    ab_to_glottolog %>%
    filter(level == "dialect") %>%
    select(-glottocode, -descendants, -level) %>%
    unnest(ancestors) %>%
    rename(glottocode = ancestors) %>%
    semi_join(filter(glottolog, level == "language"), by = "glottocode")

  # families to languages
  ab_to_glottolog_families <-
    ab_to_glottolog %>%
    filter(level == "family") %>%
    select(-glottocode, -ancestors, -level) %>%
    unnest(descendants) %>%
    rename(glottocode = descendants) %>%
    semi_join(filter(glottolog, level == "language"), by = "glottocode")

  ab_to_glottolog <-
    bind_rows(
      select(filter(ab_to_glottolog, level == "language"), -level),
      ab_to_glottolog_families,
      ab_to_glottolog_dialects
    ) %>%
    select(glottocode, round, country, variable, lang_id, depth, ancestors)

  ab_dists <-
    crossing(rename_all(ab_to_glottolog, str_c, "_1"),
           rename_all(ab_to_glottolog, str_c, "_2")) %>%
    filter((round_1 != round_2) |
             (variable_1 != variable_2) |
             (country_1 != country_2) |
             (lang_id_1 < lang_id_2))

  left_join(ab_dists,
            tbl(glottolog_db, "distances"),
            copy = TRUE, by = c("glottocode_1", "glottocode_2")) %>%
    group_by(round_1, variable_1, country_1, lang_id_1,
             round_2, variable_2, country_2, lang_id_2) %>%
    summarise_at(shared = median(shared),
                 geo = mean(geo))
}

asjp_distances <- function() {
  ethnologue_dists <- IO$ethnologue_distances

}


run <- function() {
  distances <- glottolog_distances()
  hdl <- gzfile(OUTFILE, "w")
  write_csv(distances, hdl, na = "")
  close(hdl)
}

run()
