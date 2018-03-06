source(here::here("src", "R", "init.R"))

OUTFILE <- project_path("data", "afraborometer_lang_dists.csv.gz")

glottolog_distances <- function() {
  glottolog_db <- src_sqlite("external/lingdata/glottolog.db")

  ab_to_glottolog <- IO$afrobarometer_to_glottolog %>%
    select(round, country, variable, lang_id, country, glottocode, iso_alpha,
           level) %>%
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
    left_join(ab_to_glottolog, glottolog_languoids, by = "glottocode") %>%
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
  asjp_db <- src_sqlite("external/lingdata/asjp.db")
  ajsp_langs <- collect(tbl(asjp_db, "languages"))
  afrobarometer_to_asjp <-
    left_join(IO$afrobarometer_to_iso,
              select(asjp_langs, asjp_language = language,
                     iso_639_3 = iso), by = c("iso_639_3")) %>%
    filter(!is.na(asjp_language)) %>%
    select(round, variable, lang_id, country, asjp_language) %>%
    distinct()

  dists <-
    crossing(rename_all(afrobarometer_to_asjp, str_c, "_1"),
           rename_all(afrobarometer_to_asjp, str_c, "_2")) %>%
    filter((round_1 != round_2) |
           (variable_1 != variable_2) |
           (country_1 != country_2) |
           (lang_id_1 < lang_id_2)) %>%
    # db only has distances for language_1 < language_2
    mutate(asjp_language_ = if_else(asjp_language_2 < asjp_language_1, asjp_language_1, asjp_language_2),
           asjp_language_1 = if_else(asjp_language_2 < asjp_language_1, asjp_language_2, asjp_language_1),
           asjp_language_2 = asjp_language_) %>%
    select(-asjp_language_)

 inner_join(dists,
            select(tbl(asjp_db, "distances"),
                    language_1, language_2, ldnd, ldn),
            by = c("asjp_language_1" = "language_1",
                    "asjp_language_2" = "language_2"),
            copy = TRUE) %>%
    group_by(round_1, variable_1, lang_id_1, country_1,
             round_2, variable_2, lang_id_2, country_2) %>%
    summarise(asjp_ldn = mean(ldn, na.rm = TRUE),
              asjp_ldnd = mean(ldnd, na.rm = TRUE))

}

run <- function() {
  distances <-
    left_join(glottolog_distances(),
              asjp_distances(),
              by = c("round_1", "variable_1", "lang_id_1", "country_1",
                     "round_2",  "variable_2", "lang_id_2", "country_2"))
  hdl <- gzfile(OUTFILE, "w")
  write_csv(distances, hdl, na = "")
  close(hdl)
}

run()
