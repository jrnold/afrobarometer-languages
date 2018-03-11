source(here::here("src", "R", "init.R"))

OUTFILE <- project_path("data", "afraborometer_lang_dists.csv.gz")

glottolog_distances <- function() {
  glottolog_db <- src_sqlite("external/lingdata/glottolog.db")

  ab_to_glottolog <- IO$afrobarometer_to_glottolog %>%
    select(round, country, variable, lang_id, iso_alpha2, glottocode, level) %>%
    filter(!is.na(glottocode))

  glottolog_languoids <- tbl(glottolog_db, "languoids") %>%
    filter(!bookkeeping) %>%
    collect()

  max_depth <- glottolog_languoids %>%
    filter(level == "language") %>%
    pluck("depth") %>%
    max(na.rm = TRUE)

  glottolog_languages <-
    tbl(glottolog_db, "languoids") %>%
    filter(level == "language") %>%
    select(glottocode) %>%
    collect()

  glottolog_dialects <-
    tbl(glottolog_db, "languoids") %>%
    filter(level == "dialect") %>%
    select(glottocode) %>%
    collect()

  glottolog_families <-
    tbl(glottolog_db, "languoids") %>%
    filter(level == "family") %>%
    select(glottocode) %>%
    collect()

  # dialects to languages
  dialect2lang <-
    glottolog_dialects %>%
    left_join(tbl(glottolog_db, "paths"),
              by = "glottocode", copy = TRUE) %>%
    filter(dist > 0) %>%
    inner_join(rename(glottolog_languages, glottocode_to = glottocode),
               by = "glottocode_to") %>%
    group_by(glottocode) %>%
    filter(dist == min(dist)) %>%
    select(glottocode, glottocode_to)

  # dialects to languages
  family2lang <- glottolog_families %>%
    left_join(tbl(glottolog_db, "paths"),
              by = "glottocode", copy = TRUE) %>%
    filter(dist < 0) %>%
    inner_join(rename(glottolog_languages, glottocode_to = glottocode),
               by = "glottocode_to") %>%
    select(glottocode, glottocode_to)

  lang2lang <- select(glottolog_languages, glottocode) %>%
    mutate(glottocode_to = glottocode)

  ab_to_glottolog %>%
    left_join(bind_rows(family2lang, dialect2lang, lang2lang),
              by = "glottocode") %>%
    select(round, variable, lang_id, iso_alpha2, glottocode = glottocode_to)

  ab_dists <-
    crossing(rename_all(ab_to_glottolog, str_c, "_1"),
             rename_all(ab_to_glottolog, str_c, "_2")) %>%
    filter((round_1 != round_2) |
             (variable_1 != variable_2) |
             (iso_alpha2_1 != iso_alpha2_2) |
             (lang_id_1 != lang_id_2)) %>%
    left_join(select(tbl(glottolog_db, "distances"), -geo),
              by = c("glottocode_1", "glottocode_2"),
              copy = TRUE) %>%
    group_by(round_1, round_2, variable_1, variable_2,
             lang_id_1, lang_id_2, iso_alpha2_1, iso_alpha2_2) %>%
    summarise(shared = mean(shared),
              dist = mean(sqrt(shared / max_depth)))


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
    mutate(asjp_language_ = if_else(asjp_language_2 < asjp_language_1,
                                    asjp_language_1, asjp_language_2),
           asjp_language_1 = if_else(asjp_language_2 < asjp_language_1,
                                     asjp_language_2, asjp_language_1),
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
