# Glottolog distances
languages_respno <- IO$languages_respno %>%
  filter(variable %in% c("respondent", "interview"),
         !is.na(glottocode))

uniq_langs <- unique(languages_respno$glottocode)

glottolog_db <- src_sqlite(here::here("external", "lingdata", "glottolog.db"))

glottolog_dists <- tbl(glottolog_db, "distances") %>%
  filter(glottocode_1 %in% uniq_langs,
         glottocode_2 %in% uniq_langs) %>%
  select(glottocode_1, glottocode_2, shared) %>%
  collect()

max_depth <- tbl(glottolog_db, "languoids") %>%
  filter(!bookkeeping) %>%
  summarise(max_depth = max(depth, na.rm = TRUE)) %>%
  collect() %>%
  pluck("max_depth")

# District level measures
lang_dists_by_country <- languages_respno %>%
  filter(variable %in% c("interview", "respondent")) %>%
  filter(!is.na(glottocode)) %>%
  count(round, variable, country, glottocode) %>%
  group_by(round, variable, country) %>%
  mutate(p = n / sum(n),
         to = list(tibble(glottocode_to = glottocode, p_to = p))) %>%
  unnest(to)  %>%
  group_by(round, variable, country) %>%
  filter(glottocode != glottocode_to) %>%
  left_join(glottolog_dists,
              by = c(glottocode = "glottocode_1",
                     glottocode_to = "glottocode_2")) %>%
  mutate(shared = if_else(is.na(shared), 0L, shared))


lf_by_country <-
  lang_dists_by_country %>%
  mutate(dist1 = 1 - (shared / max_depth) ^ 0.5,
         dist2 = 1 - (shared / max_depth) ^ 0.05) %>%
  group_by(round, variable, country) %>%
  summarise(lf_50 = sum(exp(log(p) + log(p_to) + log(dist1))),
            lf_05 = sum(p * p_to * dist2))

lang_dists_by_country %>%
  mutate(d = list(seq_len(max_depth))) %>%
  unnest() %>%
  ungroup() %>%
  mutate(dist = p * p_to * (shared < d)) %>%
  group_by(round, variable, country, d) %>%
  summarise(dist = sum(dist)) %>%
  mutate(d = sprintf("lf_d%02d", d)) %>%
  spread(d, dist)
