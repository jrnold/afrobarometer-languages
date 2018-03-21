source(here::here("src", "R", "init.R"))

# Glottolog distances
languages_respno <- IO$languages_respno %>%
  filter(variable %in% c("respondent", "interview"),
         !is.na(glottocode))

uniq_langs <- unique(languages_respno$glottocode)

glottolog_dists <- tbl(IO$glottolog, "distances") %>%
  filter(glottocode_1 %in% uniq_langs,
         glottocode_2 %in% uniq_langs) %>%
  select(glottocode = glottocode_1, glottocode_to = glottocode_2, shared) %>%
  collect()

max_depth <- tbl(IO$glottolog, "languoids") %>%
  filter(!bookkeeping) %>%
  summarise(max_depth = max(depth, na.rm = TRUE)) %>%
  collect() %>%
  pluck("max_depth")

lang_dists_by_country <- languages_respno %>%
  filter(variable %in% c("interview", "respondent")) %>%
  filter(!is.na(glottocode)) %>%
  count(round, variable, country, glottocode, wt = withinwt) %>%
  group_by(round, variable, country) %>%
  mutate(p = n / sum(n),
         to = list(tibble(glottocode_to = glottocode, p_to = p))) %>%
  unnest(to)  %>%
  group_by(round, variable, country) %>%
  filter(glottocode != glottocode_to) %>%
  left_join(glottolog_dists,
              by = c(glottocode = "glottocode",
                     glottocode_to = "glottocode_to")) %>%
  mutate(shared = if_else(is.na(shared), 0L, shared))

elf_by_country <-
  lang_dists_by_country %>%
  mutate(dist1 = 1 - (shared / max_depth) ^ 0.5,
         dist2 = 1 - (shared / max_depth) ^ 0.05) %>%
  group_by(round, variable, country) %>%
  summarise(elf_50 = sum(exp(log(p) + log(p_to) + log(dist1))),
            elf_05 = sum(p * p_to * dist2))

elf_by_country_2 <- lang_dists_by_country %>%
  mutate(d = list(seq_len(max_depth))) %>%
  unnest() %>%
  ungroup() %>%
  mutate(dist = p * p_to * (shared < d)) %>%
  group_by(round, variable, country, d) %>%
  summarise(dist = sum(dist)) %>%
  mutate(d = sprintf("elf_d%02d", d)) %>%
  spread(d, dist)

elf_by_country <- left_join(elf_by_country, elf_by_country_2,
                            by = c("round", "variable", "country"))

