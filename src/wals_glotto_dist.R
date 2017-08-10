wals2glotto <- select(IO$wals, wals_code, glottocode) %>%
  filter(!is.na(glottocode)) %>%
  inner_join(select(languoids, glottocode, depth, ancestors, family),
             by = "glottocode")

wals_glottodist <-
  wals2glotto %>%
  inner_join(select(wals2glotto, family,
                    wals_to = wals_code,
                    ancestors_to = ancestors),
             by = "family") %>%
  filter(wals_code != wals_to) %>%
  mutate(distance = map2_int(ancestors, ancestors_to, ~ length(setdiff(.x, .y)))) %>%
  select(wals_code, wals_to, distance) %>%
  group_by(wals_code, wals_to) %>%
  summarise(distance = min(distance))


