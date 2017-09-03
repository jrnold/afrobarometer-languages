source("src/R/init.R")

# earth radius in m
EARTH_RADIUS <- 6378137
# circumference / 2 = 2 pi r / 2 = pi r
ANTIPODE_DIST <- base::pi * EARTH_RADIUS

glottolog <- IO$glottolog %>%
  mutate(glottocode,
         ancestors = str_split(ancestors, " "),
         descendants = str_split(descendants, " "))

ab_to_glottolog <- IO$afrobarometer_to_glottolog %>%
  select(round, country, variable, lang_id, country, glottocode, iso_alpha2) %>%
  filter(!is.na(glottocode)) %>%
  left_join(select(glottolog, glottocode, ancestors, descendants, level, depth), by = "glottocode")

MAX_DEPTH <- max(glottolog$depth)

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
  select(-descendants) %>%
  left_join(select(glottolog, glottocode, latitude, longitude), by = "glottocode")

# # Expand to leaves
# ab_to_glottolog2 <-
#   ab_to_glottolog %>%
#   # expand descendants
#   left_join(select(glottolog, glottocode, descendants), by = "glottocode") %>%
#   unnest(descendants) %>%
#   select(-glottocode) %>%
#   rename(glottocode = descendants) %>%
#   left_join(select(glottolog, glottocode, ancestors, descendants), by = "glottocode") %>%
#   filter(map_lgl(descendants, ~ is.null(.x) || all(is.na(.x))))

round_ <- 6
distances <-
  inner_join(ab_to_glottolog,
            ab_to_glottolog,
            by = "iso_alpha2",
            suffix = c("", "_to")) %>%
  # remove self matches
  filter(!(variable == variable_to & lang_id == lang_id_to &
             country == country_to)) %>%
  mutate(shared = map2_int(ancestors, ancestors_to, ~ length(base::intersect(.x, .y)))) %>%
  select(-ancestors, -glottocode, -ancestors_to, -glottocode_to) %>%
  mutate(dist_clade_1 = 1 - (shared / MAX_DEPTH) ^ 0.5,
         dist_clade_2 = 1 - (shared / MAX_DEPTH) ^ 0.05,
         dist_clade_3 = 1 - (shared / MAX_DEPTH),
         dist_clade_4 = 1 - shared / depth,
         mdist_geo = distGeo(cbind(longitude, latitude), cbind(longitude_to, latitude_to))) %>%
  select(-matches("longitude|latitude"))

