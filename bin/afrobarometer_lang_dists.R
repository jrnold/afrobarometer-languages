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
  left_join(select(glottolog, glottocode, ancestors, descendants, level, depth),
            by = "glottocode")

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
  select(glottocode, round, country, variable, lang_id, depth, ancestors) %>%
  left_join(select(glottolog, glottocode, latitude, longitude),
            by = "glottocode")

distances <-
  crossing(ab_to_glottolog,
           rename_all(ab_to_glottolog, str_c, "_to")) %>%
  # remove self matches
  filter(!(variable == variable_to & lang_id == lang_id_to &
             country == country_to)) %>%
  mutate(shared = map2_int(ancestors, ancestors_to,
                           ~ length(base::intersect(.x, .y))),
         dist_geo = geosphere::distGeo(cbind(longitude, latitude),
                                       cbind(longitude_to, latitude_to))) %>%
  select(-ancestors, -glottocode, -ancestors_to, -glottocode_to,
         -depth_to, -matches("longitude|latitude")) %>%
  group_by(round, country, variable, lang_id,
           round_to, country_to, variable_to, lang_id_to) %>%
  # Using the median ensures that depth and shared are still integers
  summarise_at(vars(shared, depth, dist_geo), funs(median))

OUTFILE <- "data/afraborometer_lang_dists.csv.gz"

hdl <- gzfile(OUTFILE, "w")
write_csv(distances, hdl, na = "")
close(hdl)

with_conn <- function(conn, expr) {
  force(conn)
  on.exit(close(conn))
  eval_tidy(enquo(expr), env = env_parent(), data = list(conn = conn))
}
