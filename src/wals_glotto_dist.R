library("geosphere")
source("src/init.R")

wals <- IO$wals

# distances between WALS languages using the Glottolog tree
wals2glotto <- select(IO$wals, wals_code, glottocode) %>%
  filter(!is.na(glottocode)) %>%
  inner_join(select(IO$glottolog, glottocode, depth, ancestors, family),
             by = "glottocode") %>%
  mutate(ancestors = str_split(ancestors, " "))

glotto_genetic_dist <-
  wals2glotto %>%
  crossing(., select(wals2glotto, wals_code_to = wals_code,
                     ancestors_to = ancestors)) %>%
  filter(wals_code != wals_code_to) %>%
  mutate(distance = map2_int(ancestors, ancestors_to,
                             ~ length(setdiff(.x, .y)))) %>%
  mutate(distance = distance / depth) %>%
  select(wals_code, wals_code_to, distance) %>%
  group_by(wals_code, wals_code_to) %>%
  summarise(genetic_glotto = min(distance))

EARTH_RADIUS <- 6378137
# 1/2 circumference
ANTIPODAL_DIST <- base::pi * EARTH_RADIUS ^ 2

X <- select(wals, longitude, latitude) %>% as.matrix
D <- distm(X, X, fun = distGeo)
colnames(D) <- wals$wals_code
wals_geo_dist <- as_tibble(D) %>%
  mutate(wals_code = UQ(wals$wals_code)) %>%
  gather(wals_code_to, distance, -wals_code) %>%
  mutate(distance = distance / ANTIPODAL_DIST) %>%
  select(wals_code, wals_code_to, geo = distance)

wals_genetic_dist <- IO$wals %>%
  select(wals_code, family, genus) %>%
  crossing(., .) %>%
  filter(wals_code != wals_code1) %>%
  mutate(distance = ((family != family1) + (genus != genus1)) / 3) %>%
  select(wals_code, wals_code_to = wals_code1, genetic_wals = distance)

wals_distances <-
  left_join(wals_genetic_dist, wals_geo_dist,
          by = c("wals_code", "wals_code_to")) %>%
  left_join(glotto_genetic_dist,
            by = c("wals_code", "wals_code_to")) %>%
  # replace missing with 1
  mutate(genetic_glotto = coalesce(genetic_glotto, 1)) %>%
  # weights give equal weight to "genetic" and
  mutate(distance = (genetic_wals + genetic_glotto + geo) / 3)

wals <- read_rds("../language_lgbt/data/wals.rds")

dists <- select(wals_distances, wals_code, wals_code_to, distance) %>%
  filter(!is.na(distance))

impute_feature <- function(feature_id, dists, lang_features, n = 1) {
  non_miss <- filter(lang_features, feature_id == UQ(feature_id)) %>%
    select(wals_code, value)

  imputed <-
    dists %>%
    # anti_join(non_miss, by = "wals_code") %>%
    inner_join(non_miss, by = c("wals_code_to" = "wals_code")) %>%
    filter(wals_code_to != wals_code) %>%
    group_by(wals_code) %>%
    # filter(rank(distance, ties.method = "random") <= 1) %>%
    filter(rank(distance, ties.method = "first") <= n) %>%
    count(wals_code, value) %>%
    group_by(wals_code) %>%
    filter(rank(n, ties.method = "random") <= 1) %>%
    select(wals_code, imputed = value)

  #bind_rows(non_miss, imputed) %>%
  left_join(non_miss, imputed, by = "wals_code") %>%
    mutate(feature_id = UQ(feature_id))
}

GENDER_FEATURES <- c("30A", "31A", "32A")

lang_feats <- wals$language_features %>%
  filter(feature_id %in% GENDER_FEATURES) %>%
  mutate(value = if_else(feature_id == "30A", pmin(value, 3L), value)) %>%
  bind_rows(filter(wals$language_features, feature_id == "44A") %>%
              mutate(value = as.integer(value %in% c(1, 4)),
                     feature_id = "44A1")) %>%
  bind_rows(filter(wals$language_features, feature_id == "44A") %>%
              mutate(value = as.integer(value %in% c(1, 2, 3, 5)),
                     feature_id = "44A3"))

wals_gender_features <-
  map_df(c(GENDER_FEATURES, "44A1", "44A3"), impute_feature, dists = dists,
    lang_features = lang_feats, n = 1)

IO$afrobarometer_to_wals %>%
  select(round, variable, lang_id, country, wals_code) %>%
  left_join(wals_gender_features, by = "wals_code") %>%
  count(feature_id, round, variable, lang_id, country, value) %>%
  group_by(feature_id, round, variable, lang_id, country) %>%
  filter(rank(desc(n), ties.method = "random") <= 1) %>%
  select(-n)

IO$afrobarometer_other_to_wals %>%
  filter(!is.na(wals_code)) %>%
  select(round, variable,country, lang_value = value, wals_code) %>%
  left_join(wals_gender_features, by = "wals_code") %>%
  count(feature_id, round, variable, country, lang_value, value) %>%
  group_by(feature_id, round, variable, country, lang_value) %>%
  filter(rank(desc(n), ties.method = "random") <= 1) %>%
  select(-n)
