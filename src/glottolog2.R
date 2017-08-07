library("jsonlite")
library("rlang")
library("tidyverse")
library("geosphere")
source("src/init.R")

glottolog_tree <- read_json("external/glottolog/tree-glottolog.json")

glottolog_edges <- function(tree) {
  # Add edges
  env <- rlang::new_environment()
  get_edges <- function(x) {
    from <- x[["glottocode"]]
    children <- map_chr(x[["children"]], get_edges)
    if (length(children)) {
      env[[from]] <- tibble(from = from, to = children)
    }
    from
  }
  # Add top level root
  env[["root0000"]] <- tibble(from = "root0000",
                              to = map_chr(glottolog_tree, "glottocode"))
  # Add other edges
  bind_rows(as_list(walk(tree, get_edges)))
}

# Add edges
env <- rlang::new_environment()
walk_glottolog <- function(x, depth = 1, ancestors = character(), family = NULL) {
  glottocode <- x[["glottocode"]]
  if (depth == 1) {
    family <- glottocode
  }
  descendants <- map(x[["children"]],
                     walk_glottolog,
                     depth = depth + 1,
                     family = family,
                     ancestors = c(glottocode, ancestors)) %>%
    flatten_chr()
  env[[glottocode]] <-
    tibble(glottocode = glottocode,
           name = x[["name"]],
           ancestors = list(ancestors),
           descendants = list(descendants),
           depth = depth,
           family = family,
           is_family = (depth == 1),
           is_leaf = (!length(x[["children"]])))
  c(glottocode, descendants)
}
# Add other edges
walk(glottolog_tree, walk_glottolog)
languoids <- as_list(env) %>% bind_rows()
rm(env)

# Add additional information
languoids <- left_join(languoids,
          select(IO$glottolog_languoids,
                 glottocode = id,
                 iso_639_3 = hid,
                 latitude, longitude,
                 level, status, bookkeeping),
          by = "glottocode")

geomean2 <- function(longitude, latitude) {
  if (length(longitude) > 1) {
    out <- as.numeric(geomean(cbind(longitude, latitude)))
    list(tibble(longitude = out[1], latitude = out[2]))
  } else {
    list(tibble(longitude = longitude, latitude = latitude))
  }
}

# calculate latitude and longitude for all
distances2 <-
  filter(languoids, is.na(latitude), is.na(longitude)) %>%
  select(glottocode, descendants) %>%
  unnest() %>%
  inner_join(select(filter(languoids,
                          !is.na(latitude), !is.na(longitude)),
                   glottocode, latitude, longitude),
            by = c(descendants = "glottocode")) %>%
  group_by(glottocode) %>%
  summarise(xy = geomean2(longitude, latitude)) %>% unnest()

languoids <-
  left_join(languoids, distances2,
          by = "glottocode") %>%
  mutate(latitude = coalesce(latitude.x, latitude.y),
         longitude = coalesce(longitude.x, longitude.y)) %>%
  select(-matches("\\.[xy]$"))


# calculate patrial distance
languoid_dists <-
  inner_join(select(languoids, from = glottocode, family,
                 ancestors_from = ancestors),
            select(languoids, to = glottocode, family,
                 ancestors_to = ancestors),
            by = "family") %>%
  filter(to != from) %>%
  mutate(distance = length(setdiff(ancestors_from, ancestors_to)),
         common = list(union(ancestors_from, ancestors_to))) %>%
  select(from, to, distance, common)


# All WALS match an ISO code,
# Not all glottocodes have a WALS
#
# 1. WALS distances using glottoces
# 2. For all glottocodes find the closest WALS language(s)
glottocode_to_wals1 <- select(IO$wals, from = glottocode, wals_code) %>%
  mutate(to = glottocode, distance = 0)

glottocode_to_wals2 <-
  select(languoids_dists, from, to, distance) %>%
  anti_join(glottocode_to_wals1, by = "from") %>%
  inner_join(select(filter(IO$wals, !is.na(glottocode)),
                    glottocode, wals_code),
             by = c(to = "glottocode")) %>%
  group_by(from) %>%
  filter(distance == min(distance))

glottocode_to_wals <- bind_rows(glottocode_to_wals1,
                                glottocode_to_wals2)

glottocode_to_iso_639 <-
  select(languoid_dists, from, to, distance) %>%
  # remove those that already have iso_639_codes
  anti_join(filter(languoids, !is.na(iso_639_3)),
            by = c(from = "glottocode")) %>%
  inner_join(select(filter(languoids, !is.na(iso_639_3)),
                    to = glottocode, iso_639_3),
             by = "to") %>%
  group_by(from) %>%
  filter(distance == min(distance)) %>%
  bind_rows(select(filter(languoids, !is.na(iso_639_3)),
                   from = glottocode, iso_639_3) %>%
              mutate(to = glottocode, distance = 0))
