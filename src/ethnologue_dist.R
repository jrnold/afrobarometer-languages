source("src/init.R")

ethnologue <- read_csv(gzfile("data-raw/ethnologue/ethnologue.csv.gz"),
                       na = "",
                       col_types =cols(
                         path = col_character(),
                         id = col_character(),
                         name = col_character(),
                         is_language = col_integer(),
                         country = col_character(),
                         depth = col_integer(),
                         family = col_character(),
                         parent = col_character(),
                         ancestors = col_character(),
                         children = col_character(),
                         descendants = col_character()
                       )) %>%
  filter(is_language) %>%
  select(iso_code = id, family, ancestors) %>%
  mutate(ancestors = str_split(ancestors, " ")) %>%

iso_dist <-
  inner_join(ethnologue, ethnologue, by = "family") %>%
  filter(iso_code.x != iso_code.y) %>%
  # I can use setdiff to get patrial distance since the nodes are uniquely ID'd
  mutate(distance = map2_int(ancestors.x, ancestors.y,
                             ~ length(setdiff(.x, .y)))) %>%
  select(iso_639_3_from = iso_code.x,
         iso_639_3_to = iso_code.y,
         distance)

write_csv(gzfile("data/iso_639_3_distance.csv.gz", "w"), na = "")
