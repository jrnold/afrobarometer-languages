source("src/init.R")

wals <- IO$wals %>%
  select(wals_code, iso_code)

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
  select(iso_code = id, path, family, ancestors) %>%
  mutate(ancestors = str_split(ancestors, " "))


iso_wals <- inner_join(ethnologue, wals, by = "iso_code") %>%
  select(family, iso_code, wals_code, ancestors)

wals_ethnologue_dist <-
  inner_join(iso_wals, iso_wals, by = "family") %>%
  filter(iso_code.x != iso_code.y) %>%
  # I can use setdiff to get patrial distance since the nodes are uniquely ID'd
  mutate(distance = map2_int(ancestors.x, ancestors.y,
                             ~ length(setdiff(.x, .y)))) %>%
  select(wals_code_from = wals_code.x,
         wals_code_to = wals_code.y,
         distance)

write_csv("data/wals_ethnologue_distance.csv")
