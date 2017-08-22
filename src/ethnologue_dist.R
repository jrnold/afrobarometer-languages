# Phylogenetic distance between all ISO codes
source("src/init.R")

OUTPUT <- "data-raw/ethnologue/iso_639_3_distance.csv.gz"

ethnologue <- read_csv(gzfile("data-raw/ethnologue/ethnologue.csv.gz"),
                       na = "",
                       col_types =cols(
                         path = col_character(),
                         id = col_character(),
                         name = col_character(),
                         is_language = col_logical(),
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
  mutate(ancestors = str_split(ancestors, " "))
  # Also include ISO 639-3 Languages

macrolangs <-
  IO$iso_639_3_macrolanguages %>%
  select(M_Id, iso_code = I_Id) %>%
  left_join(ethnologue, by = "iso_code")

all_langs <- bind_rows(ethnologue, macrolangs)

family_dist <- function(family, outfile) {
  famdata <- filter(all_langs, family == UQ(family))
  dist <- inner_join(famdata, famdata, by = "family") %>%
    filter(iso_code.x != iso_code.y) %>%
    # I can use setdiff to get patrial distance since the nodes are uniquely ID'd
    mutate(distance = map2_int(ancestors.x, ancestors.y,
                               ~ length(setdiff(.x, .y)))) %>%
    mutate(iso_639_3_from = coalesce(M_Id.x, iso_code.x),
           iso_639_3_to = coalesce(M_Id.y, iso_code.y)) %>%
    group_by(iso_639_3_from, iso_639_3_to) %>%
    summarise(distance = min(distance)) %>%
    write_csv(outfile, na = "")
}

run <- function() {
  outfile <- gzfile(OUTPUT, "w")
  on.exit(close(outfile))
  walk(unique(ethnologue$family), family_dist, outfile = outfile)

}

