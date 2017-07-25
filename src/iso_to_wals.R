#' ---
#' title: Generate ISO to WALS Mappings
#' ----
#'
#' Map all ISO 639-3 languages to the the closest WALS language(s) using the Ethnologue language hierarchy.
#'

library("tidyverse")
library("rprojroot")
library("magrittr")
library("jsonlite")
library("stringr")
library("igraph")

OUTPUT <- find_rstudio_root_file("data", "iso_to_wals.csv")

INPUTS <- list(
  wals = list("external", "wals", "language.csv"),
  wals_update = list("data-raw", "wals-updates.csv"),
  ethnologue_tree = list("data-raw", "ethnologue-tree.json")
) %>%
{setNames(map(., function(x) invoke(find_rstudio_root_file, x)),
          names(.))}

wals_updates <-
  read_csv(INPUTS[["wals_update"]],
           col_types =
             cols_only(
               wals_code = col_character(),
               iso_code = col_character()
             ))

wals <- read_csv(INPUTS[["wals"]],
                 col_types = cols(
                   .default = col_character(),
                   latitude = col_double(),
                   longitude = col_double()
                 )) %>%
  select(wals_code, iso_code) %>%
  left_join(select(wals_updates,
                   wals_code,
                   iso_code_new = iso_code),
            by = "wals_code") %>%
  mutate(iso_code = coalesce(iso_code_new, iso_code)) %>%
  select(-iso_code_new)

get_edges <- function(x, name) {
  subgroup_edges <- if (length(x[["subgroups"]])) {
    map(names(x[["subgroups"]]), ~ tibble(from = name, to = .x))
  } else {
    NULL
  }
  lang_edges <- if (length(x[["languages"]])) {
    map(names(x[["languages"]]), ~ tibble(from = name, to = .x))
  } else {
    NULL
  }
  if (length(x[["subgroups"]])) {
    lower_edges <- map2_df(x[["subgroups"]],
                           names(x[["subgroups"]]),
                           get_edges)
  } else {
    lower_edges <- NULL
  }
  bind_rows(subgroup_edges, lang_edges, lower_edges)
}

ethnologue_tree <- fromJSON(INPUTS$ethnologue_tree) %>%
   {map2(., names(.), get_edges)}

make_distances <- function(x) {
  g <- graph_from_data_frame(x)
  as_tibble(rownames_to_column(as.data.frame(distances(g)), "from")) %>%
    gather(to, distance, -from) %>%
    dplyr::filter(str_detect(to, "^/language"),
                  str_detect(from, "^/language")) %>%
    dplyr::mutate(to = str_replace(to, "^/language/(.*)/\\d+$", "\\1"),
                  from = str_replace(from, "^/language/(.*)/\\d+$", "\\1"))
}

#' ISO to WALS
#'
#' For all ISO 639-3 languages in Ethnologue find the closest ISO-639-3
#' language(s) associated with a WALS language (within the Ethnologue family).
#'
iso_to_wals <-
  map_df(ethnologue_tree, make_distances) %>%
  inner_join(select(wals, wals_code, iso_code),
            by = c("to" = "iso_code")) %>%
  rename(iso_code = from, iso_code_to = to) %>%
  group_by(iso_code) %>%
  filter(distance == min(distance)) %>%
  select(iso_code, wals_code, iso_code_to, distance) %>%
  arrange(iso_code, wals_code)

#' Write output
write_csv(iso_to_wals, path = OUTPUT, na = "")
