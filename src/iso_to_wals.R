#' ---
#' title: Generate ISO to WALS Mappings
#' ----
#'
#' Map all ISO 639-3 languages to the the closest WALS language(s) using the Ethnologue language hierarchy.
#'
source("src/init.R")

OUTPUT <- find_rstudio_root_file("data", "iso_to_wals.csv")

make_distances <- function(x) {
  g <- igraph::graph_from_data_frame(x)
  as_tibble(rownames_to_column(as.data.frame(igraph::distances(g)), "from")) %>%
    gather(to, distance, -from) %>%
    # Keep only languages
    dplyr::filter(str_detect(to, "^/language"),
                  str_detect(from, "^/language")) %>%
    # extract the ISO 639-3 code from path
    mutate(to = str_replace(to, "^/language/(.*)/\\d+$", "\\1"),
                  from = str_replace(from, "^/language/(.*)/\\d+$", "\\1"))
}

#' ISO to WALS
#'
#' For all ISO 639-3 languages in Ethnologue find the closest ISO-639-3
#' language(s) associated with a WALS language (within the Ethnologue family).
#'
iso_to_wals <-
  map_df(IO$ethnologue_tree, make_distances) %>%
  inner_join(select(IO$wals, wals_code, iso_code),
            by = c("to" = "iso_code")) %>%
  rename(iso_code = from, iso_code_to = to) %>%
  group_by(iso_code) %>%
  filter(distance == min(distance)) %>%
  select(iso_code, wals_code, iso_code_to, distance) %>%
  arrange(iso_code, wals_code)

#' Write output
write_csv(iso_to_wals, path = OUTPUT, na = "")
