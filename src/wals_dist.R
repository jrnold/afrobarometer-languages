#' ---
#' title: Generate ISO to WALS Mappings
#' ----
#'
#' Map all ISO 639-3 languages to the the closest WALS language(s) using the Ethnologue language hierarchy.
#'
source("src/init.R")

OUTPUT <- find_rstudio_root_file("data", "wals_dist.csv")

ethnologue_language_codes <- IO$ethnologue_language_codes

wals_to_iso <-
  select(IO$wals, wals_code, iso_code) %>%
  # expand any iso_macrolanguaes
  left_join(IO$iso_639_3_macrolanguages, by = c("iso_code" = "M_Id")) %>%
  mutate(iso_code = coalesce(I_Id, iso_code)) %>%
  select(-I_Id, -I_Status)

wals_dist <-
  IO$ethnologue_distances %>%
  inner_join(select(wals_to_iso, from = iso_code, wals_code),
             by = "from") %>%
  select(-from) %>%
  rename(from = wals_code) %>%
  inner_join(select(wals_to_iso, to = iso_code, wals_code),
             by = "to") %>%
  select(-to) %>%
  rename(to = wals_code) %>%
  group_by(from, to) %>%
  summarise(distance = min(distance)) %>%
  arrange(from, distance, to)

#' Write output
write_csv(wals_dist, path = OUTPUT, na = "")
