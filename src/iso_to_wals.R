#' ---
#' title: Generate ISO to WALS Mappings
#' ----
#'
#' Map all ISO 639-3 languages to the the closest WALS language(s) using the Ethnologue language hierarchy.
#'
source("src/init.R")

OUTPUT <- find_rstudio_root_file("data", "iso_to_wals.csv")

# Initial WALS to ISO_codes
wals_to_iso <-
  select(IO$wals, wals_code, iso_code) %>%
  # expand any iso_macrolanguaes
  left_join(IO$iso_639_3_macrolanguages, by = c("iso_code" = "M_Id")) %>%
  mutate(iso_code = coalesce(I_Id, iso_code)) %>%
  select(-I_Id, -I_Status)

# Add self-distances to ethnologue_distances
ethnologue_distances <- IO$ethnologue_distances

ethnologue_distances <- bind_rows(
    ethnologue_distances,
    select(ethnologue_distances, from) %>%
      distinct() %>%
      mutate(to = from, distance = 0L)
  )

#' ISO to WALS
#'
#' For all ISO 639-3 languages in Ethnologue find the closest ISO-639-3
#' language(s) associated with a WALS language (within the Ethnologue family).
#'
iso_to_wals <-
  ethnologue_distances %>%
  inner_join(wals_to_iso, by = c("to" = "iso_code")) %>%
  rename(iso_code = from, iso_code_to = to) %>%
  group_by(iso_code) %>%
  filter(distance == min(distance)) %>%
  select(iso_code, wals_code, iso_code_to, distance) %>%
  arrange(iso_code, wals_code)

#' Write output
write_csv(iso_to_wals, path = OUTPUT, na = "")
