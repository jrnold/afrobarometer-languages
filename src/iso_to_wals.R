#' ---
#' title: Generate ISO to WALS Mappings
#' ----
#'
#' Map all ISO 639-3 languages to the the closest WALS language(s) using the Ethnologue language hierarchy.
#'
source("src/init.R")

OUTPUT <- find_rstudio_root_file("data", "iso_to_wals.csv")

ethnologue_language_codes <- IO$ethnologue_language_codes

# Initial WALS to ISO_codes
wals_to_iso <-
  select(IO$wals, wals_code, iso_code) %>%
  # expand any iso_macrolanguaes
  left_join(IO$iso_639_3_macrolanguages, by = c("iso_code" = "M_Id")) %>%
  mutate(iso_code = coalesce(I_Id, iso_code)) %>%
  select(-I_Id, -I_Status)

ethnologue_distances <- IO$ethnologue_distances
# Add self-distances to ethnologue_distances
ethnologue_distances <- bind_rows(
    ethnologue_distances,
    select(ethnologue_distances, from) %>%
      distinct() %>%
      mutate(to = from, distance = 0L)
  ) %>%
  left_join(select(ethnologue_language_codes,
                   from = LangID, country_from = CountryID),
            by = "from") %>%
  left_join(select(ethnologue_language_codes,
                   to = LangID, country_to = CountryID),
            by = "to") %>%
  mutate(same_country = coalesce((country_from == country_to), FALSE)) %>%
  select(from, to, distance, same_country)


#' ISO to WALS
#'
#' For all ISO 639-3 languages in Ethnologue find the closest ISO-639-3
#' language(s) associated with a WALS language (within the Ethnologue family).
#'
iso_to_wals <-
  ethnologue_distances %>%
  inner_join(wals_to_iso, by = c("to" = "iso_code")) %>%
  rename(iso_code = from, iso_code_to = to) %>%
  # Keep closest languages on the tree
  group_by(iso_code) %>%
  filter(distance == min(distance)) %>%
  # Prefer languages that have the same country of origin
  group_by(iso_code) %>%
  filter(same_country == max(same_country)) %>%
  mutate(same_country = as.integer(same_country)) %>%
  select(iso_code, wals_code, iso_code_to, distance, same_country) %>%
  arrange(iso_code, wals_code)

#' Write output
write_csv(iso_to_wals, path = OUTPUT, na = "")
