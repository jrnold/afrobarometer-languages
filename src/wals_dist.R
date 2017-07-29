#' ---
#' title: Generate ISO to WALS Mappings
#' ----
#'
#' Map all ISO 639-3 languages to the the closest WALS language(s) using the Ethnologue language hierarchy.
#'
source("src/init.R")

OUTPUT <- find_rstudio_root_file("data", "wals_dist.csv")

ethnologue_language_codes <- IO$ethnologue_language_codes

wals <- IO$wals

wals_to_iso <-
  select(wals, wals_code, iso_code) %>%
  # expand any iso_macrolanguaes
  left_join(IO$iso_639_3_macrolanguages, by = c("iso_code" = "M_Id")) %>%
  mutate(iso_code = coalesce(I_Id, iso_code)) %>%
  select(-I_Id, -I_Status)

wals_dist <-
  IO$ethnologue_distances %>%
  inner_join(select(wals_to_iso, from = iso_code, wals_code),
             by = "from") %>%
  select(-from) %>%
  #rename(iso_code_from = from) %>%
  rename(from = wals_code) %>%
  inner_join(select(wals_to_iso, to = iso_code, wals_code),
             by = "to") %>%
  select(-to) %>%
  # rename(iso_code_to = to) %>%
  rename(to = wals_code) %>%
  group_by(from, to) %>%
  summarise(distance = min(distance)) %>%
  arrange(from, distance, to)

#' Check data

#' Check primary key
assert_that(nrow(distinct(wals_dist, from, to)) == nrow(wals_dist))

#' Check columns
eval_tidy(quo({
  assert_that(all(!is.na(.data$from)))
  assert_that(is.character(.data$from))

  assert_that(all(!is.na(.data$to)))
  assert_that(is.character(.data$to))

  assert_that(all(!is.na(.data$distance)))
  assert_that(is_integerish(.data$distance))
  # 14 was the max when I checked
  assert_that(all(.data$distance > 0 & .data$distance <= 14))

}), data = wals_dist)

#' Check that all WALS codes are valid
assert_that(nrow(anti_join(wals_dist, wals,
                           by = c("from" = "wals_code"))) == 0)
#' Check that all WALS codes are valid
assert_that(nrow(anti_join(wals_dist, wals,
                           by = c("from" = "wals_code"))) == 0)

#' Check that (almost) all WALS codes appear in the data
#' there's some discrepencies between ISO codes and the ethnologue version
KNOWN_MISSING_WALS <- "huc"
assert_that(anti_join(filter(wals, !is.na(iso_code),
                 macroarea == "Africa",
                 !iso_code %in% KNOWN_MISSING_WALS),
                 wals_dist, by = c("wals_code" = "to")) %>%
              nrow() %>% equals(0))

assert_that(anti_join(filter(wals, !is.na(iso_code),
                             macroarea == "Africa",
                             !iso_code %in% KNOWN_MISSING_WALS),
                      wals_dist, by = c("wals_code" = "from")) %>%
              nrow() %>% equals(0))

#' Write output
write_csv(wals_dist, path = OUTPUT, na = "")
