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

#' Recently retired ISO langs
RETIRED_LANGS <-
  c("prb", "skk", "rie", "snh", "iap", "puk", "krm", "jeg", "kgd",
  "rsi")

#' ISO to WALS
#'
#' For all ISO 639-3 languages in Ethnologue find the closest ISO-639-3
#' language(s) associated with a WALS language (within the Ethnologue family).
#'
iso_to_wals <-
  ethnologue_distances %>%
  # remove retired langs
  filter(!from %in% RETIRED_LANGS) %>%
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
  # macrolang matches can create multi matches. select one of them
  group_by(iso_code, wals_code) %>%
  slice(1) %>%
  arrange(iso_code, wals_code)

#' Tests
assert_that(
  nrow(distinct(iso_to_wals, iso_code, wals_code)) ==
    nrow(iso_to_wals)
)

with(iso_to_wals, {
  assert_that(all(!is.na(iso_code)))
  assert_that(is.character(wals_code))
  assert_that(all(str_detect(wals_code, "^[a-z]{2,3}$")))

  assert_that(all(!is.na(wals_code)))
  assert_that(is.character(wals_code))
  assert_that(all(str_detect(iso_code, "^[a-z]{3}$")))

  assert_that(all(!is.na(distance)))
  assert_that(is.integer(distance))
  assert_that(all(distance >= 0))

  assert_that(all(!is.na(same_country)))
  assert_that(is.integer(same_country))
  assert_that(all(same_country %in% c(0, 1)))

})

#' Check that WALS codes are valid
assert_that(nrow(anti_join(iso_to_wals, IO$wals, by = "wals_code")) == 0)

#' Check that iso-codes are valid
invalid_iso_codes <-
  anti_join(iso_to_wals, IO$iso_639_3_codes, by = c(iso_code = "Id")) %>%
  ungroup() %>%
  select(iso_code) %>%
  distinct()
assert_that(nrow(invalid_iso_codes) == 0)

#" 1      prb retired
#' 2      skk retired
#' 3      rie retired
#' 4      snh retired
#' 5      iap retired
#' 6      puk retired
#' 7      krm retired
#' 8      jeg retired
#" 9      kgd retired
#' 10      rsi retired

#' Write output
write_csv(iso_to_wals, path = OUTPUT, na = "")
