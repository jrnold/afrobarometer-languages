#' ---
#' title: "Match Afrobarometer Languages to ISO 639-3 Language Codes"
#' ---
#'
#' Match regular Afrobarometer languages to the ISO 639-3
#' standards. This uses the mappings in data-raw/afrobarometer-mappings/*.yml from the Afrobarometer languages to ISO 639-3 individual and then adds ISO 639-3 macrolanguages.
#'
source("src/init.R")

OUTPUT <- project_path("data", "afrobarometer_other_to_iso_639_3.csv")

# Read Afrobarometer Languages
afrobarometer_langs_other <- IO$afrobarometer_langs_other %>%
  # add variable to merge on
  mutate(lang_name = str_to_lower(value))

# Misc datan
misc_data <- IO$misc_data

#' Load ISO languages, keeping only living languages
#' and special codes
iso_langs <- IO$iso_639_3_codes %>%
    filter(Language_Type %in% c("L", "S")) %>%
    select(-Language_Type, -Comment)

#' ISO Macrolanguages
iso_macrolangs <- IO$iso_639_3_macrolanguages %>%
  filter(I_Status == "A") %>%
  select(-I_Status)

#' Afrobarometer Mappings
afrobarometer_other_to_iso <- IO$afrobarometer_other_mappings %>%
  map_df(function(.x) {
    if (!is.null(.x[["iso_639_3"]])) {
      out <- tidyr::crossing(country = .x[["country"]],

                             iso_639_3 = .x[["iso_639_3"]])
      out[["lang_name"]] <- .x$lang_name
      out
    }
  }) %>%
  distinct() %>%
  { # Exclude Linting
    bind_rows(.,
              inner_join(., iso_macrolangs,
                        by = c("iso_639_3" = "I_Id")) %>%
              select(-iso_639_3) %>%
              rename(iso_639_3 = M_Id))
  } %>%
  # there may be duplicates
  distinct()


#' add ISO information
afrobarometer_other_to_iso %<>%
  left_join(select(iso_langs,
                   iso_639_3 = Id,
                   iso_scope = Scope,
                   iso_ref_name = Ref_Name,
                   ),
            by = "iso_639_3")

#' Add Additional info from Afrobarometer
afrobarometer_other_to_iso <-
  left_join(afrobarometer_langs_other,
            afrobarometer_other_to_iso,
            by = c("iso_alpha2" = "country", "lang_name")) %>%
  select(round, variable, country, value, iso_639_3, iso_scope,
         iso_ref_name, iso_alpha2)


#'
#' # Test Output Data
#'

# Primary Key
assert_that(
  nrow(distinct(afrobarometer_other_to_iso, round, variable,
                country, value, iso_639_3)) ==
    nrow(afrobarometer_other_to_iso)
)

with(afrobarometer_other_to_iso, {
  assert_that(all(!is.na(round)))
  assert_that(is_integerish(round))
  assert_that(all(unique(round) %in% misc_data$afrobarometer$rounds))

  assert_that(is.character(variable))
  assert_that(all(!is.na(variable)))

  assert_that(all(!is.na(country)))
  assert_that(is.integer(country))
  assert_that(all(country >= 1 & country <= 36))

  assert_that(all(!is.na(value)))
  assert_that(is.character(value))

  # Iso Code
  assert_that(all(!is.na(iso_639_3)))
  assert_that(is.character(iso_639_3))
  assert_that(all(str_detect(iso_639_3, misc_data$iso$code_pattern)))

  assert_that(is.character(iso_ref_name))

  assert_that(is.character(iso_scope))

})

#' Check that ISO Scopes are valid
assert_that(nrow(filter(afrobarometer_other_to_iso,
                        !is.na(iso_scope),
                        !iso_scope %in% misc_data$iso$scopes$values)) == 0)

#' All ISO 639-3 codes must be valid
iso_lang_nonmatches <-
  afrobarometer_other_to_iso %>%
  filter(!is.na(iso_639_3)) %>%
  anti_join(iso_langs, by = c("iso_639_3" = "Id"))
stopifnot(nrow(iso_lang_nonmatches) == 0)

iso_country_non_matches <-
  afrobarometer_other_to_iso %>%
  filter(!iso_scope %in% c("S")) %>%
  # find any non-matches
  anti_join(IO$iso_639_3_countries,
            by = c("iso_639_3" = "LangID", "iso_alpha2" = "CountryID"))

if (nrow(iso_country_non_matches)) {
  print(iso_country_non_matches)
  stop("Some ISO languages appear in invalid countries")
}

#' #' If multiple matches check that they are relatively similar
#' distant_matches <-
#'   afrobarometer_other_to_iso %>%
#'   # ignore macro-languages since they aren't in the ethnologue dist
#'   filter(iso_scope == "I") %>%
#'   # distinct country/value combos
#'   select(iso_alpha2, value, iso_639_3) %>%
#'   distinct() %>%
#'   # ignore known bad cases
#'   group_by(iso_alpha2, value) %>%
#'   do(as_tibble(tidyr::crossing(from = .$iso_639_3, to = .$iso_639_3))) %>%
#'   # filter self matches
#'   filter(from != to) %>%
#'   # left join to ensure non-matches will still be present
#'   left_join(IO$ethnologue_distances, by = c("from", "to")) %>%
#'   # set an arbitrarily large distance for non-family matches
#'   mutate(distance = if_else(is.na(distance), 1000L, distance)) %>%
#'   group_by(from, value) %>%
#'   summarise(distance = max(distance)) %>%
#'   arrange(desc(distance), value) %>%
#'   # Most matches are 2 and below
#'   filter(distance > 2)
#'
#' if (nrow(distant_matches) > 0) {
#'   print(distant_matches)
#'   stop("Found linguistically dissimilar matches")
#' }

#' # Write Output
afrobarometer_other_to_iso %>%
  write_csv(path = OUTPUT, na = "")
