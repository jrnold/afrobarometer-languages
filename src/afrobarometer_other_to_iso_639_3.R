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
  {
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
  select(round, question, country, value, iso_639_3, iso_scope,
         iso_ref_name, iso_alpha2)



#'
#' # Test Output Data
#'
with(afrobarometer_other_to_iso, {
  assert_that(all(!is.na(round)))
  assert_that(is.character(round))
  assert_that(all(unique(round) %in% misc_data$afrobarometer$rounds))

  assert_that(is.character(question))
  assert_that(all(!is.na(question)))

  assert_that(all(!is.na(country)))
  assert_that(is.integer(country))
  assert_that(all(country >= 1 & country <= 36))

  assert_that(all(!is.na(value)))
  assert_that(is.character(value))

  # Iso Code
  assert_that(is.character(iso_639_3))
  assert_that(all(str_detect(na.omit(iso_639_3), misc_data$iso$code_pattern)))

  assert_that(is.character(iso_ref_name))

  assert_that(is.character(iso_scope))

})

assert_that(nrow(filter(afrobarometer_other_to_iso,
                        !is.na(iso_scope),
                        !iso_scope %in% misc_data$iso$scopes$values)) == 0)

#' All ISO 639-3 codes must be valid
iso_lang_nonmatches <-
  afrobarometer_other_to_iso %>%
  filter(!is.na(iso_639_3)) %>%
  anti_join(iso_langs, by = c("iso_639_3" = "Id"))
stopifnot(nrow(iso_lang_nonmatches) == 0)

#' known_iso_country_nonmatches <-
#'   misc_data$iso$country_exceptions$values %>%
#'   map_df(as_tibble)


#' Check that the Afrobarometer countries in which
#' the language is spoken is consistent with countries
#' in which the Ethnologue records the language as being spoken.
ethnologue_langidx <- IO$ethnologue %>%
    select(iso_639_3 = LangID, iso_alpha2 = CountryID) %>%
    distinct()

known_country_nonmatches <-
  misc_data$iso$other_country_nonmatches$values %>%
  map_df(as_tibble)

iso_country_non_matches <-
  afrobarometer_other_to_iso %>%
  # ignore macrolangs
  filter(iso_scope %in% c("I")) %>%
  # remove any known non-matche
  anti_join(known_country_nonmatches,
           by = c("round", "question", "country", "value")) %>%
  # find any non-matches
  anti_join(ethnologue_langidx, by = c("iso_639_3", "iso_alpha2"))

#' stopifnot(nrow(iso_country_non_matches) == 0)
#'
#' # This generates YAML to add to misc_data exceptions
#' # iso_country_non_matches %>%
#' #   select(round, question, lang_id, lang_name, iso_639_3) %>%
#' #   as.yaml(column.major = FALSE) %>% cat()
#'
#' # Write Output
afrobarometer_other_to_iso %>%
  write_csv(path = OUTPUT, na = "")
