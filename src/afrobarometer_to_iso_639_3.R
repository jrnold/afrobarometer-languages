#' ---
#' title: "Match Afrobarometer Languages to ISO 639-3 Language Codes"
#' ---
#'
#' Match regular Afrobarometer languages to the ISO 639-3
#' standards. This uses the mappings in data-raw/afrobarometer-mappings/*.yml from the Afrobarometer languages to ISO 639-3 individual and then adds ISO 639-3 macrolanguages.
#'
source("src/init.R")

OUTPUT <- project_path("data", "afrobarometer_to_iso_639_3.csv")

# Read Afrobarometer Languages
afrobarometer_langs <- IO$afrobarometer_langs %>%
  rename(lang_id = value, lang_name = name)

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
afrobarometer_to_iso <- IO$afrobarometer_mappings %>%
    map_df(function(.x) {
      if (!is.null(.x[["iso_639_3"]])) {
        out <- tidyr::crossing(question = .x[["question"]],

                               iso_639_3 = .x[["iso_639_3"]])
        out[["lang_id"]] <- .x$lang_id
        out[["round"]] <- .x$round
        out
      }
    }) %>%
  {bind_rows(.,
     inner_join(., iso_macrolangs,
                by = c("iso_639_3" = "I_Id")) %>%
      select(-iso_639_3) %>%
      rename(iso_639_3 = M_Id))} %>%
  # there may be duplicates
  distinct() %>%
  # Add valid countries
  left_join(select(IO$afrobarometer_to_iso_639_3_countries,
                   round, question, lang_id, iso_639_3, countries),
            by = c("round", "question", "lang_id", "iso_639_3"))

#' add ISO information
afrobarometer_to_iso %<>%
  left_join(select(iso_langs,
                   iso_639_3 = Id,
                   iso_scope = Scope,
                   iso_ref_name = Ref_Name,
                   ),
            by = "iso_639_3")

#' Add Additional info from Afrobarometer
afrobarometer_to_iso %<>%
  right_join(select(afrobarometer_langs, round, question, lang_id, lang_name),
            by = c("round", "question", "lang_id")) %>%
  select(round, question, lang_id, lang_name,
         iso_639_3, iso_ref_name, iso_scope, countries) %>%
  arrange(round, question, lang_id, iso_639_3)



#'
#' # Test Output Data
#'
with(afrobarometer_to_iso, {
  assert_that(all(!is.na(round)))
  assert_that(is.character(round))
  assert_that(seteq(unique(round), misc_data$afrobarometer$rounds))

  assert_that(is.character(question))
  assert_that(all(!is.na(question)))

  # Lang Id
  assert_that(is.integer(lang_id))
  assert_that(all(lang_id >= -1 & lang_id <= 9999))

  # Lang Name
  assert_that(all(!is.na(lang_name)))
  assert_that(is.character(lang_name))

  # Iso Code
  assert_that(all(!is.na(iso_639_3)))
  assert_that(is.character(iso_639_3))
  assert_that(all(str_detect(iso_639_3, misc_data$iso$code_pattern)))

  assert_that(all(!is.na(iso_ref_name)))
  assert_that(is.character(iso_ref_name))

  assert_that(all(!is.na(iso_scope)))
  assert_that(is.character(iso_scope))
  assert_that(all(iso_scope %in% misc_data$iso$scopes$values))
})

known_iso_country_nonmatches <-
  misc_data$iso$country_exceptions$values %>%
  map_df(as_tibble)

afrobarometer_lang_nonmatches <-
  anti_join(afrobarometer_langs, afrobarometer_to_iso,
            by = c("question", "lang_id"))
stopifnot(nrow(afrobarometer_lang_nonmatches) == 0)

#' There should be no missing ISO codes.
#' Use the special codes: `und`, `mis`, or `mul` instead.
stopifnot(all(!is.na(afrobarometer_to_iso$iso_639_3)))

#' All ISO 639-3 codes must be valid
iso_lang_nonmatches <-
  afrobarometer_to_iso %>%
  anti_join(iso_langs, by = c("iso_639_3" = "Id"))
stopifnot(nrow(iso_lang_nonmatches) == 0)

#' Check that the Afrobarometer countries in which
#' the language is spoken is consistent with countries
#' in which the Ethnologue records the language as being spoken.
ethnologue_langidx <- IO$ethnologue %>%
    select(LangID, CountryID) %>%
    group_by(LangID) %>%
    summarise(countries = list(sort(unique(CountryID))))

known_iso_country_nonmatches <-
  misc_data$iso$country_exceptions$values %>%
  map_df(as_tibble)

iso_country_non_matches <-
  select(afrobarometer_to_iso, -countries) %>%
  # ignore macrolangs
  filter(iso_scope %in% c("I")) %>%
  # remove any known non-matche
  anti_join(known_iso_country_nonmatches,
            by = c("round", "question", "lang_id", "iso_639_3")) %>%
  inner_join(select(mutate(filter(afrobarometer_langs,
                                  !is.na(countries)),
                          countries = str_split(countries, " +")),
                   round, question, lang_id, countries),
            by = c("round", "question", "lang_id")) %>%
  inner_join(rename(ethnologue_langidx, ethnologue_countries = countries),
             by = c(iso_639_3 = "LangID")) %>%
  mutate(country_overlap =
           map2_lgl(countries, ethnologue_countries, ~ any(.x %in% .y))) %>%
  filter(!country_overlap) %>%
  mutate(countries = map_chr(countries, paste, collapse = " "),
         ethnologue_countries = map_chr(ethnologue_countries, paste,
                                        collapse = " "))
stopifnot(nrow(iso_country_non_matches) == 0)


# This generates YAML to add to misc_data exceptions
# iso_country_non_matches %>%
#   select(round, question, lang_id, lang_name, iso_639_3) %>%
#   yaml::as.yaml(column.major = FALSE) %>% cat()

#' # Write Output
afrobarometer_to_iso %>%
  write_csv(path = OUTPUT, na = "")
