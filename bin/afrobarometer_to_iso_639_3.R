#' ---
#' title: "Match Afrobarometer Languages to ISO 639-3 Language Codes"
#' ---
#'
#' Match regular Afrobarometer languages to the ISO 639-3
#' standards. This uses the mappings in data-raw/afrobarometer-mappings/*.yml from the Afrobarometer languages to ISO 639-3 individual and then adds ISO 639-3 macrolanguages.
#'
source("src/R/init.R")

OUTPUT <- project_path("data", "afrobarometer_to_iso_639_3.csv")

# Read Afrobarometer Languages
afrobarometer_langs <- IO$afrobarometer_langs

# Misc datan
misc_data <- IO$misc_data

#' Load ISO languages, keeping only living languages
#' and special codes
iso_langs <- IO$iso_639_3_codes %>%
    filter(Language_Type %in% c("L", "S")) %>%
    select(-Language_Type, -Comment)

#' Mappings for ISO 639-3
#' both for individual to macro, and macro to individual
iso_macrolangs <- IO$iso_639_3_macrolanguages %>%
  filter(I_Status == "A") %>%
  select(-I_Status) %>%
  {
    bind_rows(select(., iso_from = M_Id, iso_to = I_Id),
              select(., iso_from = I_Id, iso_to = M_Id))
  }

#' Afrobarometer Mappings
afrobarometer_to_iso <- IO$afrobarometer_mappings %>%
    map_df(function(.x) {
      # it shouldn't be empty but if it is, continue
      if (!is.null(.x[["iso_639_3"]])) {
        if (is.null(names(.x[["iso_639_3"]]))) {
          # if no names, then all countries
          out <- tidyr::crossing(variable = .x[["variables"]],
                                 iso_639_3 = .x[["iso_639_3"]])
        } else {
          iso_codes <- .x[["iso_639_3"]]
          out <- map2_df(names(iso_codes), iso_codes,
                         ~ tibble(iso_639_3 = .x, country = .y))
          out <- tidyr::crossing(out, variable = .x[["variables"]])
        }
        out[["lang_id"]] <- .x$lang_id
        out[["round"]] <- .x$round
        out
      } else {
        stop("iso_639_3 key is not found: round ", .x$round, ", variable ",
             .x$variable, " lang_id ", .x$lang_id)
      }
    }) %>%
  # there may be duplicates
  distinct() %>%
  rename(valid_country = country)

#' Expand macrolanguages or add individual languages
afrobarometer_to_iso %<>%
  {bind_rows(.,
             inner_join(., iso_macrolangs, by = c(iso_639_3  = "iso_from")) %>%
             select(-iso_639_3, iso_639_3 = iso_to)
  )} %>%
  distinct()

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
  right_join(select(afrobarometer_langs, round, variable, lang_id, lang_name,
                    country, iso_alpha2),
            by = c("round", "variable", "lang_id")) %>%
  # Filter country non-matches
  filter(is.na(valid_country) | valid_country == iso_alpha2) %>%
  select(-valid_country) %>%
  select(round, variable, lang_id, lang_name, country, iso_alpha2,
         iso_639_3, iso_ref_name, iso_scope) %>%
  arrange(round, variable, lang_id, iso_alpha2, iso_639_3)

#'
#' # Test Output Data
#'
with(afrobarometer_to_iso, {
  assert_that(all(!is.na(round)))
  assert_that(is_integerish(round))
  assert_that(seteq(unique(round), misc_data$afrobarometer$rounds))

  assert_that(is.character(variable))
  assert_that(all(!is.na(variable)))

  # Lang Id
  assert_that(is.integer(lang_id))
  assert_that(all(lang_id >= -1 & lang_id <= 9999))

  # Lang Name
  assert_that(all(!is.na(lang_name)))
  assert_that(is.character(lang_name))

  # country
  assert_that(all(!is.na(country)))
  assert_that(is.integer(country))
  assert_that(all(country >= 1 & country <= 36))

  # ISO 3166 alpha-2
  assert_that(all(!is.na(iso_alpha2)))
  assert_that(is.character(iso_alpha2))
  assert_that(all(str_detect(iso_alpha2, "^[A-Z]{2}$")))

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

# all afrobarometer langs should appear
afrobarometer_lang_nonmatches <-
  anti_join(afrobarometer_langs, afrobarometer_to_iso,
            by = c("variable", "lang_id", "country"))
stopifnot(nrow(afrobarometer_lang_nonmatches) == 0)

# check primary keys
assert_that(
  nrow(distinct(afrobarometer_to_iso, round, variable,
                lang_id, country, iso_639_3)) ==
    nrow(afrobarometer_to_iso)
)

#' All ISO 639-3 codes must be valid
iso_lang_nonmatches <-
  afrobarometer_to_iso %>%
  anti_join(iso_langs, by = c("iso_639_3" = "Id"))
stopifnot(nrow(iso_lang_nonmatches) == 0)

#' Any match should at most match at most one macrolanguage
num_macrolangs <-
  afrobarometer_to_iso %>%
  filter(iso_scope == "M") %>%
  count(round, variable, lang_id, country) %>%
  filter(n > 1)
if (nrow(num_macrolangs) > 1) {
  print(num_macrolangs)
  stop("Each language can match AT MOST one macrolanguage")
}

#' Any match must have at least one individual language
num_indiv_langs <-
  afrobarometer_to_iso %>%
  filter(iso_scope %in% c("I", "M")) %>%
  group_by(round, variable, lang_id, country) %>%
  summarise(n = sum(iso_scope == "I")) %>%
  filter(n == 0)
if (nrow(num_indiv_langs) > 1) {
  print(num_indiv_langs)
  stop("Each language can match AT LEAST one individual ISO 639-3 language")
}

#' Check that the Ethnologue indicates that the ISO language match is spoken in that country
iso_country_non_matches <-
  afrobarometer_to_iso %>%
  filter(!iso_scope %in% c("S"),
         !iso_639_3 %in% IO$misc_data$iso$common_langs$values) %>%
  anti_join(IO$iso_639_3_countries,
            by = c(iso_639_3 = "LangID", iso_alpha2 = "CountryID"))
stopifnot(nrow(iso_country_non_matches) == 0)

#' Check that all combinations of (country, language name)
#' match the same ISO codes across rounds.
consistent_mappings <-
  afrobarometer_to_iso %>%
  #' Arabic is handled differently
  filter(lang_name != "Arabic") %>%
  group_by(round, variable, lang_name, iso_alpha2) %>%
  summarise(iso_639_3 = str_c(sort(unique(iso_639_3)), collapse = " ")) %>%
  group_by(iso_alpha2, lang_name) %>%
  filter(length(unique(iso_639_3)) > 1) %>%
  arrange(lang_name, iso_alpha2)
if (nrow(consistent_mappings)) {
  print(consistent_mappings)
  stop("There are inconsistent mappings in the ISO 639-3 mappings")
}

#' # Write Output
afrobarometer_to_iso %>%
  write_csv(path = OUTPUT, na = "")
