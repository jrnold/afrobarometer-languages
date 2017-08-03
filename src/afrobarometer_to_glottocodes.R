source("src/init.R")
library("yaml")

OUTPUT <- project_path("data", "afrobarometer_to_glottolog.csv")

walk_tree <- function(x, level = 0L, env = rlang::new_environment()) {
  glottocode <- x[["glottocode"]]
  descendants <- flatten_chr(map(x[["children"]], walk_tree,
                                 level = level + 1L, env = env))
  env[[glottocode]] <- list(
    descendants = c(glottocode, descendants),
    level = level
  )
  c(glottocode, descendants)
}

glottolog_descendants <- rlang::new_environment()

glottolog_tree <-
  read_json("external/glottolog/tree-glottolog.json",
            simplifyVector = FALSE) %>%
  walk(walk_tree, level = 0, env = glottolog_descendants)

all_descendants <-
  map_df(ls(glottolog_descendants),
       function(i) {
         out <- as.tibble(glottolog_descendants[[i]])
         out[["node"]] <- i
         out
       })

glottolog_langoids <-
  read_csv("external/glottolog/languoid.csv", na = "",
          col_types = cols(
              bookkeeping = col_character(),
              child_dialect_count = col_integer(),
              child_family_count = col_integer(),
              child_language_count = col_integer(),
              description = col_character(),
              family_pk = col_integer(),
              father_pk = col_integer(),
              hid = col_character(),
              id = col_character(),
              jsondata = col_character(),
              latitude = col_double(),
              level = col_character(),
              longitude = col_double(),
              markup_description = col_character(),
              name = col_character(),
              newick = col_character(),
              pk = col_integer(),
              status = col_character()
            ))

# Glottocode Mappings
resource2df <- function(x) {
  if (length(x$identifiers)) {
    out <- map_df(x$identifiers, as_tibble)
    out$glottocode <- x$id
    out
  }
}

resourcemap <- read_json("external/glottolog/resourcemap.json",
                         simplifyVector = FALSE) %>%
  `[[`("resources") %>%
  map_df(resource2df)

#' Glottolog to ISO matches
#'
#' - ISO codes in a list
glottolog_to_iso <-
  all_descendants %>%
  inner_join(filter(resourcemap, type == "iso639-3") %>%
               select(glottocode, iso_639_3 = identifier),
             by = c("descendants" = "glottocode")) %>%
  filter(!is.na(iso_639_3)) %>%
  select(iso_639_3, glottocode = node, level) %>%
  group_by(glottocode) %>%
  mutate(isocodes = list(unique(iso_639_3)))

# Manual Glottocode Matches from the mappings
to_glottocodes_manual <-
  IO$afrobarometer_mappings %>%
  map_df(function(.x) {
    if (!is.null(.x[["glottocode"]])) {
      # it shouldn't be empty but if it is, continue
      if (is.null(names(.x[["glottocode"]]))) {
        # if no names, then all countries
        out <- tidyr::crossing(question = .x[["question"]],
                               glottocode = as.character(.x[["glottocode"]]))
        out$valid_country <- NA_character_
      } else {
        glottocodes <- .x[["glottocode"]]
        out <- map2_df(names(glottocodes),
                       glottocodes,
                       ~ tibble(glottocode = as.character(.x), valid_country = .y))
        out <- tidyr::crossing(out, question = .x[["question"]])
      }
      if (nrow(out) == 0) {
        print(out)
      }
      out[["lang_id"]] <- .x$lang_id
      out[["round"]] <- .x$round
      out
    }
  }) %>%
  left_join(select(IO$afrobarometer_langs,
                    round, question, lang_id = value, iso_alpha2),
             by = c("round", "question", "lang_id")) %>%
  filter(is.na(valid_country) | (valid_country == iso_alpha2)) %>%
  select(-valid_country)

to_glottocodes_auto <-
  IO$afrobarometer_to_iso %>%
  # remove languages that are already matched
  anti_join(to_glottocodes_manual,
            by = c("round", "lang_id", "question")) %>%
  filter(iso_scope == "I") %>%
  select(round, lang_id, question, iso_alpha2, iso_639_3) %>%
  group_by(round, lang_id, question, iso_alpha2) %>%
  mutate(isocodes_from = list(unique(iso_639_3))) %>%
  # Join with Glottolog matches
  left_join(glottolog_to_iso, by = "iso_639_3") %>%
  mutate(all_matched = map2_lgl(isocodes_from, isocodes, ~ all(.x %in% .y))) %>%
  # Keep things that match all
  filter(all_matched) %>%
  group_by(round, lang_id, question, iso_alpha2) %>%
  filter(level == max(level)) %>%
  select(round, lang_id, question, iso_alpha2, glottocode) %>%
  distinct()

# All matches
afrobarometer_to_glottolog <-
  left_join(
    select(IO$afrobarometer_langs, round,
           lang_id = value, question, iso_alpha2, country, lang_name = name),
    bind_rows(
      to_glottocodes_auto,
      to_glottocodes_manual
    ),
    by = c("round", "question", "lang_id", "iso_alpha2")) %>%
  select(round, question, lang_id, lang_name, country, iso_alpha2, glottocode) %>%
  arrange(round, question, lang_id, country)

#'
#' # Test data
#'
assert_that(nrow(afrobarometer_to_glottolog) ==
              nrow(IO$afrobarometer_langs))

#' check primary key
assert_that(nrow(distinct(afrobarometer_to_glottolog,
                          round, question, lang_id, country))
            == nrow(afrobarometer_to_glottolog))

#' all glottolog langs should be valid
#'
afrobarometer_to_glottolog %>%
  filter(!is.na(glottocode)) %>%
  anti_join(glottolog_langoids, by = c("glottocode" = "id"))

#' All Afrobarometer codes should be acounted for
afrobarometer_to_glottolog %>%
  filter(is.na(glottocode)) %>%
  anti_join(filter(IO$afrobarometer_to_iso, iso_scope == "S"),
            by = c("round", "question", "lang_id", "country"))

glottolog_lang_geo <- read_csv("external/glottolog/languages_and_dialects_geo.csv")

#' All Glottocode languages should be
glottolog_non_african <-
  afrobarometer_to_glottolog %>%
    filter(!is.na(glottocode)) %>%
    left_join(select(glottolog_lang_geo, glottocode, macroarea),
         by = "glottocode") %>%
    filter(macroarea != "Africa") %>%
    filter(!glottocode %in% IO$misc_data$glottolog$non_african)
if (nrow(glottolog_non_african)) {
  print(select(macroarea, glottolog_non_african, glottocode,  lang_name, round, question, lang_id))
  stop("Non-African Glottolog languages found")
}

#' Write data
write_csv(afrobarometer_to_glottolog, OUTPUT, na = "")

