#' ---
#' title: "Match Afrobarometer Languages to ISO 639-3 Language Codes"
#' ---
#'
#' These matches were originally made by matching
#' alternative names along with manual editing.
#'
suppressPackageStartupMessages({
  library("tidyverse")
  library("stringr")
  library("rprojroot")
  library("purrr")
  library("magrittr")
  library("yaml")
})

OUTPUT <- find_rstudio_root_file("data", "afrobarometer_other_langs_to_iso_639_3.csv")

INPUTS <- list(
  afrobarometer_other_langs = list("data", "afrobarometer_other_langs.csv"),
  iso_langs = list("external",
                   "iso-639-3",
                   "iso-639-3_Code_Tables_20170217",
                   "iso-639-3_20170202.tab"),
  iso_macrolangs = list("external",
                        "iso-639-3",
                        "iso-639-3_Code_Tables_20170217",
                        "iso-639-3-macrolanguages_20170131.tab"),
  afrobarometer_other_langs_to_iso =
    list("data-raw", "afrobarometer_other_langs.yml"),
  misc_data = list("data-raw", "misc.yml")
  ) %>%
  {setNames(map(., function(x) invoke(find_rstudio_root_file, x)),
            names(.))}

# Read Afrobarometer Languages
afrobarometer_other_langs <-
  read_csv(INPUTS$afrobarometer_other_langs,
           col_types = cols(
             country = col_integer(),
             question = col_character(),
             value = col_character(),
             round = col_character(),
             country_iso = col_character()
           ))

misc_data <- yaml.load_file(INPUTS$misc_data)

afrobarometer_other_langs_to_iso <-
  yaml.load_file(INPUTS$afrobarometer_other_langs_to_iso) %>%
  map(compact) %>%
  map_df(function(.x) {
    if (!is.null(.x[["iso_639_3"]])) {
      tibble(country = .x$country,
             lang_name = .x$lang_name,
             iso_639_3 = .x$iso_639_3)
    }
  })


iso_macrolangs <-
  read_tsv(INPUTS$iso_macrolangs,
           col_types = cols(
             M_Id = col_character(),
             I_Id = col_character(),
             I_Status = col_character()
           ), na = "") %>%
  filter(I_Status == "A") %>%
  select(-I_Status)

afrobarometer_other_langs_to_iso %<>% {
    bind_rows(
      .,
      inner_join(., iso_macrolangs,
                 by = c("iso_639_3" = "I_Id")) %>%
        select(-iso_639_3) %>%
        rename(iso_639_3 = M_Id) %>%
        distinct()
    )
  }

#' add ISO information
iso_langs <-
  read_tsv(INPUTS$iso_langs,
           col_names =
             c("Id", "Part2B", "Part2T", "Part1", "Scope", "Language_Type",
               "Ref_Name", "Comment"),
           col_types =
             cols(
               Id = col_character(),
               Part2B = col_character(),
               Part2T = col_character(),
               Part1 = col_character(),
               Scope = col_character(),
               Language_Type = col_character(),
               Ref_Name = col_character(),
               Comment = col_character()
             ), na = "") %>%
  filter(Language_Type %in% c("L", "S")) %>%
  select(-Language_Type, -Comment)

afrobarometer_other_langs_to_iso %<>%
  left_join(select(iso_langs,
                   iso_639_3 = Id,
                   iso_scope = Scope,
                   iso_ref_name = Ref_Name,
                   ),
            by = "iso_639_3")

#' Merge to original other language-variable data
#'
afrobarometer_other_langs_to_iso_639_3 <-
  left_join(afrobarometer_other_langs,
            afrobarometer_other_langs_to_iso,
            by = c("country_iso" = "country",
                   "value" = "lang_name"))

afrobarometer_other_langs_to_iso_639_3 %>%
  write_csv(path = OUTPUT, na = "")
