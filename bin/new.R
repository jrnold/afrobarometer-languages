# process each afrobarometer dataset
#
# - round: integer (1--6)
# - variable: (interview, interviewer, respondent, interview_other, interviewer_other, respondent_other)
# - country: ISO 3166 Alpha-2 code
# - value: language name (lower-case string)
#


library("tidyverse")
library("yaml")

glottolog_db <- src_sqlite("external/lingdata/glottolog.db")

glotto2wals <- tbl(glottolog_db, "wals_codes") %>%
  collect() %>%
  group_by(glottocode) %>%
  summarise(wals_code = list(sort(wals_code))) %>%
  deframe()

countries <- read_csv("data-raw/afrobarometer_countries.csv",
                      na = "")
variables <- read_csv("data-raw/afrobarometer_lang_variables.csv",
                      na = "")

# All combinations of (round, country, variable)
round_country_var <- inner_join(select(variables, round, name),
           select(countries, round, country = iso_alpha2),
           by = "round")

process_iso <- function(x) {
  cross_df(x[c("name", "country", "iso_639_3")])
}

process_glottolog <- function(x) {
  tryCatch(
    cross_df(x[c("name", "country", "glottocode")]),
    error = function(e) print(is.null(x[["glottocode"]]))
  )

}

process_wals <- function(x) {
  if (!is.null(x[["wals"]])) {
    print(x)
    cross_df(x[c("name", "country", "wals")])
  } else if (!is.null(x[["glottocode"]])) {
    wals_codes <- glotto2wals[[x[["glottocode"]]]]
    cross_df(list(name = x$name,
                  country = x$country, wals = wals_codes))
  } else {
    print(x)
    NULL
  }

}


links <- yaml.load_file("data-raw/links.yml")


to_iso <- map_dfr(links, process_iso)

to_glottolog <- discard(links, function(x) {is.null(x[["glottocode"]])}) %>%
  map_dfr(process_glottolog)

to_wals <- discard(links, function(x) {is.null(x[["glottocode"]])}) %>%
  map_dfr(process_wals)


