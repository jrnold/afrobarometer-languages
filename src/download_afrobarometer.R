#' ---
#' title: Download Afrobarometer data
#' ---
library("httr")
library("rprojroot")
library("purrr")

OUTPUT <- find_rstudio_root_file("external", "afrobarometer")
AFROBAROMETER_URLS <- list(
  "r1" = "http://afrobarometer.org/sites/default/files/data/round-1/merged_r1_data.sav",
  "r2" = "http://afrobarometer.org/sites/default/files/data/round-2/merged_r2_data.sav",
  "r3" = "http://afrobarometer.org/sites/default/files/data/round-3/merged_r3_data.sav",
  "r4" = "http://afrobarometer.org/sites/default/files/data/round-4/merged_r4_data.sav",
  "r5" = "http://afrobarometer.org/sites/default/files/data/round-5/merged-round-5-data-34-countries-2011-2013-last-update-july-2015.sav",
  "r6" = "http://afrobarometer.org/sites/default/files/data/round-6/merged_r6_data_2016_36countries2.sav"
)

#' Download Afrobarometer Merged Datasets
#'
#' Currently this only downloads r6
#'
#' @param path URL of Afrobarometer Data
#' @param dst
#' @return A string with the filename of the file created.
download_afrobarometer_file <- function(src, dst) {
  if (!dir.exists(dst)) {
    dir.create(dst, recursive = TRUE)
  }
  outfile <- file.path(dst, basename(src))
  if (!file.exists(outfile)) {
    message("Downloading ", src, "\n")
    resp <- GET(src, add_headers("user-agent" = "Mozilla/5.0"),
                write_disk(outfile, overwrite = TRUE))
    status <- status_code(resp)
    message("Status code: ", status, "\n")
    if (status == 200) {
      message("Wrote to ", outfile, "\n")
    } else {
      stop("Failed to download \n", call. = FALSE)
    }
    outfile
  } else {
    message(sprintf("%s already exists; not downloading it\n", dst))
  }
}

AFROBAROMETER_URLS %>%
  map(download_afrobarometer_file, dst = OUTPUT)

# afrobarometer_langs_r3 <- function() {
#   "http://afrobarometer.org/sites/default/files/data/round-3/merged_r3_data.sav" %>%
#   url() %>%
#   haven::read_sav() %>%
#   {bind_rows(
#     mutate(.,
#       question = "q3",
#       country = str_sub(as.character(as_factor(respno)), 1, 3),
#       label = as.character(haven::as_factor(afrob$q3)),
#       value = as.integer(unclass(afrob$q3))
#     ) %>% select(question, country, label, value),
#     mutate(.,
#       question = "q103",
#       country = str_sub(as.character(as_factor(respno)), 1, 3),
#       label = as.character(haven::as_factor(afrob$q103)),
#       value = as.integer(unclass(afrob$q103))
#     ) %>% select(question, country, label, value)
#   )} %>% count(question, label, value, country)
# }
#
# afrobarometer_langs_r2 <- function() {
#   "http://afrobarometer.org/sites/default/files/data/round-2/merged_r2_data.sav" %>%
#     url() %>%
#     haven::read_sav() %>%
#     {bind_rows(
#       mutate(.,
#              question = "q83",
#              country = str_sub(as.character(as_factor(respno)), 1, 3),
#              label = as.character(haven::as_factor(afrob$q83)),
#              value = as.integer(unclass(afrob$q83))
#       ) %>% select(question, country, label, value),
#       mutate(.,
#              question = "q97",
#              country = str_sub(as.character(as_factor(respno)), 1, 3),
#              label = as.character(haven::as_factor(afrob$q97)),
#              value = as.integer(unclass(afrob$q97))
#       ) %>% select(question, country, label, value)
#     )} %>% count(question, label, value, country)
# }
#
# afrobarometer_langs_r1 <- function() {
#   "http://afrobarometer.org/sites/default/files/data/round-1/merged_r1_data.sav" %>%
#     url() %>%
#     haven::read_sav() %>%
#     { mutate(.,
#              question = "language",
#              country = str_sub(as.character(as_factor(respno)), 1, 3),
#              label = as.character(haven::as_factor(afrob$language)),
#              value = as.integer(unclass(afrob$language))
#       ) %>% select(question, country, label, value),
#     } %>% count(question, label, value, country)
# }
#

afrobarometer_langs_r4 <- function() {
  "http://afrobarometer.org/sites/default/files/data/round-4/merged_r4_data.sav" %>%
  url() %>%
  haven::read_sav() %>%
  {bind_rows(
    mutate(.,
      question = "Q3",
      country = str_sub(as.character(as_factor(RESPNO)), 1, 3),
      label = as.character(haven::as_factor(.$Q3)),
      value = as.integer(unclass(.$Q3))
    ) %>% select(question, country, label, value),
    mutate(.,
      question = "Q103",
      country = str_sub(as.character(as_factor(RESPNO)), 1, 3),
      label = as.character(haven::as_factor(.$Q103)),
      value = as.integer(unclass(.$Q103))
    ) %>% select(question, country, label, value)
  )} %>% count(question, label, value, country)
}
