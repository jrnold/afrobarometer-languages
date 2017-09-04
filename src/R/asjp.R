library("stringdist")
library("stringr")
library("tidyverse")
library("foreach")
library("doParallel")
registerDoParallel()

source("src/R/init.R")

#' Tidy ASJP Database
#'
#' Tidy the ASJP data provided as the tab dataset.
#'
#' @param path Path to the tab dataset
#' @return A list of data frames:
#' \describe{
#' \item{languages}{
#'   A data frame with a row for each language.
#'   \tabular{lll}{
#'     \code{language} \tab character \tab ASJP language name \cr
#'     \code{wls_fam} \tab character \tab WALS family \cr
#'     \code{wls_gen} \tab character \tab WALS genus \cr
#'     \code{e} \tab character \tab \cr
#'     \code{hh} \tab character \tab \cr
#'     \code{lat} \tab numeric \tab latitude \cr
#'     \code{lon} \tab numeric \tab longitude  \cr
#'     \code{pop} \tab integer \tab population \cr
#'     \code{wcode} \tab character \tab WALS code  \cr
#'     \code{iso} \tab character \tab ISO 639-3 code
#'   }
#' }
#' \item{words}{
#'   A data frame with a row for each language, meaning, word combination:
#'   \tabular{lll}{
#'     \code{language} \tab character \tab Language meaning \cr
#'     \code{meaning} \tab character \tab Meaning \cr
#'     \code{word} \tab character \tab Word in that language \cr
#'     \code{synonym} \tab integer \tab Synonym number. If greater than 1, then there are mulitple words for that meaning. \cr
#'     \code{loanword} \tab logical \tab Word is a loan-word
#'   }
#' }
#' \item{meanings}{
#'   A data frame with 100 rows of the meanings used in the ASJP data.
#'   \tabular{lll}{
#'     \code{meaning} \tab character \tab Word meaning \cr
#'     \code{in_forty} \tab logical \tab 40 meanings used in ASJP distances \cr
#'     \code{id} \tab integer \tab Meaning ID, a number between 1 and 100.
#'    }
#'  }
#' }
#'
tidy_asjp <- function(path) {

  asjp <- read_tsv(path,
                   locale = locale(encoding = "CP1252"),
                   col_types = cols(
                     .default = col_character(),
                     lat = col_double(),
                     lon = col_double(),
                     pop = col_integer()
                   )) %>%
    rename(language = names)

  lang_vars <- c("wls_fam", "wls_gen", "e", "hh", "lat", "lon", "pop",
                 "wcode", "iso")

  asjp_meanings <-
    c("I" = TRUE,
      "you" = TRUE,
      "we" = TRUE,
      "this" = FALSE,
      "that" = FALSE,
      "who" = FALSE,
      "what" = FALSE,
      "not" = FALSE,
      "all" = FALSE,
      "many" = FALSE,
      "one" = TRUE,
      "two" = TRUE,
      "big" = FALSE,
      "long" = FALSE,
      "small" = FALSE,
      "woman" = FALSE,
      "man" = FALSE,
      "person" = TRUE,
      "fish" = TRUE,
      "bird" = FALSE,
      "dog" = TRUE,
      "louse" = TRUE,
      "tree" = TRUE,
      "seed" = FALSE,
      "leaf" = TRUE,
      "root" = FALSE,
      "bark" = FALSE,
      "skin" = TRUE,
      "flesh" = FALSE,
      "blood" = TRUE,
      "bone" = TRUE,
      "grease" = FALSE,
      "egg" = FALSE,
      "horn" = TRUE,
      "tail" = FALSE,
      "feather" = FALSE,
      "hair" = FALSE,
      "head" = FALSE,
      "ear" = TRUE,
      "eye" = TRUE,
      "nose" = TRUE,
      "mouth" = FALSE,
      "tooth" = TRUE,
      "tongue" = TRUE,
      "claw" = FALSE,
      "foot" = FALSE,
      "knee" = TRUE,
      "hand" = TRUE,
      "belly" = FALSE,
      "neck" = FALSE,
      "breast" = TRUE,
      "heart" = FALSE,
      "liver" = TRUE,
      "drink" = TRUE,
      "eat" = FALSE,
      "bite" = FALSE,
      "see" = TRUE,
      "hear" = TRUE,
      "know" = FALSE,
      "sleep" = FALSE,
      "die" = TRUE,
      "kill" = FALSE,
      "swim" = FALSE,
      "fly" = FALSE,
      "walk" = FALSE,
      "come" = TRUE,
      "lie" = FALSE,
      "sit" = FALSE,
      "stand" = FALSE,
      "give" = FALSE,
      "say" = FALSE,
      "sun" = TRUE,
      "moon" = FALSE,
      "star" = TRUE,
      "water" = TRUE,
      "rain" = FALSE,
      "stone" = TRUE,
      "sand" = FALSE,
      "earth" = FALSE,
      "cloud" = FALSE,
      "smoke" = FALSE,
      "fire" = TRUE,
      "ash" = FALSE,
      "burn" = FALSE,
      "path" = TRUE,
      "mountain" = TRUE,
      "red" = FALSE,
      "green" = FALSE,
      "yellow" = FALSE,
      "white" = FALSE,
      "black" = FALSE,
      "night" = TRUE,
      "hot" = FALSE,
      "cold" = FALSE,
      "full" = TRUE,
      "new" = TRUE,
      "good" = FALSE,
      "round" = FALSE,
      "dry" = FALSE,
      "name" = TRUE) %>%
    enframe(name = "meaning", value = "in_forty") %>%
    # meanings are numbered 1 to 100
    mutate(id = row_number())

  asjp_languages <- select(asjp, language, one_of(lang_vars))

  asjp_words <- asjp %>%
    select(-one_of(lang_vars)) %>%
    gather(meaning, word, -language, na.rm = TRUE) %>%
    mutate(word = str_split(word, ", +")) %>%
    unnest(word) %>%
    group_by(language, meaning, word) %>%
    mutate(synonym = row_number()) %>%
    ungroup() %>%
    # loanwords start with %
    mutate(loanword = {str_sub(word, 1, 1) == "%"},
           word = str_replace(word, "^%", "")) %>%
    ungroup()

  list(languages = asjp_languages,
       words = asjp_words,
       meanings = asjp_meanings)
}

lexical_dist <- function(language_1, language_2, wordlists) {
  x <- wordlists[[language_1]]
  y <- wordlists[[language_2]]
  common_words <- base::intersect(x$meaning, y$meaning)
  if (length(common_words)) {
    crossing(from = filter(x, meaning %in% common_words),
             to = filter(y, meaning %in% common_words)) %>%
      mutate(ld = stringdist(word, word1, method = "lv"),
             ldn = ld / pmax(str_length(word), str_length(word1))) %>%
      group_by(meaning, meaning1) %>%
      summarise(ld = mean(ld), ldn = mean(ldn)) %>%
      ungroup() %>%
      # calculate ldnd
      mutate(same_meaning = {meaning == meaning1}) %>%
      summarise(ldnd = mean(ldn[same_meaning]) / mean(ldn[!same_meaning]),
                ldn = mean(ldn[same_meaning]),
                ld = mean(ld[same_meaning])) %>%
      mutate(language_1 = UQ(language_1), language_2 = UQ(language_2))
  }
}

#' Calculate ASJP distances
#'
#' @param .data Data frame with columns language, meaning, word
#' @return Data frame with with \code{M (M - 1) / 2} rows, where \code{n} is the number of languages in \code{.data}.
#'   \tabular{rll} {
#'   \code{language_1} \tab character \tab 1st ASJP language name \cr
#'   \code{language_2} \tab character \tab 2nd ASJP language name \cr
#'   \code{ld} \tab double \tab Mean Levenshtein distance \cr
#'   \code{ldn} \tab double \tab Mean normalized Levenshtein distance (LDN) \cr
#'   \code{ldnd} \tab double \tab Mean normalize Levenshtein distance normalized (LDND)
#'   }
#'   language_2, ld, ln
asjp <- tidy_asjp(unz("external/asjp-dataset.tab.zip", filename = "dataset.tab"))

languages_to_use <-
  IO$afrobarometer_to_iso %>%
  select(iso_639_3) %>%
  distinct() %>%
  inner_join(asjp$languages, by = c("iso_639_3" = "iso")) %>%
  select(language) %>%
  distinct(language)
# languages_to_use <-
#   asjp$languages

wordlists <-
  asjp$words %>%
  semi_join(languages_to_use, by = "language") %>%
  # keep only the 40 words used for AJSP calcs
  semi_join(filter(asjp$meanings, in_forty), by = "meaning") %>%
  # remove loanwords
  filter(!loanword) %>%
  select(language, meaning, word) %>%
  split(.$language) %>%
  map(~ select(.x, -language)) %>%
  as.environment()

langpairs <-
  crossing(language_from = ls(wordlists),
           language_to = ls(wordlists)) %>%
  filter(language_from < language_to)

f <- partial(lexical_dist, wordlists = wordlists)
asjp_dists <-
  foreach(x = langpairs$language_from,
          y = langpairs$language_to,
          .combine = bind_rows) %dopar% { f(x, y) }
