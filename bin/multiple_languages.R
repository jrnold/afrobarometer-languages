#' ---
#' title: Multiple Languages
#' ---
#'
#' This question only appeared in round 4. It has a different enough structure
#' that I need to handle is separately.
source(here::here("src", "R", "init.R"))

# Name of the question in the Afrobarometer Survey
LANG_VAR <- "Q88E"
# Afrobarometer round number. It was only asked in round 4.
ROUND <- 4

OUTFILE <- here::here("data", "multiple_languages.csv")

#' Regexes that combine previously split languages into one language
REGEX_COMBINE <- c("\\bdamara(ra)? nama\\b" = "damara/nama",
                    "\\bpeulh fulfulde\\b" = "peulh/fulfulde",
                    "\\bga adangbe\\b" = "ga/adangbe",
                    "\\bga dangbe\\b" = "ga/dangbe",
                    "\\bsoninke sarakolle\\b" = "soninke/sarakolle",
                    "\\bsenufo mianka\\b" = "senufo/mianka ",
                    "\\b(pidgin|simple) english\\b" = "\\1_english",
                    "\\bkete krachi\\b" = "kete-krachi",
                    "\\bkari? kari?\\b" = "kari-kari",
                    "\\bshi (tcheua|shewa)\\b" = "shi-\\1",
                    "\\b(nyanja|sena) chewa\\b" = "\\1/chewa",
                    "\\bsimple liberian english\\b" = "simple_liberian_english",
                    "\\b(south(ern)?) sotho\\b" = "\\1_sotho",
                    "\\b(north(ern)?) sotho\\b" = "\\1_sotho")

REGEX_FIXES <- c("\\s*[(]" = "_(",
                 "\\band a little\\b" = " ",
                 "\\b(some|language|only)\\b" = " ",
                 "\\bet apparentes\\b" = " ",
                 "\\b([ns])\\s*sotho\\b" = " \\1_sotho ",
                 "\\bitalien_anglais\\b" = "\\bitalien anglais\\b",
                 # bad numbers
                 # No idea what this is. The unicode code-point doesn't make sense.
                 "\\bc1100\\b" = " ",
                 "\\bkimak0nde\\b" = "kimakonde",
                 "\\bloz1\\b" = "lozi",
                 "\\bp0rtugues\\b" = "portugues",
                 # no idea what this is
                 "3$" = "",
                 "c734ga" = " ",
                 "chewa1" = "chewa",
                 " fr$" = " french",
                 "-?1" = "missing",
                 # other replacementes mostly truncated endings of strings
                 "(emglish|engl\\[sh|englisgh)" = "english",
                 # "\\bengli sh\\b" = "english",
                 " fr$" = " french",
                 " fra$" = " french",
                 " eng$" = " english",
                 " engl?$" = " english",
                 " englis?$" = " english",
                 " afrik?$" = " afrikaans",
                 " afrikaans and$" = " afrikaans",
                 " soni$" = " soninke",
                 " dogo?$" = " dogon",
                 " peulh fu$" = " peulh fulfulde",
                 # peulh / fulfulde bambara soninke / sarakolle so
                 # since soninke already taken, probably sonrhai
                 " sarakolle so$" = " sarakolle sonrhai",
                 # soninke / sarakolle bambara peulh / fulfulde ma
                 " fulfulde ma$" = " fulfulde maure",
                 # soninke / sarakolle bambara peulh / fulfulde ar
                 " fulfulde ar$" = " fulfulde arabe",
                 # too ambiguous
                 # kiswahili kizigua kibondei kidigo kizaramo ki
                 # " kim?$" = "",
                 " peulh fulf$" = " peulh fulfulde",
                 " oshiw$" = " oshiwambo",
                 " afr$" = " afrikaans",
                 " lug$" = " luganda",
                 " oshiwam$" = " oshiwambo",
                 " portugue$" = " portuguese",
                 " ts$" = " tsonga",
                 # peulh seems most likely
                 " yoruba pe$" = " yoruba peulh",
                 " khassonke bamb$" = " khassonke bambara",
                 " soninke sara$" = " soninke sarakolle",
                 " fanakaloko seny$" = " fanakaloko senyanja",
                 " isizulu sesw$" = " isizulu seswati",
                 " english sexh$" = " english sexhosa",
                 " runyor$" = " runyoro",
                 " isoko lang$" = " isoko",
                 # too little information to tell
                 " [a-z]$" = "",
                 " xh$" = " xhosa",
                 " gre$" = " grebo",
                 " shuw$" = " shuwa",
                 # bambara malinke kakolo soninke / sarakolle mau
                 " mau$" = " maure",
                 # From FON, BARIBA, DENDI, OTAMARI, FRANCAIS, ANGLAIS, AD
                 # other speakers listing fon speak adja
                 " ad$" = " adja",
                 # Probably portugues
                 " po$" = " portuguese",
                 # other speakers of soninke speak ouolof
                 " ou$" = " oulof",
                 # and/e
                 "\\b(e|and)\\b" = " ",
                 # n = and, but don't match North Sotho
                 " n " = " ")

#' Clean language strings
initial_clean <- function(x) {
  x %>%
    # lower case
    str_to_lower() %>%
    # replace separtors with spaces
    str_replace_all("([,&+;./<]|-)", " ") %>%
    # standardize spaces - i hope there aren't tabs
    str_replace_all("\\s+", " ") %>%
    str_trim() %>%
    # fix patterns
    str_replace_all(REGEX_FIXES) %>%
    # standardize spaces
    str_replace_all("\\s+", " ") %>%
    str_trim() %>%
    # combine languges to single tokens
    str_replace_all(REGEX_COMBINE) %>%
    # standardize spaces
    str_replace_all("\\s+", " ") %>%
    str_trim()
}

final_clean <- function(x) {
  x %>%
    str_replace_all("_", " ") %>%
    str_trim()
}

countries <- IO$countries %>%
  filter(round == ROUND) %>%
  select(value, iso_alpha2)

# Clean languages into space-separeted strings.
cleaned_langs <-
  IO$afrobarometer(ROUND) %>%
  select(respno = RESPNO, country = COUNTRY,
         languages = UQ(LANG_VAR)) %>%
  mutate(languages_clean = initial_clean(languages),
         country = as.integer(country),
         respno = as.character(respno)) %>%
  left_join(countries, by = c("country" = "value")) %>%
  select(-country) %>%
  rename(country = iso_alpha2)

# Split the language strings.
# Return a data frame that is long (respno, lang_numb)
# merged with the ISO, WALS, and Glottolog codes.
split_langs <-
  cleaned_langs %>%
  select(-languages) %>%
  rename(languages = languages_clean) %>%
  mutate(languages = str_split(languages, "\\s+")) %>%
  # rename(languages = languages_clean) %>%
  unnest() %>%
  mutate(languages = final_clean(languages)) %>%
  rename(lang_name = languages) %>%
  left_join(IO$language_names, by = c("country", "lang_name" = "name")) %>%
  group_by(respno) %>%
  # add language number
  mutate(lang_numb = n())

write_csv(split_langs, OUTFILE, na = "")
