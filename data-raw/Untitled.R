source(here::here("src", "R", "init.R"))

lang_var <- "Q88E"
rnd <- 4

countries <- IO$countries %>%
  filter(round == rnd) %>%
  select(value, iso_alpha2)
#
# - damara/nama
# - peulh/fulfulde
# - dangbe/ga
# - ga/adangbe
# - ga/dangbe
# - soninke / sarakolle
# - senufo / mianka

#' Regexes that combine previously split languages into one language
REGEX_COMBINE <- c("\\bdamara(ra)? nama\\b" = "damara_/_nama",
                    "\\bpeulh fulfulde\\b" = "peulh_/_fulfulde",
                    "\\bga adangbe\\b" = "ga_/_adangbe",
                    "\\bga dangbe\\b" = "ga_/_dangbe",
                    "\\bsoninke sarakolle\\b" = "soninke_/_sarakolle",
                    "\\bsenufo mianka\\b" = "senufo_/_mianka ",
                    "\\b(pidgin|simple) english\\b" = "\\1_english",
                    "\\bkete krachi\\b" = "kete-krachi",
                    "\\bkari? kari?\\b" = "kari-kari",
                    "\\bshi (tcheua|shewa)\\b" = "shi-\\1",
                    "\\b(nyanja|sena) chewa\\b" = "\\1_/_chewa",
                    "\\bsimple liberian english\\b" = "simple_liberian_english")

REGEX_FIXES <- c("\\s*[(]" = "_(",
                 "\\band a little\\b" = " ",
                 "\\b(some|language)\\b" = " ",
                 "\\bet apparentes\\b" = " ",
                 "\\b([ns])\\s*sotho\\b" = " \\1_sotho ",
                 "italien_anglais" = "italien anglais",
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
                 " engl?$" = " english",
                 " englis?$" = " english",
                 " afrik?$" = " afrikaans",
                 " afrikaans and$" = " afrikaans",
                 " eng$" = " english",
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
                 " kim?$" = "",
                 " peulh fulf$" = " peulh/fulfulde",
                 " oshiw$" = " oshiwambo",
                 " afr$" = " afrikaans",
                 " lug$" = " luganda",
                 " oshiwam$" = " oshiwambo",
                 " portugue" = " portuguese",
                 " ts$" = " tsonga",
                 # peulh seems most likely
                 " yoruba pe$" = " yoruba peulh",
                 " khassonke bamb$" = " khassonke bambara",
                 " soninke sara$" = " soninke sarakolle",
                 " fanakaloko seny$" = " fanakaloko senyanja",
                 " isizulu sesw$" = " isizulu seswati",
                 " english sexh$" = " english sexhosa",
                 " runyor$" = " runyoro",
                 # too little information to tell
                 " [a-z]$" = "",
                 " xh$" = " xhosa",
                 " gre$" = " grebo",
                 " shuw$" = " shuwa",
                 " xhosa ts$" = " xhosa tsonga",
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
                 " (e|and) " = " ",
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


cleaned_langs <-
  IO$afrobarometer(4) %>%
  select(respno = RESPNO, country = COUNTRY,
         languages = UQ(lang_var)) %>%
  mutate(languages_clean = initial_clean(languages),
         country = as.integer(country),
         respno = as.character(respno)) %>%
  left_join(countries, by = c("country" = "value")) %>%
  select(-country) %>%
  rename(country = iso_alpha2)

split_langs <-
  cleaned_langs %>%
  select(-languages) %>%
  mutate(languages = str_split(languages_clean, "\\s+")) %>%
  # rename(languages = languages_clean) %>%
  unnest() %>%
  pluck("languages") %>%
  final_clean() %>%
  unique() %>%
  sort() %>%
  str_c(collapse = "\n") %>%
  cat(file = "data-raw/q88e-responses.text")

# need to resolve these issues.
# po: Oshiwambo, Russian, English, French, Afrikaans, Po Not sure.
# gu: from English, Afrikaans, Zulu, Xhosa, Kokni, Arabic, Gu ??
# ao - ok!
# ki
# ko
# ad
