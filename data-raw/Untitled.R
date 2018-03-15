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

IO$afrobarometer(4) %>%
  select(respno = RESPNO, country = COUNTRY,
         languages = UQ(lang_var)) %>%
  mutate(languages = str_to_lower(languages),
         languages = str_replace_all(languages, "\\s*/\\s*", "/"),
         languages = str_replace_all(languages, "([ns])\\.?\\s*sotho", "\\1_sotho"),
         languages = str_replace_all(languages, "[,&+;.]", " "),
         languages = str_replace_all(languages, " (and|n|e) ", " "),
         languages = str_replace_all(languages, "\\s*\\(", "_("),
         languages = str_replace_all(languages, "\\s+", " "),
         languages = str_replace_(languages, " eng$", " english"),
         languages = str_replace_(languages, "pidgin[ -]?english", "pidgin_english"),
         languages = str_replace_(languages, "kari-kari", "kari_kari"),
         country = as.integer(country),
         respno = as.character(respno)) %>%
  left_join(countries, by = c("country" = "value")) %>%
  select(-country) %>%
  rename(country = iso_alpha2) %>%
  unnest() %>%
  pluck("languages") %>%
  unique() %>%
  sort() %>%
  str_c(collapse = "\n") %>%
  cat(file = "data-raw/q88e-responses.text")
