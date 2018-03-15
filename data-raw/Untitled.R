source(here::here("src", "R", "init.R"))

lang_var <- "Q88E"
rnd <- 4

countries <- IO$countries %>%
  filter(round == rnd) %>%
  select(value, iso_alpha2)

IO$afrobarometer(4) %>%
  select(respno = RESPNO, country = COUNTRY,
         languages = UQ(lang_var)) %>%
  mutate(languages = str_replace(languages, "\\s+", " "),
         languages = str_split(str_to_lower(languages), "\\s*([,;&+]| (e|n|and) )\\s*"),
         country = as.integer(country),
         respno = as.character(respno)) %>%
  left_join(countries, by = c("country" = "value")) %>%
  select(-country) %>%
  rename(country = iso_alpha2) %>%
  unnest() %>%
  pluck("languages") %>%
  unique() %>% sort()
