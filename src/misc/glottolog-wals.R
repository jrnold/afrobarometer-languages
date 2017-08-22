#' Exploring differences in Glottolog-WALS mappings
source("src/init.R")

wals <- IO$wals %>%
  select(wals_code, glottocode)

glottolog <- IO$glottolog_resourcemap %>%
  filter(type == "wals", !str_detect(identifier, "genus|family")) %>%
  select(-type, glottocode, wals_code = identifier)

glottolog_wals_genus <- IO$glottolog_resourcemap %>%
  filter(type == "wals", str_detect(identifier, "genus|family")) %>%
  select(-type, glottocode, wals_code = identifier) %>%
  left_join(select(IO$glottolog, glottocode, depth, name), by = "glottocode")

# In WALS and not Glottolog
wals %>%
  anti_join(glottolog, by = c("wals_code", "glottocode"))

# WALS codes that are not matched in glottolog
wals %>%
  anti_join(glottolog, by = c("wals_code"))

# Glottolog but not WALS
glottolog %>%
  anti_join(wals, by = c("wals_code", "glottocode"))

wals %>%
  anti_join(wals, by = c("wals_code"))

# Differences - WALS codes with different glottocode
full_join(glottolog, wals, by = "wals_code") %>%
  filter(glottocode.x != glottocode.y)

# Glottocodes with different WALS codes
full_join(glottolog, wals, by = "glottocode") %>%
  filter(wals_code.x != wals_code.y)

# Multiple Wals to same Glottocodes
group_by(wals, glottocode) %>%
  filter(n() > 1) %>%
  arrange(glottocode)

# Multiple Glottocodes to same wals
group_by(glottolog, wals_code) %>%
  filter(n() > 1) %>%
  arrange(wals_code)
