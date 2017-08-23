# distances between WALS languages using the Glottolog tree
wals2glotto <- select(IO$wals, wals_code, glottocode) %>%
  filter(!is.na(glottocode)) %>%
  inner_join(select(languoids, glottocode, depth, ancestors, family,
                    latitude, longitude),
             by = "glottocode")

wals_glottodist <-
  wals2glotto %>%
  inner_join(select(wals2glotto, family,
                    wals_to = wals_code,
                    latitude_to = latitude,
                    longitude_to = longitude,
                    ancestors_to = ancestors),
             by = "family") %>%
  filter(wals_code != wals_to) %>%
  mutate(phylodist = map2_int(ancestors, ancestors_to, ~ length(setdiff(.x, .y))),
         # divide by 1000 to put it into kilometers
         geodist = round(geosphere::distGeo(cbind(longitude_to, latitude_to), cbind(longitude, latitude)) / 1000)) %>%
  select(wals_code, wals_to, phylodist, geodist) %>%
  group_by(wals_code, wals_to) %>%
  summarise(phylodist = min(phylodist),
            geodist = min(geodist))
