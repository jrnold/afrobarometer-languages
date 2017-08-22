# Geographic Information for all Afrobarometer Countries
#
# This script queries the geonames API to get the bounding box
# for each country in the Afrobarometer surveys. This can be used
# to check the plausibility of language matches since the Glottolog doesn't directly include country info.
#
library(tidyverse)

centroid <- function(countryCode, east, west, north, south, ...) {
  x <- matrix(c(east, east, west, west,
                north, south, south, north),
              ncol = 2)
  cent <- geosphere::centroid(x)
  tibble(longitude = cent[1], latitude = cent[2])
}

countrygeo <-
  IO$afrobarometer_countries %>%
  pluck("iso_alpha2") %>%
  unique() %>%
  map_df(geonames::GNcountryInfo) %>%
  select(iso_alpha2 =countryCode,
         east, west, north, south, languages) %>%
  mutate_at(vars(east, west, north, south), as.numeric) %>%
  bind_cols(pmap_df(., centroid))


