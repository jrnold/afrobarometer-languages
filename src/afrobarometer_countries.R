#' ---
#' Title: Country-level data for the Afrobarometer countries
#' ---
#' 
#' This dataset lists all the Afrobarometer countries.
#' It also matches them to standard abbreviations and names which makes
#' it easier to merge these data with other country level data.

country_codes <- read_csv("data-raw/country-codes/data/country-codes.csv")
afrobarometer_countries <- 
  read_csv("data-raw/afrobarometer-countrylist.csv") %>%
  rename(`ISO3166-1-Alpha-3` = iso_alpha3) %>%   
  left_join(country_codes, 
            by = c("ISO3166-1-Alpha-3"))

saveRDS(afrobarometer_countries,
        file = "data/afrobarometer_countries.rds")
