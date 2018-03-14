#' ---
#' title: "Create afrobarometer_variables table"
#' ---
source(here::here("src", "R", "init.R"))

OUTPUT <- project_path("data", "afrobarometer_variables.csv")

IO$afrobarometer_variables_raw %>%
  write_csv(path = OUTPUT, na = "")
