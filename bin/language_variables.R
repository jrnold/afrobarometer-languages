#' ---
#' title: "Create language_variables table"
#' ---
source(here::here("src", "R", "init.R"))

OUTPUT <- project_path("data", "language_variables.csv")

IO$afrobarometer_variables_raw %>%
  select(round, matches("^(interview(er)?|respondent)")) %>%
  gather(type, variable, -round, na.rm = TRUE) %>%
  mutate(other = str_detect(type, "_other$"),
         type = str_replace(type, "_other$", "")) %>%
  write_csv(OUTPUT, na = "")
