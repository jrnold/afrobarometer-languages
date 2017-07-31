source("src/init.R")

#' Copy country variable data
file.copy(project_path("data-raw", "afrobarometer_lang_variables.csv"),
          project_path("data"))
