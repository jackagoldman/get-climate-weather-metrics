library(targets)
library(tarchetypes) 

# Set target options:
tar_option_set(
  packages = c("tibble", "sf", "tidyverse", "reticulate", "rgee", "weathermetrics") 
)

# source scripts
tar_source("src/get-era5-indices.R")
tar_source("src/weather-analysis-tools.R")


# Replace the target list below with your own:
list(
  tar_target()
)
