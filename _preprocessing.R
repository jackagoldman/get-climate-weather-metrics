library(targets)
library(tarchetypes) 

# Set target options:
tar_option_set(
  packages = c("tibble", "sf", "tidyverse", "reticulate", "rgee", "weathermetrics") 
)

# source scripts
tar_source("src/get-era5-indices.R")
tar_source("src/weather-analysis-tools.R")

#paths
matchedPairsDefol <- #"<path/to/my/file>"
matchedPairsNonDefol <- #"<path/to/my/file>"
defolInfo <- #"<path/to/my/file>"

# Replace the target list below with your own:
list(
  tar_target(matchedDefolFile, matchedPairsDefol, format = "file"), 
  tar_target(matchedNonDefolFile, matchedPairsNonDefol, format = "file"), 
  tar_target(defolShp, sf::st_read(matchedDefolFile)),
  tar_target(NondefolShp, sf::st_read(matchedNonDefolFile)),
  tar_target(defolInfoFile, defolInfo, format = "file"),
  tar_target(defolInfoTable, readr::read_csv(defolInfoFile)),
  tar_target(preppedData, cleanDataDefol(defolShp, NondefolShp, defolInfoTable)),
  tar_target(data.tsdgt1, removeTsd0(preppedData))
)
