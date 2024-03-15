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
matchedPairsDefol <- "/Users/jgoldman/Library/CloudStorage/OneDrive-UniversityofToronto/Data/qc-data/matching-pairs-design/clean-tables/"
matchedPairsNonDefol <- 
defolInfo <- 

# Replace the target list below with your own:
list(
  tar_target(matchedDefolFile, matchedPairsDefol, format = "file"), 
  tar_target(matchedNonDefolFile, matchedPairsNonDefol, format = "file"), 
  tar_target(defolShp, sf::st_read(matchedPairesDefol)),
  tar_target(NondefolShp, sf::st_read(matchedPairedNondefol)),
  tar_target(defolInfoFile, defolInfo, format = "file"),
  tar_target(defolInfoTable, readr::read_csv(defolInfoFile)),
  tar_target(preppedData, cleanDataDefol(defolShp, NondefolShp, defolInfoTable))
)
