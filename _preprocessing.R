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
matchedPairsDefol <- "~/Desktop/OneDrive - University of Toronto/Data/chapter_3/on-qc-defol.shp"
matchedPairsNonDefol <- "~/Desktop/OneDrive - University of Toronto/Data/chapter_3/on-qc-nondefol.shp"
defolInfo <- "~/Work/PhD/sbw-fire-interactions/nbr-recovery/data/paired_fires/on-qc-defol-table.csv"

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
