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
matchedPairsDefol <- ""
matchedPairsNonDefol <- 
defolInfo <- 
RES_DIR <- ""


# set bandlist (metrics to extract from era5)
bandList <- list('temperature_2m', 'total_precipitation_sum')

# Replace the target list below with your own:
list(
  tar_target(matchedDefolFile, matchedPairsDefol, format = "file"), 
  tar_target(matchedNonDefolFile, matchedPairsNonDefol, format = "file"), 
  tar_target(defolShp, st_read()),
  tar_target(NondefolShp, st_read()),
  tar_target(defolInfoFile, defolInfo, format = "File"),
  tar_target(defolInfoTable, read_csv(defolInfoFile)),
  tar_target(preppedData, cleanDataDefol(defolShp, NondefolShp, defolInfoTable)),
  tar_map()
  
)
