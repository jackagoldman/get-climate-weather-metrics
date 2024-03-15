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
RES_DIR <- ""
path2ConfigFile <-    #example "~/Code/python-rgee-config.py"

# point to config file
reticulate::py_run_file(path2ConfigFile)
# Initalize gee
ee_Initialize()


# set bandlist (metrics to extract from era5)
bandList <- list('temperature_2m', 'total_precipitation_sum')


#get preprocessed data
processed_data <- tar_read(preppredData, store = "store_preprocessing")

#set values to map over
values_df <- tibble::tibble(processed_data) |> dplyr::select(c(id, prov))

# Replace the target list below with your own:
list(
  tarchetypes::tar_map(
    values = values_df,
    targets::tar_target(wx, weatherIndices(processed_data, bandList)))
  
)
