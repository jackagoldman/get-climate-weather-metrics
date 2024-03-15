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
RES_DIR <- "results/"
path2ConfigFile <-  "~/Code/python-rgee-config.py"  #example "~/Code/python-rgee-config.py"
filename <- "on-qc-wx-clim"
extension <- ".shp"

# point to config file
reticulate::py_run_file(path2ConfigFile)
# Initalize gee
rgee::ee_Initialize()


# set bandlist (metrics to extract from era5)
bandList <- list('temperature_2m', 'total_precipitation_sum')


#get preprocessed data
processed_data <- tar_read(preppedData, store = "store_preprocessing")

#set values to map over
values_df <- tibble::tibble(processed_data) |> dplyr::select(c(id, prov))

# Replace the target list below with your own:

list(tarchetypes::tar_map(
    values = values_df,
    targets::tar_target(wx, weatherIndices(processed_data, bandList))),
    targets::tar_target(results, command = dplyr::bind_rows(wx)),
    targets::tar_target(write.results, output_wx(results, RES_DIR, filename, extension))

)


