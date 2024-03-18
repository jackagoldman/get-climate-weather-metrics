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
extension <- ".csv"

# point to config file
reticulate::py_run_file(path2ConfigFile)
# Initalize gee
rgee::ee_Initialize()


# set bandlist (metrics to extract from era5)
bandList <- list('temperature_2m', 'total_precipitation_sum')

# set filter image dates
startDay <- 90  #ex. 120 (April 30)
endDay <- 243   #ex. 243 (Aug 31)


#get preprocessed data
processed_data <- tar_read(data.tsdgt1, store = "store_preprocessing")

#set values to map over
values_df <- tibble::tibble(processed_data) |> dplyr::select(c(id, prov))


# pipeline

clean <- tar_target(df.matchedTsd, matchTsd(processed_data))

mapped <- tarchetypes::tar_map(
    values = values_df,
    targets::tar_target(wx, weatherIndices(clean[["df.matchedTsd"]], bandList, startDay, endDay)), unlist = FALSE
    )
  
combined <- tar_combine(
    results,
    mapped[["wx"]],
    command = dplyr::bind_rows(!!!.x))
    
    
return <- targets::tar_target(write.results, output_wx(combined[["results"]], RES_DIR, filename, extension))

list(clean, mapped, combined, return)




