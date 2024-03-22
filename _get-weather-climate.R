library(targets)
library(tarchetypes) 
library(crew)
library(rgee)

mycontroller <- crew::crew_controller_local(
  name = "my_controller",
  workers = 10,
  seconds_idle = 3,
  reset_globals = FALSE
)

# Set target options:
tar_option_set(
  packages = c("tibble", "sf", "tidyverse", "reticulate", "rgee", "weathermetrics") ,
  error = "abridge"
)

# source scripts
tar_source("src/get-era5-indices.R")
tar_source("src/weather-analysis-tools.R")
tar_source("src/fwi-equations.R")

#paths
RES_DIR <- "~/Desktop/" # ex. "~/Desktop/"
path2ConfigFile <- "~/Code/python-rgee-config.py" #example "~/Code/python-rgee-config.py"
filename <- "on-qc-wx-clim" # ex. "on-qc-wx-clim"
extension <- ".csv" # ex. ".shp" or ".csv"

# point to config file
reticulate::py_run_file(path2ConfigFile)
# Initalize gee
rgee::ee_Initialize()


# set bandlist (metrics to extract from era5)
bandList <- list('temperature_2m', 'total_precipitation_sum')

# set filter image dates
startDay <- 120  #ex. 120 (April 30)
endDay <- 243   #ex. 243 (Aug 31)

# get preprocessed data
# if file is from _preprocessing pipeline it must be read in using tar_read 
# and the preprocessing data store
#if the file is not from the preprocessing pipeline read in from path
processed_data <- tar_read(data.tsdgt1, store = "store_preprocessing")
# dataPath <- ""



#set values to map over
values_df <- tibble::tibble(processed_data) |> dplyr::select(c(id))

# pipeline

list(tar_target(df.matchedTsd, matchTsd(processed_data), priority = 1),
  tar_target(defolOnly, command = dplyr::filter(df.matchedTsd, defoliated == "1"), priority = 1),
  tar_target(dailyWx.res, getDailyWx(values_df, defolOnly), priority = 1),
  tar_target(weather.results, output_daily(dailyWx.res, RES_DIR, "weather"), priority = 1),
  tar_target(wxClimData, getWxClim(values_df, df.matchedTsd)),
  tar_target(write.results, output_wx(wxClimData, RES_DIR, filename, extension)),
  tar_target(droughtCode, getDC(dailyWx.res), priority = 1),
  tar_target(dc.results, output_daily(droughtCode, RES_DIR, "dc"), priority = 1),
  tar_target(finefuelMC, getFFMC(dailyWx.res), priority = 1),
  tar_target(ffmc.results, output_daily(finefuelMC, RES_DIR, "ffmc"), priority = 1)
)









