


#' Title
#'
#' @param data 
#' @param bandList 
#'
#' @return single row dataframe.
#' 
#' @details
#' add a argument to the function that takes different timeFrame specifications. This will make it more adaptible.
#' 
#'
#' @examples
weatherIndices <- function(data, bandList){
  
  require(weathermetrics)
  require(sf)
  require(tidyverse)
  
  data <- data2sf(data)
  
  #get centroid
  data1 <- getCentroid(data)
  fireCent <- data1$centroid
  fireID <- data1$id
  
  
  #return fire centroids as ee object
  fireCentee <- sf_as_ee(fireCent)

  
  #get timeframe 
  #NOTE : timeframe must be from year of defol to year of fire OR date of fire, right now its year of fire
  #should this be summer temperature only? or growing season?
  filterTime <- timeFrame(data1)
  imageStart <- filterTime$time.gap
  fireYear <- filterTime$year
  
  
  #filter imagery by data and filter to point
  era5 <- ee$ImageCollection("ECMWF/ERA5_LAND/DAILY_AGGR")$filterDate(imageStart, fireYear)$filterBounds(fireCentee)
  
  #filter era5 bands
  era5Bands <- era5$select(bandList)
  
  # calculate mean composite over timeframe
  era5means <- era5Bands$mean()

  # extract values
  era5.metrics <- ee_extract(
    x = era5means,
    y = fireCentee,
    scale = 250,
    fun = ee$Reducer$mean(), # mean or sum
    sf = TRUE
  )
 # convert temperature to celcius from kelvin
  era5.metrics <- mutate( era5.metrics, temperature_2m = weathermetrics::kelvin.to.celsius(temperature_2m))

  

 #join to data1 
 data1.drop <- data1 |> st_drop_geometry()
 data.return <- cbind(data1.drop,  era5.metrics)
 data.return <- data.return |> st_drop_geometry() |> as.list()
 # return data frame
 return(data.return)
 
}




