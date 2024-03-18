#' get weather indices
#'
#' @param data 
#' @param bandList 
#'
#' @return single row dataframe. 
#' 
#' 
#' @details
#' Temperature_2m is returned in degrees celcius.
#' total_precipitation is returned in (mm).
#' add a argument to the function that takes different timeFrame specifications. This will make it more adaptible.
#' 
#'
#' @examples
#' bandList <- list("temperature_2m")
#' wx_results <- weatherIndices(fires, bandList, 120, 240)
#' 
weatherIndices <- function(data, bandList, startDay, endDay){
  
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
  
  #filter to days of year
  era5 <- era5$filter(ee$Filter$dayOfYear(startDay, endDay))
  

  #if precipiation in band list, make second image collection and at to era5means as band
  ifelse(stringr::str_detect(bandList , "_precip"), 
         bandListPre <- bandList[grepl("_precip",bandList)],
         bandList <- bandList[!grepl("_precip", bandList)] )
  
  #filter era5 bands
  era5Bands <- era5$select(bandList)
  
 #era5 precip
 if(exists("bandListPre")){
  era5precip <- ee$ImageCollection("ECMWF/ERA5/MONTHLY")$
    filterDate(imageStart, fireYear)$
    filterBounds(fireCentee)$
    filter(ee$Filter$dayOfYear(startDay, endDay))$
    select('total_precipitation')$sum()
  
  era5.precip <- ee_extract(
    x = era5precip,
    y = fireCentee,
    scale = 250,
    fun = ee$Reducer$sum(), # mean or sum
    sf = TRUE
  ) |> st_drop_geometry() 
  
  era5.precip <- mutate(era5.precip, total_precipitation = (total_precipitation * 1000))
 }
  
  # get metrics for other bands
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
  era5.metrics <- mutate(era5.metrics, temperature_2m = weathermetrics::kelvin.to.celsius(temperature_2m))

  
  if(exists("era5.precip")){
    era5.metrics <- cbind(era5.metrics, era5.precip)
  }

 #join to data1 
 data1.drop <- data1 |> st_drop_geometry() 
 data.return <- cbind(data1.drop,  era5.metrics)
 data.return <- data.return |> st_drop_geometry() |> select(-c(geometry)) 
 # return data frame
 return(data.return)
 
}




