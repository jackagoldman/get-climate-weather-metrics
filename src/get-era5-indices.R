
# set bandlist ( to be done in _targets)
bandList <- list('temperature_2m', 'total_precipitation_sum')

weatherIndices <- function(data, bandList){
  require(weathermetrics)
  require(sf)
  
  #get centroid
  data1 <- getCentroid(data)
  fireCent <- data1$centroid
  
  #return fire centroids as ee object
  fireCentee <- sf_as_ee(fireCent)
  fireCenteeDefol <- sf_as_ee(fireCent[[1]])
  fireCenteeNonDefol <- sf_as_ee(fireCent[[2]])
  
  #get timeframe 
  #NOTE : timeframe must be from year of defol to year of fire OR date of fire, right now its year of fire
  #should this be summer temperature only? or growing season?
  filterTime <- timeFrame(data1)
  endDefolYear <- filterTime$time.gap
  fireYear <- filterTime$year
  
  
  #filter imagery by data and filter to point
  era5 <- ee$ImageCollection("ECMWF/ERA5_LAND/DAILY_AGGR")$filterDate(endDefolYear, fireYear)$filterBounds(fireCentee)
  
  #filter era5 bands
  era5Bands <- era5$select(bandList)
  
  # calculate mean composite over timeframe
  era5means <- era5Bands$mean()

  # extract values
  # defol
  ee.defol <- ee_extract(
    x = era5means,
    y = fireCenteeDefol,
    scale = 250,
    fun = ee$Reducer$mean(), # mean or sum
    sf = TRUE
  )
 # convert temperature to celcius from kelvin
 ee.defol <- mutate(ee.defol, temperature_2m = weathermetrics::kelvin.to.celsius(temperature_2m))

 # defol
 ee.nondefol <- ee_extract(
   x = era5means,
   y = fireCenteeNonDefol,
   scale = 250,
   fun = ee$Reducer$mean(), # mean or sum
   sf = TRUE
 )
 # convert temperature to celcius from kelvin
 ee.nondefol <- mutate(ee.nondefol, temperature_2m = weathermetrics::kelvin.to.celsius(temperature_2m))
 
 # add defol column and rbind
 ee.defol <- mutate(ee.defol, defoliated = as.numeric(1))
 ee.nondefol <- mutate(ee.nondefol, defoliated = as.numeric(0))
 
 #merge data
 data.set <- rbind(ee.defol, ee.nondefol) 
 
 #join to data1 
 data.return <- data1 |> 
   left_join(data.set, by = "defoliated") |> 
   select(-c(centroid))
 
 # return data frame
 return(data.return)
 
}



