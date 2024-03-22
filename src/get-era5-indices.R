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
  
  
  ###### PRE-FIRE
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
  era5precip <- ee$ImageCollection("ECMWF/ERA5_LAND/DAILY_AGGR")$
    filterDate(imageStart, fireYear)$
    filterBounds(fireCentee)$
    filter(ee$Filter$dayOfYear(startDay, endDay))$
    select('total_precipitation_sum')$sum()
  
  era5.precip <- ee_extract(
    x = era5precip,
    y = fireCentee,
    scale = 250,
    fun = ee$Reducer$sum(), # mean or sum
    sf = TRUE
  ) |> st_drop_geometry() 
  
  era5.precip <- mutate(era5.precip, total_precipitation_sum = (total_precipitation_sum * 1000))
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
    era5.metrics <- rename(era5.metrics, pre_fire_precip = total_precipitation_sum)
    
  }

  era5.metrics <- rename(era5.metrics, pre_fire_temp = temperature_2m)
  
  ##### Post
  # get fire year + 10
  postYear <-  eedate_to_rdate(rdate_to_eedate(fireYear)$advance(10, "year")) |> as.character()
  
  #if precipiation in band list, make second image collection and at to era5means as band
  ifelse(stringr::str_detect(bandList , "_precip"), 
         bandListPre <- bandList[grepl("_precip",bandList)],
         bandList <- bandList[!grepl("_precip", bandList)] )
  
  #era5 precip
  if(exists("bandListPre")){
    era5postPrecip <- ee$ImageCollection("ECMWF/ERA5_LAND/DAILY_AGGR")$
      filterDate(fireYear, postYear)$
      filterBounds(fireCentee)$
      filter(ee$Filter$dayOfYear(startDay, endDay))$
      select('total_precipitation_sum')$sum()
    
    era5.post.precip <- ee_extract(
      x = era5precip,
      y = fireCentee,
      scale = 250,
      fun = ee$Reducer$sum(), # mean or sum
      sf = TRUE
    ) |> st_drop_geometry() 
    
    era5.post.precip <- mutate(era5.post.precip, total_precipitation_sum = (total_precipitation_sum * 1000))
  }
  
  # get metrics for other bands
  # calculate mean composite over timeframe
  era5postMeans <- ee$ImageCollection("ECMWF/ERA5_LAND/DAILY_AGGR")$
    filterDate(fireYear, postYear)$
    filterBounds(fireCentee)$
    filter(ee$Filter$dayOfYear(startDay, endDay))$
    select(bandList)$
    mean()

  
  # extract values
  era5.post.metrics <- ee_extract(
    x = era5postMeans,
    y = fireCentee,
    scale = 500,
    fun = ee$Reducer$mean(), # mean or sum
    sf = TRUE
  )
  # convert temperature to celcius from kelvin
  era5.post.metrics <- mutate(era5.post.metrics, temperature_2m = weathermetrics::kelvin.to.celsius(temperature_2m))
  
  
  if(exists("era5.post.precip")){
    era5.post.metrics <- cbind(era5.post.metrics, era5.post.precip)
    era5.post.metrics <- rename(era5.post.metrics, post_fire_precip = total_precipitation_sum)

  }
  
  era5.post.metrics <- rename(era5.post.metrics, post_fire_temp = temperature_2m)
  
  
  
 #join to data1 
 data1.drop <- data1 |> st_drop_geometry() 
 data.return <- cbind(data1.drop,  era5.metrics, era5.post.metrics)
 data.return <- data.return |> st_drop_geometry() |> select(-c(geometry, tsd, cumltve_yrs, Fire_Year, prov))
 # return data frame
 return(data.return)
 
}


#' Wrapper around weatherIndices function 
#'
#' @param values_df 
#' @param data 
#'
#' @return
#' @export
#'
#' @examples
getWxClim <- function(values_df, data){
  
  #create empty list
  qlist <- list()
  
  for(i in 1:nrow(values_df)){
  
  qlist[[i]] <-  weatherIndices(data[i,], bandList, startDay, endDay)
  i + 1
  
}
  res <- do.call(rbind, qlist)
  
  return(res)
}




#' Gets daily weather for centroid using era5
#'
#' @param data 
#' @param startDay 
#' @param endDay 
#'
#' @return dataframe. columns for month, doy, temp(t), relative humidity(rh), precipitation(ppt), wind speed (ws), id, lat, lon
#' @export
#'
#' @examples
dailyWeather <- function(data, startDay, endDay){
  
  require(weathermetrics)
  require(sf)
  require(tidyverse)
  
  data <- data2sf(data)
  
  #get centroid
  data1 <- getCentroid(data)
  fireCent <- data1$centroid
  fireID <- data1$id
  
  #get geometry
  geom <- sf_as_ee(data$geometry)
  
  #return fire centroids as ee object
  fireCentee <- sf_as_ee(fireCent)
  
  #get timeframe 
  #NOTE : timeframe must be from year of defol to year of fire OR date of fire, right now its year of fire
  #should this be summer temperature only? or growing season?
  filterTime <- timeFrame(data1)
  imageStart <- filterTime$time.gap
  fireYear <- filterTime$year
  
  
  ###### PRE-FIRE
  #filter imagery by data and filter to point
  era5 <- ee$ImageCollection("ECMWF/ERA5_LAND/DAILY_AGGR")$
    select('temperature_2m', 'dewpoint_temperature_2m', 'total_precipitation_sum', 
           'u_component_of_wind_10m', 'v_component_of_wind_10m')$
    filterBounds(geom)$
    filter(ee$Filter$dayOfYear(startDay, endDay))$
    filterDate(imageStart, fireYear)
    
  

  
  # need to correct for kelvin by converting to celcius ( k -273.15 = c)
  
  withRh <- ee$ImageCollection(era5$map(function(image){
    vws <- image$select("v_component_of_wind_10m")$rename("vws")
    uws <- image$select("u_component_of_wind_10m")$rename("uws")
    vws <- vws$exp()
    uws <- uws$exp()
    ws <- vws$add(uws)$rename("ws")
    ws <- ws$sqrt()
    dt <- image$select("dewpoint_temperature_2m")$rename("dt")
    t <- image$select("temperature_2m")$rename("t")
    dt <- dt$subtract(273.15)
    t <- t$subtract(273.15)
    ppt <- image$select("total_precipitation_sum")$rename("ppt")
    ppt <- ppt$divide(1000)
    b1 <- dt$multiply(17.67)$divide(dt$add(243))$exp()
    b2 <- t$multiply(17.67)$divide(t$add(243))$exp()
    b3 <- b1$divide(b2)
    b4 <- b3$multiply(100)$int()$rename("rh")
    date <- image$date()
    doy <- date$getRelative("day", "year")
    doyImage <- ee$Image(doy)$
      rename("doy")$
      int()
    month <- date$getRelative("month", "year")
    monthImage <- ee$Image(month)$
      rename("mon")$
      int
    year <- date$get("year")
    yearImage <- ee$Image(year)$
      rename("year")$
      int
    
    
    img <- b4$
      addBands(t)$
      addBands(month)$
      addBands(ppt)$
      addBands(ws)$
      addBands(year)$
      addBands(doyImage)# Appropriate use of clip.
    
    return(img)
      }
    ))
  
  # cast to list 
  preList <- withRh$toList(withRh$size())
  
  # for each image. loop through and extract
  #get sequence lenggth for for loop
  startLoop <- 1
  endLoop <- (endDay - startDay) #* ((as.numeric(sub("-.*","", fireYear))) - (as.numeric(sub("-.*","", imageStart)))) # set to length of start/end day? *multiplied by the time.gap
 
  #greate empty list
  mylist <- list()
  for(i in startLoop:endLoop){ # set to length of start/end day? *multiplied by the time.gap
    # get image based on location in list
    tI <- ee$Image(preList$get(i))
    
    # extract metrics for each image
    mets <- ee_extract(
      x = tI,
      y = fireCentee,
      scale = 10000,
      fun = ee$Reducer$mean(),# mean or sum
      sf = TRUE
    )
    
    # clean metrics by getting lat long and removing geometry
    mets <- mets |> 
      dplyr::mutate(lon = sf::st_coordinates(geometry)[,1],
                    lat = sf::st_coordinates(geometry)[,2]) |> st_drop_geometry()
    
    # input into empty list
    mylist[[i]] <- mets
    i + 1
  }
  
  
  # bind rresults into dataframe
 res <- do.call(rbind, mylist)

 # rename month and year column
 res <- rename(res, mon = constant)
 res <- rename(res, year = constant_1)
 return(res)

}


#' Calculates daily drought code
#'
#' @param res dataframe. Results table from dailyWeather function
#'
#' @return dataframe. with additional drought code (DC) column to existing daily weather dataframe
#' @export
#'
#' @examples
getDC <- function(res){
  
  # take dataframe and using cffdrs package computer DC.
  # start day 120 with default dc_yda value of 15
  # after that every day uses the previous days value
  # for loop 
  #if doy = 120, set first arg to 15. run loop through once
  # if doy is anything other than 120, input output value 
  # from last iteraction as dc_yda value in this one
  
  # loop through each row in each year
  # get year 
  jloop <- list()
  iloop <- list()
  yr <- unique(res$year)
  for(i in yr){
    
    #subset results by year
    res_yr <- res[res$year == i,]
    
    #make sure its ordered by doy
    res_yr <- res_yr[order(res_yr$doy),]
    
    for(j in 1:nrow(res_yr)){
      
      # get row
      row <- res_yr[j,]
      
      if(c("120") %in% row$doy){
        # if it is start of list, return fifteen
        row <- mutate(row, dc_yda = c(15))
        dc <- drought_code(row$dc_yda, row$t, row$rh, row$ppt, row$lat, row$mon)
        # change dc to dc_yda
        row <- mutate(row, dc_yda = dc)
        row <- rename(row, dc = dc_yda)
        # ? maybe dettach row dataframe here?
        # make dc_yda value
        dc_yda <- dc
      }else{
        dc <- drought_code(dc_yda, row$t, row$rh, row$ppt, row$lat, row$mon)
        row <- mutate(row, dc = dc)
        dc_yda <- dc
      }
      
      jloop[[j]] <- row
      
    }
    output <- do.call(rbind, jloop)
    
    iloop[[i]] <- output
    
  }
  dc_output <- do.call(rbind, iloop)
  return(dc_output)
}



#' Calculates daily ffmc
#'
#' @param res 
#'
#' @return dataframe. with additional fine fuel moisture code (FFMC) column to existing daily weather dataframe
#' @export
#'
#' @examples
getFFMC <- function(res){
  
  # take dataframe and using cffdrs package computer DC.
  # start day 120 with default dc_yda value of 15
  # after that every day uses the previous days value
  # for loop 
  #if doy = 120, set first arg to 15. run loop through once
  # if doy is anything other than 120, input output value 
  # from last iteraction as dc_yda value in this one
  
  # loop through each row in each year
  # get year 
  jloop <- list()
  iloop <- list()
  yr <- unique(res$year)
  for(i in yr){
    
    #subset results by year
    res_yr <- res[res$year == i,]
    
    #make sure its ordered by doy
    res_yr <- res_yr[order(res_yr$doy),]
    
    for(j in 1:nrow(res_yr)){
      
      # get row
      row <- res_yr[j,]
      
      if(c("120") %in% row$doy){
        # if it is start of list, return fifteen
        row <- mutate(row, ffmc_yda = c(15))
        ffmc <- fine_fuel_moisture_code(row$ffmc_yda, row$t, row$rh, row$ws, row$ppt)
        # change dc to dc_yda
        row <- mutate(row, ffmc_yda = ffmc)
        row <- rename(row, ffmc = ffmc_yda)
        # ? maybe dettach row dataframe here?
        # make dc_yda value
        ffmc_yda <- ffmc
      }else{
        ffmc <- fine_fuel_moisture_code(ffmc_yda, row$t, row$rh, row$ws, row$ppt)
        row <- mutate(row, ffmc = ffmc)
        ffmc_yda <- ffmc
      }
      
      jloop[[j]] <- row
      
    }
    output <- do.call(rbind, jloop)
    
    iloop[[i]] <- output
    
  }
  ffmc_output <- do.call(rbind, iloop)
  return(ffmc_output)
}


#' Wrapper around daily weather function
#'
#' @param values_df 
#' @param data 
#'
#' @return
#' @export
#'
#' @examples
getDailyWx <- function(values_df, data){
  
  #create empty list
  qlist <- list()
  
  for(i in 1:nrow(values_df)){
    
    data1 <- data[i,]
    id <- data1$id
    
    dw <-  dailyWeather(data1, startDay, endDay)
    
    dw <- mutate(dw, id = rep(id))
    
    qlist[[i]] <-  dw
    i + 1
    
  }
  res <- do.call(rbind, qlist)
  
  return(res)
}

