# rgee weather analysis tools

cleanDataDefol <- function(defol, nondefol, defol.info){
  
  data <- rbind(defol, nondefol)
  
  data <- data |> 
    rename(Fire_ID = fire_name)
  
 data1 <- defol.info |> 
    left_join(data, by = c("Fire_ID", "defoliated"))
 
 data1 <- tibble::as_tibble(data1)
 
 return(data1)
  
}



getCentroid <- function(data){
  
  data <- st_make_valid(data)
  
  data <- mutate(data, centroid = sf::st_centroid(data$geometry))
  
  #drop pre-existing geometry 
  data <- st_drop_geometry(data)
  
  return(data)
}


getBuffer <- function(data){
  data <- st_make_valid(data)
  
  data <- mutate(data, buffer = sf::st_buffer(data$geometry, 10))
  
  #drop pre-existing geometry 
  data <- st_drop_geometry(data)
  
  return(data)
  
}


# need to do for both defoliated/non-defoliated for control, use time frame for defol for non-defol
timeFrame <- function(data){
   if(c("1") %in% data$defoliated){
    data1 <- data |> filter(defoliated == "1")
    year <- data1 |> select(c(Fire_Year)) |> st_drop_geometry() |> as.numeric()
    tsd <- data1 |> select(c(tsd)) |>  st_drop_geometry() |> as.numeric()
    time.gap <- (year - tsd) 
    time.gap <- lubridate::ymd(time.gap, truncated = 2L) |> as.character() |> as.data.frame()
    year <- lubridate::ymd(year, truncated = 2L) |> as.character() |> as.data.frame()
    res <- cbind(time.gap, year)
    colnames(res) <- c("time.gap", "year")
  }
  
  return(res)
}

