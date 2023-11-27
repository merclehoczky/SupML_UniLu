find_nearest <- function(lat, lon, api_key, type) {
  
  base_url <- "https://maps.googleapis.com/maps/api/place/nearbysearch/json"
  location <- paste(lat, lon, sep = ",")
  
  response <- GET(url = base_url, query = list(
    keyword = "lake",
    location = location,
    rankby = "distance",
    
    type = type,  
    
    key = api_key
  ))
  
  # Parse the response
  data <- fromJSON(rawToChar(response$content))
  resp <-rawToChar(response$content)
  # Check if results are available
  if(length(data$results) > 0) {
    first_result <- data$results[1,]
    location_lat <- first_result$geometry$location$lat
    location_lng <- first_result$geometry$location$lng
    return(c(location_lat, location_lng))
  } else {
    print("No results found")
    return(NULL)
  }
}


for(i in 1:nrow(data)) {
  if(is.na(data$dist_to_lake[i])) {
    origin_lat <- data$lat[i]
    origin_lng <- data$lon[i]
    
    # Find the nearest lake
    location <- find_nearest(origin_lat, origin_lng, api_key, "lake")
    
    # Check if a lake was found
    if(!is.null(location)) {
      # Format for Distance Matrix API
      origin <- paste(origin_lat, origin_lng, sep = ",")
      destination <- paste(location[1], location[2], sep = ",")
      
      # Calculate distance
      distance <- calculate_distance(origin, destination, api_key)
      
      # Update 'dist_to_lake' in data if distance is not null
      if(!is.null(distance)) {
        data$dist_to_lake[i] <- distance
      } else {
        # Print the row index if distance calculation failed
        print(paste("Missed distance for row:", i))
      }
    } else {
      # Print the row index if no nearby lake is found
      print(paste("Missed location for row:", i))
    }
  }
 print(i)
}


sum(is.na(data$dist_to_lake))


#### In case there're still NA - imputation mice
imputed_lake <- mice(data[, c("dist_to_lake", "Micro_rating_ServicesAndNature")], method = 'pmm', m = 5, maxit = 5)
completed_lake <- complete(imputed_lake)
# Check results 
summary(data$dist_to_lake)
summary(completed_lake$dist_to_lake)
data$dist_to_lake <- completed_lake$dist_to_lake

