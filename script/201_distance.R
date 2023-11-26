library(httr)
library(jsonlite)
api_key <- rstudioapi::askForPassword()
Sys.setenv(GU_API_KEY = api_key) 

# Find nearest place 
find_nearest <- function(lat, lon, api_key, type) {

  base_url <- "https://maps.googleapis.com/maps/api/place/nearbysearch/json"
  location <- paste(lat, lon, sep = ",")
  
  response <- GET(url = base_url, query = list(
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


# Calculate the distance
calculate_distance <- function(origin, destination, api_key, mode = "walking") {
  base_url <- "https://maps.googleapis.com/maps/api/distancematrix/json"
  
  response <- GET(url = base_url, query = list(
    origins = origin,
    destinations = destination,
    key = api_key,
    mode = mode
  ))
  
  # Parse the response
  data <- fromJSON(rawToChar(response$content))
  data <- data.frame(data)
  # Check if the response status and element status are OK
  if (data$status == "OK") {
    # Extract the distance in meters
    
    dist <- as.character(data['elements']) 
    parsed <- eval(parse(text = dist))
    distance <- parsed[[1]]$distance$value
    #print(origin)
    #print(data)
    return(distance)
  } else {
    # Handle cases where the API response is not OK
    print(paste("API response status not OK:", data$status))
    return(NULL)
  }
}

