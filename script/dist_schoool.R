
# Loop through rows where 'dist_to_school_1' is NA
for(i in 1:nrow(data)) {
  if(is.na(data$dist_to_school_1[i])) {
    origin_lat <- data$lat[i]
    origin_lng <- data$lon[i]
    
    # Find the nearest school
    school_location <- find_nearest(origin_lat, origin_lng, api_key, "primary_school")
    
    # Check if a school was found
    if(!is.null(school_location)) {
      # Format for Distance Matrix API
      origin <- paste(origin_lat, origin_lng, sep = ",")
      destination <- paste(school_location[1], school_location[2], sep = ",")
      
      # Calculate distance
      distance <- calculate_distance(origin, destination, api_key)
      
      # Update 'dist_to_school_1' in data
      if(!is.null(distance)) {
        data$dist_to_school_1[i] <- distance
      }
    }
  }
}
sum(is.na(data$dist_to_school_1))


#### In case there're still NA - imputation mice
imputed_school <- mice(data[, c("dist_to_school_1", "Micro_rating_ServicesAndNature")], method = 'pmm', m = 5, maxit = 5)
completed_school <- complete(imputed_school)
# Check results 
summary(data$dist_to_school_1)
summary(completed_school$dist_to_school_1)
data$dist_to_school_1 <- completed_school$dist_to_school_1