####----
parking_translations <- c("Parcheggio", "Parkplatz", "Parkhaus", "Parking")

# Create a dummy variable for the word 'Parking' in different languages
create_parking_dummy <- function(data_frame, column_name) {
  # Create a regex pattern that matches any of the translations, case-insensitive
  parking_pattern <- paste(parking_translations, collapse = "|")

  # ignore.case = TRUE to make the match case-insensitive
  data_frame$parking_dummy <- as.integer(grepl(parking_pattern, data_frame[[column_name]], ignore.case = TRUE))

  return(data_frame)
}

# Apply the function to the 'training_data' data frame
training <- create_parking_dummy(training_data, "descr")

# Sum of found parking words
sum_parking <- training %>%
  summarise(sum_parking = sum(parking_dummy == 1, na.rm = TRUE))

# Check and compare findidngs with exsisting data
selected <- training %>%
  select(descr, parking_dummy, parking_indoor,parking_outside)
