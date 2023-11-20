#### Parking -------------------------------------------------------------------
# Specify the keywords for parking in multiple languages
keywords <- c("parking", "parcheggio", "stationnement", "Parken", "Parkplatz", "Parkmöglichkeit", "garage")

# Specify the negations
negations <- c( "no", "non", "not", "nicht", "kein Parkplatz", "keine Garage")

# Find keywords in "desc" and turn everything else (character(0)) into NA
data$parking_matches <- sapply(data$descr, function(text) extract_surrounding_words(text, keywords))
data$parking_matches <- sapply(data$parking_matches, function(matches) ifelse(length(matches) == 0, NA, matches))
#data$parking[is.na(data$parking_matches)] <- NA

# Find negations indicating no parking
data$parking <- sapply(data$parking_matches, function(matches) {
  if (is.na(matches)) {
    return(NA)
  }
  check_for_negation(matches, negations)
})

# Set parking to 1 if any of parking, parking_indoor, or parking_outside is 1
# Merge indoor and outdoor parking 
data$parking_check <- ifelse(data$parking_indoor == 1 | data$parking_outside == 1, 1, NA)
# 
data$parking_check <- ifelse(
  data$parking == 1 | data$parking_check == 1, 1, 
  ifelse(is.na(data$parking) & is.na(data$parking_check), NA, 
         ifelse(data$parking == 0 & is.na(data$parking_check), 0, NA))
)
data$parking_check[data$parking == 0 & is.na(data$parking_check)] <- 0

# Check if done correct
parking_subset <- data[c("parking", "parking_indoor", "parking_outside", "parking_check")]
# Print the results
summary(parking_subset)

# Clean up columns
# Save parking_check into parking
data$parking <- data$parking_check
# Drop other parking columns
data <- subset(data, select = -c(parking_indoor, parking_outside, parking_check, parking_matches))


