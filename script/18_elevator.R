#### Elevator ----
# Specify the keywords for elevator in multiple languages
keywords_elevator <- c("elevator", "ascenseur", "Aufzug", "ascensore")

# Specify the negations for elevator
negations_elevator <- c(
  "no elevator", "non", "not", "nicht", "pas d'ascenseur", "nessun ascensore", "kein Aufzug",
  "without elevator", "no lift", "sin ascensor", "kein Fahrstuhl"
)

# Find keywords in "desc" and turn everything else (character(0)) into NA
data$elevator_matches <- sapply(data$descr, function(text) extract_surrounding_words(text, keywords_elevator))
data$elevator_matches <- sapply(data$elevator_matches, function(matches) ifelse(length(matches) == 0, NA, matches))

# Find negations indicating elevator or no elevator
data$elevator_new <- sapply(data$elevator_matches, function(matches) {
  if (is.na(matches)) {
    return(NA)
  }
  check_for_negation(matches, negations_elevator)
})

# Set elevator_check to 1 if any of elevator or elevator_new is 1
data$elevator_check <- ifelse(data$elevator == 1 | data$elevator_new == 1, 1, NA)

# Set elevator_check to 0 if elevator_new is 0 and elevator is NA
data$elevator_check[data$elevator_new == 0 & is.na(data$elevator)] <- 0

# Check if done correctly
elevator_subset <- data[c("elevator", "elevator_new", "elevator_check")]
# Print the results
summary(elevator_subset)

# Clean up columns
# Save elevator_check into elevator
data$elevator <- data$elevator_check
# Drop other elevator columns
data <- subset(data, select = -c(elevator_matches, elevator_new, elevator_check))
