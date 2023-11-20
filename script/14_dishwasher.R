#### Dishwasher ----

# Specify the keywords for dishwasher in multiple languages
keywords_dishwasher <- c("dishwasher", "lave-vaisselle", "Geschirrspüler", "lavastoviglie")

# Specify the negations for dishwasher
negations_dishwasher <- c("no", "non", "not", "nicht", "pas de lave-vaisselle", "nessuna lavastoviglie", "kein Geschirrspüler")

# Find keywords in "desc" and turn everything else (character(0)) into NA
data$dishwasher_matches <- sapply(data$descr, function(text) extract_surrounding_words(text, keywords_dishwasher))
data$dishwasher_matches <- sapply(data$dishwasher_matches, function(matches) ifelse(length(matches) == 0, NA, matches))

# Find negations indicating dishwasher or no dishwasher
data$dishwasher_new <- sapply(data$dishwasher_matches, function(matches) {
  if (is.na(matches)) {
    return(NA)
  }
  check_for_negation(matches, negations_dishwasher)
})

# Set dishwasher_check to 1 if any of dishwasher or dishwasher_new is 1
data$dishwasher_check <- ifelse(data$dishwasher == 1 | data$dishwasher_new == 1, 1, NA)

# Set dishwasher_check to 0 if dishwasher_new is 0 and dishwasher is NA
data$dishwasher_check[data$dishwasher_new == 0 & is.na(data$dishwasher)] <- 0

# Check if done correctly
dishwasher_subset_check <- data[c("dishwasher", "dishwasher_new", "dishwasher_check")]
# Print the results
summary(dishwasher_subset_check)

# Clean up columns
# Save dishwasher_check into dishwasher
data$dishwasher <- data$dishwasher_check
# Drop other dishwasher columns
data <- subset(data, select = -c(dishwasher_matches, dishwasher_new, dishwasher_check))
