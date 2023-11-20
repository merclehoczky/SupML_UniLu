#### Balcony ----

# Specify the keywords_balcony for balcony in multiple languages
keywords_balcony <- c("balcony", "balcone", "balcon", "Balkon")

# Specify the negations_balcony
negations_balcony <- c("no", "non", "not", "nicht", "pas de balcon", "nessun balcone", "kein Balkon")

# Find keywords_balcony in "desc" and turn everything else (character(0)) into NA
data$balcony_matches <- sapply(data$descr, function(text) extract_surrounding_words(text, keywords_balcony))
data$balcony_matches <- sapply(data$balcony_matches, function(matches) ifelse(length(matches) == 0, NA, matches))

# Find negations_balcony indicating balcony or no balcony
data$balcony_new <- sapply(data$balcony_matches, function(matches) {
  if (is.na(matches)) {
    return(NA)
  }
  check_for_negation(matches, negations_balcony)
})

# Set balcony_check to 1 if any of balcony or balcony_new is 1
data$balcony_check <- ifelse(data$balcony == 1 | data$balcony_new == 1, 1, NA)

# Check if done correct
balcony_subset <- data[c("balcony", "balcony_new", "balcony_check")]
# Print the results
summary(balcony_subset)

# Clean up columns
# Save balcony_check into balcony
data$balcony <- data$balcony_check
# Drop other balcony columns
data <- subset(data, select = -c(balcony_matches, balcony_new, balcony_check))
