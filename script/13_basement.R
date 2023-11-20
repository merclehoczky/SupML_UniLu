#### Basement ----

# Specify the keywords for basement in multiple languages
keywords_basement <- c("basement", "sous-sol", "keller", "cantina")

# Specify the negations for basement
negations_basement <- c("no", "non", "not", "nicht", "pas de sous-sol", "nessun basement", "kein Keller")

# Find keywords in "desc" and turn everything else (character(0)) into NA
data$basement_matches <- sapply(data$descr, function(text) extract_surrounding_words(text, keywords_basement))
data$basement_matches <- sapply(data$basement_matches, function(matches) ifelse(length(matches) == 0, NA, matches))

# Find negations indicating basement or no basement
data$basement_new <- sapply(data$basement_matches, function(matches) {
  if (is.na(matches)) {
    return(NA)
  }
  check_for_negation(matches, negations_basement)
})

# Set basement_check to 1 if any of basement or basement_new is 1
data$basement_check <- ifelse(data$basement == 1 | data$basement_new == 1, 1, NA)
# Set basement_check to 0 if basement_new is 0 and basement is NA
data$basement_check[data$basement_new == 0 & is.na(data$basement)] <- 0

# Check if done correctly
basement_subset <- data[c("basement", "basement_new", "basement_check")]
# Print the results
summary(basement_subset)

# Clean up columns
# Save basement_check into basement
data$basement <- data$basement_check
# Drop other basement columns
data <- subset(data, select = -c(basement_matches, basement_new, basement_check))
