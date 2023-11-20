#### Fireplace ----

# Specify the keywords for fireplace in multiple languages
keywords_fireplace <- c("fireplace", "cheminée", "Kamin", "camino")

# Specify the negations for fireplace
negations_fireplace <- c(
  "no fireplace", "pas de cheminée", "kein Kamin", "no camino"
)

# Find keywords in "desc" and turn everything else (character(0)) into NA
data$fireplace_matches <- sapply(data$descr, function(text) extract_surrounding_words(text, keywords_fireplace))
data$fireplace_matches <- sapply(data$fireplace_matches, function(matches) ifelse(length(matches) == 0, NA, matches))

# Find negations indicating fireplace or no fireplace
data$fireplace_new <- sapply(data$fireplace_matches, function(matches) {
  if (is.na(matches)) {
    return(NA)
  }
  check_for_negation(matches, negations_fireplace)
})

# Set fireplace_check to 1 if any of fireplace or fireplace_new is 1
data$fireplace_check <- ifelse(data$cheminee == 1 | data$fireplace_new == 1, 1, NA)

# Set fireplace_check to 0 if fireplace_new is 0 and fireplace is NA
data$fireplace_check[data$fireplace_new == 0 & is.na(data$cheminee)] <- 0

# Check if done correctly
fireplace_subset <- data[c("cheminee", "fireplace_new", "fireplace_check")]
# Print the results
summary(fireplace_subset)

# Clean up columns
# Save fireplace_check into fireplace
data$cheminee <- data$fireplace_check
# Drop other fireplace columns
data <- subset(data, select = -c(fireplace_matches, fireplace_new, fireplace_check))
