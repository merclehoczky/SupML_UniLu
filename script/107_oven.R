#### Oven ----
# Specify the keywords for oven in multiple languages
keywords_oven <- c("oven", "four", "Ofen", "forno")

# Specify the negations for oven
negations_oven <- c(
  "no", "non", "not", "nicht", 
  "without oven", "no stove", "no cooking appliance",
  "pas de four", "sans four", 
  "nessun forno", 
  "kein Ofen"
)

# Find keywords in "desc" and turn everything else (character(0)) into NA
data$oven_matches <- sapply(data$descr, function(text) extract_surrounding_words(text, keywords_oven))
data$oven_matches <- sapply(data$oven_matches, function(matches) ifelse(length(matches) == 0, NA, matches))

# Oven is logical, convert to numeric
data$oven <- as.numeric(data$oven)
# Find negations indicating oven or no oven
data$oven_new <- sapply(data$oven_matches, function(matches) {
  if (is.na(matches)) {
    return(NA)
  }
  check_for_negation(matches, negations_oven)
})

# Set oven_check to 1 if any of oven or oven_new is 1
data$oven_check <- ifelse(data$oven == 1 | data$oven_new == 1, 1, NA)

# Set oven_check to 0 if oven_new is 0 and oven is NA
data$oven_check[data$oven_new == 0 & is.na(data$oven)] <- 0

# Check if done correctly
oven_subset <- data[c("oven", "oven_new", "oven_check")]
# Print the results
summary(oven_subset)

# Clean up columns
# Save oven_check into oven
data$oven <- data$oven_check
# Drop other oven columns
data <- subset(data, select = -c(oven_matches, oven_new, oven_check))


summary(data$oven)

# Assuming ovens are present unless indicated otherwise: Turn NAs into 1
data$oven[is.na(data$oven)] <- 1
sum(is.na(data$oven))

# Turning into binary 
data$oven <- as.factor(data$oven)
summary(data$oven)
