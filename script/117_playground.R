#### Playground #### ---------
#### Playground ----

# Specify the keywords for playground in multiple languages
keywords_playground <- c("playground", "aire de jeux", "Spielplatz", "parco giochi")

# Specify the negations for playground
negations_playground <- c("no", "non", "not", "pas de aire de jeux", "nessun parco giochi", "kein Spielplatz")

# Find keywords in "desc" and turn everything else (character(0)) into NA
data$playground_matches <- sapply(data$descr, function(text) extract_surrounding_words(text, keywords_playground))
data$playground_matches <- sapply(data$playground_matches, function(matches) ifelse(length(matches) == 0, NA, matches))

# Find negations indicating playground or no playground
data$playground_new <- sapply(data$playground_matches, function(matches) {
  if (is.na(matches)) {
    return(NA)
  }
  check_for_negation(matches, negations_playground)
})

# Set playground_check to 1 if any of playground or playground_new is 1
data$playground_check <- ifelse(data$playground == 1 | data$playground_new == 1, 1, NA)

# Set playground_check to 0 if playground_new is 0 and playground is NA
data$playground_check[data$playground_new == 0 & is.na(data$playground)] <- 0

# Check if done correctly
playground_subset <- data[c("playground", "playground_new", "playground_check")]
# Print the results
summary(playground_subset)

# Clean up columns
# Save playground_check into playground
data$playground <- data$playground_check
# Drop other playground columns
data <- subset(data, select = -c(playground_matches, playground_new, playground_check))

summary(data$playground)

# Assuming no indication is negation: Turn NAs into 0
data$playground[is.na(data$playground)] <- 0
sum(is.na(data$playground))

# Turning into binary 
data$playground <- as.factor(data$playground)
summary(data$playground)
