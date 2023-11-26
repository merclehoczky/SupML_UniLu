#### Wheelchair/Accessibility ----

# Specify the keywords for wheelchair/accessibility in multiple languages
keywords_wheelchair <- c("wheelchair", "accessibility",  
                         "Rollstuhl", "barrierefrei", "rollstuhlgerecht", "rollstuhlgängig", "Rollstuhlfahrerfreundlich",
                         "accessibilité", "fauteuil roulant")

# Specify the negations for wheelchair/accessibility
negations_wheelchair <- c("no", "non", "not", "nicht", 
                          "pas de fauteuil roulant", "nessuna accessibilità", "nicht barrierefrei")

# Find keywords in "desc" and turn everything else (character(0)) into NA
data$wheelchair_matches <- sapply(data$descr, function(text) extract_surrounding_words(text, keywords_wheelchair))
data$wheelchair_matches <- sapply(data$wheelchair_matches, function(matches) ifelse(length(matches) == 0, NA, matches))

# Find negations indicating wheelchair/accessibility or no wheelchair/accessibility
data$wheelchair_new <- sapply(data$wheelchair_matches, function(matches) {
  if (is.na(matches)) {
    return(NA)
  }
  check_for_negation(matches, negations_wheelchair)
})

# Set wheelchair_check to 1 if any of wheelchair/accessibility or wheelchair_new is 1
data$wheelchair_check <- ifelse(data$wheelchair == 1 | data$wheelchair_new == 1, 1, NA)

# Set wheelchair_check to 0 if wheelchair_new is 0 and wheelchair is NA
data$wheelchair_check[data$wheelchair_new == 0 & is.na(data$wheelchair)] <- 0

# Check if done correctly
wheelchair_subset <- data[c("wheelchair", "wheelchair_new", "wheelchair_check")]
# Print the results
summary(wheelchair_subset)

# Clean up columns
# Save wheelchair_check into wheelchair
data$wheelchair <- data$wheelchair_check
# Drop other wheelchair columns
data <- subset(data, select = -c(wheelchair_matches, wheelchair_new, wheelchair_check))

summary(data$wheelchair)

# Assuming no indication is negation: Turn NAs into 0
data$wheelchair[is.na(data$wheelchair)] <- 0
sum(is.na(data$wheelchair))

# Turning into binary 
data$wheelchair <- as.factor(data$wheelchair)
summary(data$wheelchair)
