#### Dryer  ----

# Specify the keywords for dryer in multiple languages
keywords_dryer <- c("dryer", "sèche-linge", "Trockner", "asciugatrice")

# Specify the negations for dryer
negations_dryer <- c("no", "non", "not", "nicht", "pas de sèche-linge", "nessun asciugatrice", "kein Trockner")

# Find keywords in "desc" and turn everything else (character(0)) into NA
data$dryer_matches <- sapply(data$descr, function(text) extract_surrounding_words(text, keywords_dryer))
data$dryer_matches <- sapply(data$dryer_matches, function(matches) ifelse(length(matches) == 0, NA, matches))

# Find negations indicating dryer or no dryer
data$dryer_new <- sapply(data$dryer_matches, function(matches) {
  if (is.na(matches)) {
    return(NA)
  }
  check_for_negation(matches, negations_dryer)
})

# Set dryer_check to 1 if any of dryer or dryer_new is 1
data$dryer_check <- ifelse(data$dryer == 1 | data$dryer_new == 1, 1, NA)

# Set dryer_check to 0 if dryer_new is 0 and dryer is NA
data$dryer_check[data$dryer_new == 0 & is.na(data$dryer)] <- 0

# Check if done correctly
dryer_subset <- data[c("dryer", "dryer_new", "dryer_check")]
# Print the results
summary(dryer_subset)

# Clean up columns
# Save dryer_check into dryer
data$dryer <- data$dryer_check
# Drop other dryer columns
data <- subset(data, select = -c(dryer_matches, dryer_new, dryer_check))
