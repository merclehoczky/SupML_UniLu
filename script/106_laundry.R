#### Laundry ----

# Specify the keywords for laundry in multiple languages
keywords_laundry <- c("laundry", "washing machine",
                      "buanderie", "machine à laver", "lave-linge",
                      "Waschküche", "Waschmaschine", "Waschautomat", 
                      "lavanderia", "lavatrice")

# Specify the negations for laundry
negations_laundry <- c("no", "non", "not", "nicht", 
                       "no laundromat", "no laundry",  "no washing machine",
                       "pas de buanderie", "pas de machine à laver", "sans buanderie",
                       "nessuna lavanderia", "senza lavatrice",
                       "keine Waschküche","keine Waschmaschine"
                       )


# Find keywords in "desc" and turn everything else (character(0)) into NA
data$laundry_matches <- sapply(data$descr, function(text) extract_surrounding_words(text, keywords_laundry))
data$laundry_matches <- sapply(data$laundry_matches, function(matches) ifelse(length(matches) == 0, NA, matches))

# Find negations indicating laundry or no laundry
data$laundry_new <- sapply(data$laundry_matches, function(matches) {
  if (is.na(matches)) {
    return(NA)
  }
  check_for_negation(matches, negations_laundry)
})

# Set laundry_check to 1 if any of laundry or laundry_new is 1
data$laundry_check <- ifelse(data$laundry == 1 | data$laundry_new == 1, 1, NA)

# Set laundry_check to 0 if laundry_new is 0 and laundry is NA
data$laundry_check[data$laundry_new == 0 & is.na(data$laundry)] <- 0

# Check if done correctly
laundry_subset <- data[c("laundry", "laundry_new", "laundry_check")]
# Print the results
summary(laundry_subset)

# Clean up columns
# Save laundry_check into laundry
data$laundry <- data$laundry_check
# Drop other laundry columns
data <- subset(data, select = -c(laundry_matches, laundry_new, laundry_check))


summary(data$laundry)

# Assuming no indication is negation: Turn NAs into 0
data$laundry[is.na(data$laundry)] <- 0
sum(is.na(data$laundry))

# Turning into binary 
data$laundry <- as.factor(data$laundry)
summary(data$laundry)
