#### Furnished ----
# Specify the keywords for furnished in multiple languages
keywords_furnished <- c("furnished", "unfurnished", "meublé", "möbliert", "arredato")

# Specify the negations for furnished
negations_furnished <- c(
  "not furnished", "non", "not", "nicht", "pas meublé", "non meublé", "nicht möbliert", "non arredato",
  "unfurnished", "sin amueblar", "not möbliert"
)

# Find keywords in "desc" and turn everything else (character(0)) into NA
data$furnished_matches <- sapply(data$descr, function(text) extract_surrounding_words(text, keywords_furnished))
data$furnished_matches <- sapply(data$furnished_matches, function(matches) ifelse(length(matches) == 0, NA, matches))

# Find negations indicating furnished or not furnished
data$furnished_new <- sapply(data$furnished_matches, function(matches) {
  if (is.na(matches)) {
    return(NA)
  }
  check_for_negation(matches, negations_furnished)
})

# Set furnished_check to 1 if any of furnished or furnished_new is 1
data$furnished_check <- ifelse(data$furnished == 1 | data$furnished_new == 1, 1, NA)

# Set furnished_check to 0 if furnished_new is 0 and furnished is NA
data$furnished_check[data$furnished_new == 0 & is.na(data$furnished)] <- 0

# Check if done correctly
furnished_subset <- data[c("furnished", "furnished_new", "furnished_check")]
# Print the results
summary(furnished_subset)

# Clean up columns
# Save furnished_check into furnished
data$furnished <- data$furnished_check
# Drop other furnished columns
data <- subset(data, select = -c(furnished_matches, furnished_new, furnished_check))

summary(data$furnished)

# Assuming no indication is negation: Turn NAs into 0
data$furnished[is.na(data$furnished)] <- 0
sum(is.na(data$furnished))

# Turning into binary 
data$furnished <- as.factor(data$furnished)
summary(data$furnished)
