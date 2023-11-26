#### Minergie ----

# Specify the keywords for minergie in multiple languages
keywords_minergie <- c("minergie")

# Specify the negations for minergie
negations_minergie <- c(
  "not minergie", "non minergie", "nicht minergie", "kein minergie", "no minergie"
)

# Find keywords in "desc" and turn everything else (character(0)) into NA
data$minergie_matches <- sapply(data$descr, function(text) extract_surrounding_words(text, keywords_minergie))
data$minergie_matches <- sapply(data$minergie_matches, function(matches) ifelse(length(matches) == 0, NA, matches))

# Find negations indicating minergie or not minergie
data$minergie_new <- sapply(data$minergie_matches, function(matches) {
  if (is.na(matches)) {
    return(NA)
  }
  check_for_negation(matches, negations_minergie)
})

# Minergie is logical, set it to numerical
data$minergie <- as.numeric(data$minergie)

# Set minergie_check to 1 if minergie or minergie_new is 1
data$minergie_check <- ifelse(data$minergie == 1 | data$minergie_new == 1, 1, NA)

# Set minergie_check to 0 if minergie_new is 0 and minergie is NA
data$minergie_check[data$minergie_new == 0 & is.na(data$minergie)] <- 0

# Check if done correctly
minergie_subset <- data[c("minergie", "minergie_new", "minergie_check")]
# Print the results
summary(minergie_subset)

# Clean up columns
# Save minergie_check into minergie
data$minergie <- data$minergie_check
# Drop other minergie columns
data <- subset(data, select = -c(minergie_matches, minergie_new, minergie_check))

summary(data$minergie)

# Assuming no indication is negation: Turn NAs into 0
data$minergie[is.na(data$minergie)] <- 0
sum(is.na(data$minergie))

# Turning into binary 
data$minergie <- as.factor(data$minergie)
summary(data$minergie)
