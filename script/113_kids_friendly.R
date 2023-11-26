#### Kids_friendly ----
# Specify the keywords for kids allowed in multiple languages
keywords_kids_allowed <- c("kids", "children", 
                           "enfants", 
                           "Kinder", 
                           "bambini")

# Specify the negations for kids allowed
negations_kids_allowed <- c(
  "no kids", "no children ", 
  "pas d'enfants", "keine Kinder erlaubt", "bambini non ammessi"
)

# Find keywords in "desc" and turn everything else (character(0)) into NA
data$kids_allowed_matches <- sapply(data$descr, function(text) extract_surrounding_words(text, keywords_kids_allowed))
data$kids_allowed_matches <- sapply(data$kids_allowed_matches, function(matches) ifelse(length(matches) == 0, NA, matches))

# Find negations indicating kids allowed or no kids allowed
data$kids_allowed_new <- sapply(data$kids_allowed_matches, function(matches) {
  if (is.na(matches)) {
    return(NA)
  }
  check_for_negation(matches, negations_kids_allowed)
})

# Set kids_allowed_check to 1 if any of kids allowed or kids_allowed_new is 1
data$kids_allowed_check <- ifelse(data$kids_friendly == 1 | data$kids_allowed_new == 1, 1, NA)

# Set kids_allowed_check to 0 if kids_allowed_new is 0 and kids allowed is NA
data$kids_allowed_check[data$kids_allowed_new == 0 & is.na(data$kids_friendly)] <- 0

# Check if done correctly
kids_subset <- data[c("kids_friendly", "kids_allowed_new", "kids_allowed_check")]
# Print the results
summary(kids_subset)

# Clean up columns
# Save kids_allowed_check into kids_allowed
data$kids_friendly <- data$kids_allowed_check
# Drop other kids_allowed columns
data <- subset(data, select = -c(kids_allowed_matches, kids_allowed_new, kids_allowed_check))


summary(data$kids_friendly)

# Assuming no indication means kids allowed: Turn NAs into 1
data$kids_friendly[is.na(data$kids_friendly)] <- 1
sum(is.na(data$kids_friendly))

# Turning into binary 
data$kids_friendly <- as.factor(data$kids_friendly)
summary(data$kids_friendly)
