#### Pets ----
# Specify the keywords for pets in multiple languages
keywords_pets <- c("pets", "animaux", "Haustiere", "animali",
                   "cats", "dogs",
                   "gatti", "cani",
                   "chats", "chiens",
                   "Katzen", "Hunde")

# Specify the negations for pets
negations_pets <- c(
  "non", "not", "nicht", 
  "no pets", 
  "no",
  "pas d'animaux", "no animaux", 
  "keine Haustiere", 
  "nessun animale",
  "no animali"
)


# Modify function to extract more surrounding words
extract_surrounding_words_more <- function(text, keywords) {
  if (is.na(text)) {
    return(NA)
  }
  # Use the (?i) flag for case-insensitive matching
  regex_pattern <- paste0("(?i)(\\S+\\s+){0,4}(", paste0(keywords, collapse = "|"), ")(\\s+\\S+){0,6}")
  matches <- str_extract_all(text, regex_pattern)
  matches <- unlist(matches)
  matches[matches == "character(0)"] <- NA
  return(unlist(matches))
}

# Find keywords in "desc" and turn everything else (character(0)) into NA
data$pets_matches <- sapply(data$descr, function(text) extract_surrounding_words_more(text, keywords_pets))
data$pets_matches <- sapply(data$pets_matches, function(matches) ifelse(length(matches) == 0, NA, matches))

# Oven is logical, convert to numeric
data$pets <- as.numeric(data$pets)

# Find negations indicating pets or no pets
data$pets_new <- sapply(data$pets_matches, function(matches) {
  if (is.na(matches)) {
    return(NA)
  }
  check_for_negation(matches, negations_pets)
})

# Set pets_check to 1 if any of pets or pets_new is 1
data$pets_check <- ifelse(data$pets == 1 | data$pets_new == 1, 1, NA)

# Set pets_check to 0 if pets_new is 0 and pets is NA
data$pets_check[data$pets_new == 0 & is.na(data$pets)] <- 0

# Check if done correctly
pets_subset <- data[c("pets", "pets_new", "pets_check")]
# Print the results
summary(pets_subset)

# Clean up columns
# Save pets_check into pets
data$pets <- data$pets_check
# Drop other pets columns
data <- subset(data, select = -c(pets_matches, pets_new, pets_check))

summary(data$pets)

# Cannot determine default pets_allowed status
# data$pets[is.na(data$pets)] <- 0
sum(is.na(data$pets))

# Turning into binary 
data$pets <- as.factor(data$pets)
summary(data$pets)
