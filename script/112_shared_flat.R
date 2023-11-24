#### Shared_flat ----
# Specify the keywords for shared flat in multiple languages
keywords_shared_flat <- c("shared flat", "colocation", "WG", "Wohngemeinschaft", "appartamento condiviso")

# Specify the negations for shared flat
negations_shared_flat <- c( "non", "not",
  "no shared flat", "no roommates",  "not shared flat", 
  "nicht", "no WG", "keine WG",
  "pas de colocation",  
  "no appartamento condiviso", "senza coinquilini"
)

# Find keywords in "desc" and turn everything else (character(0)) into NA
data$shared_flat_matches <- sapply(data$descr, function(text) extract_surrounding_words(text, keywords_shared_flat))
data$shared_flat_matches <- sapply(data$shared_flat_matches, function(matches) ifelse(length(matches) == 0, NA, matches))

# Shared_flat is logical, turn it into numeric
data$shared_flat <- as.numeric(data$shared_flat)

# Find negations indicating shared flat or no shared flat
data$shared_flat_new <- sapply(data$shared_flat_matches, function(matches) {
  if (is.na(matches)) {
    return(NA)
  }
  check_for_negation(matches, negations_shared_flat)
})

# Set shared_flat_check to 1 if any of shared flat or shared_flat_new is 1
data$shared_flat_check <- ifelse(data$shared_flat == 1 | data$shared_flat_new == 1, 1, NA)

# Set shared_flat_check to 0 if shared_flat_new is 0 and shared flat is NA
data$shared_flat_check[data$shared_flat_new == 0 & is.na(data$shared_flat)] <- 0

# Check if done correctly
shared_flat_subset <- data[c("shared_flat", "shared_flat_new", "shared_flat_check")]
# Print the results
summary(shared_flat_subset)

# Clean up columns
# Save shared_flat_check into shared_flat
data$shared_flat <- data$shared_flat_check
# Drop other shared_flat columns
data <- subset(data, select = -c(shared_flat_matches, shared_flat_new, shared_flat_check))

summary(data$shared_flat)

# Assuming no indication is negation: Turn NAs into 0
data$shared_flat[is.na(data$shared_flat)] <- 0
sum(is.na(data$shared_flat))

# Turning into binary 
data$shared_flat <- as.factor(data$shared_flat)
summary(data$shared_flat)
