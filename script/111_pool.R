#### Pool ----
# Specify the keywords for pool in multiple languages
keywords_pool <- c("pool", "piscine", "Schwimmbad", "piscina")

# Specify the negations for pool
negations_pool <- c(
  "non", "not", "nicht",
  "no pool",  
  "pas de piscine", 
  "no piscina", "nessuna piscina",
  "kein Schwimmbad"
)

# Pool is logical, shange it into numeric
data$pool <- as.numeric(data$pool)
# Find keywords in "desc" and turn everything else (character(0)) into NA
data$pool_matches <- sapply(data$descr, function(text) extract_surrounding_words(text, keywords_pool))
data$pool_matches <- sapply(data$pool_matches, function(matches) ifelse(length(matches) == 0, NA, matches))

# Find negations indicating pool or no pool
data$pool_new <- sapply(data$pool_matches, function(matches) {
  if (is.na(matches)) {
    return(NA)
  }
  check_for_negation(matches, negations_pool)
})

# Set pool_check to 1 if any of pool or pool_new is 1
data$pool_check <- ifelse(data$pool == 1 | data$pool_new == 1, 1, NA)

# Set pool_check to 0 if pool_new is 0 and pool is NA
data$pool_check[data$pool_new == 0 & is.na(data$pool)] <- 0

# Check if done correctly
pool_subset<- data[c("pool", "pool_new", "pool_check")]
# Print the results
summary(pool_subset)

# Clean up columns
# Save pool_check into pool
data$pool <- data$pool_check
# Drop other pool columns
data <- subset(data, select = -c(pool_matches, pool_new, pool_check))
