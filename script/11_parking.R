library(stringr)



# Function to extract surrounding words
extract_surrounding_words <- function(text, keywords) {
  if (is.na(text)) {
    return(NA)
  }
  # Use the (?i) flag for case-insensitive matching
  regex_pattern <- paste0("(?i)(\\S+\\s+){0,2}(", paste0(keywords, collapse = "|"), ")(\\s+\\S+){0,2}")
  matches <- str_extract_all(text, regex_pattern)
  matches <- unlist(matches)
  matches[matches == "character(0)"] <- NA
  return(unlist(matches))
}



# Function to check for negations
check_for_negation <- function(matches, negations) {
  if (is.na(matches) || any(sapply(negations, function(neg) any(grepl(paste0("\\b", neg, "\\b"), matches, ignore.case = TRUE))))) {
    return(0)
  } else {
    return(1)
  }
}


# Specify the keywords for parking in multiple languages
keywords <- c("parking", "parcheggio", "stationnement", "Parken", "Parkplatz", "Parkmöglichkeit", "garage")

# Specify the negations
negations <- c( "no", "non", "not", "nicht", "kein Parkplatz", "keine Garage")

# Apply the function to the 'descr' column
training_data$parking_matches <- sapply(training_data$descr, function(text) extract_surrounding_words(text, keywords))
training_data$parking_matches <- sapply(training_data$parking_matches, function(matches) ifelse(length(matches) == 0, NA, matches))
training_data$parking <- sapply(training_data$parking_matches, function(matches) check_for_negation(matches, negations))
# Print the results
print(training_data$parking_matches)
