library(stringr)

data <- training_data
# Creating function to extract surrounding words
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

# Creating function to check for negations
check_for_negation <- function(matches, negations) {
  if (is.na(matches) || any(sapply(negations, function(neg) any(grepl(paste0("\\b", neg, "\\b"), matches, ignore.case = TRUE))))) {
    return(0)
  } else {
    return(1)
  }
}