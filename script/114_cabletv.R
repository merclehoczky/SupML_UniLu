#### Cable TV ----

# Specify the keywords for cable TV in multiple languages
keywords_cable_tv <- c("cable tv", "télévision par câble", "Kabel-TV", "tv via cavo")

# Specify the negations for cable TV
negations_cable_tv <- c(
  "no cable tv", "pas de télévision par câble", "kein Kabel-TV", "no tv via cavo"
)

# Find keywords in "desc" and turn everything else (character(0)) into NA
data$cable_tv_matches <- sapply(data$descr, function(text) extract_surrounding_words(text, keywords_cable_tv))
data$cable_tv_matches <- sapply(data$cable_tv_matches, function(matches) ifelse(length(matches) == 0, NA, matches))

# Find negations indicating cable TV or no cable TV
data$cable_tv_new <- sapply(data$cable_tv_matches, function(matches) {
  if (is.na(matches)) {
    return(NA)
  }
  check_for_negation(matches, negations_cable_tv)
})

# Set cable_tv_check to 1 if any of cable TV or cable_tv_new is 1
data$cable_tv_check <- ifelse(data$cabletv == 1 | data$cable_tv_new == 1, 1, NA)

# Set cable_tv_check to 0 if cable_tv_new is 0 and cable TV is NA
data$cable_tv_check[data$cable_tv_new == 0 & is.na(data$cabletv)] <- 0

# Check if done correctly
cabletv_subset <- data[c("cabletv", "cable_tv_new", "cable_tv_check")]
# Print the results
summary(cabletv_subset)

# Clean up columns
# Save cable_tv_check into cable_tv
data$cabletv <- data$cable_tv_check
# Drop other cable_tv columns
data <- subset(data, select = -c(cable_tv_matches, cable_tv_new, cable_tv_check))



summary(data$cabletv)

# Assuming no indication means available: Turn NAs into 1
data$cabletv[is.na(data$cabletv)] <- 1
sum(is.na(data$cabletv))

# Turning into binary 
data$cabletv <- as.factor(data$cabletv)
summary(data$cabletv)
