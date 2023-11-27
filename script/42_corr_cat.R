library(dplyr)
library(corrplot)

# Assuming df is your data frame with 72000 rows and 20 variables

# Identify factorial variables
factorial_columns <- names(df)[sapply(df, is.factor)]
factorial_variables <- df[factorial_columns]

# Create an empty data frame to store results
results_df <- data.frame(
  Variable1 = character(),
  Variable2 = character(),
  P_Value = numeric(),
  Is_Significant = logical(),
  stringsAsFactors = FALSE
)

# Perform chi-squared tests and store results
for (i in 1:(length(factorial_columns) - 1)) {
  for (j in (i + 1):length(factorial_columns)) {
    # Create a contingency table
    contingency_table <- table(factorial_variables[[i]], factorial_variables[[j]])
    
    # Perform chi-squared test
    result <- chisq.test(contingency_table, correct = FALSE)
    
    # Store results in the data frame
    results_df <- rbind(results_df, list(
      Variable1 = factorial_columns[i],
      Variable2 = factorial_columns[j],
      P_Value = result$p.value,
      Is_Significant = result$p.value < 0.10,  # Adjust the significance level as needed
      stringsAsFactors = FALSE
    ))
  }
}


# View the results data frame
print(results_df)
