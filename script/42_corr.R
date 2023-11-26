##### Correlations ######## 
#### Corr plots ------------------------------------------
library(dplyr)
library(ggplot2)
library(corrplot)
#df <- 

# Numerical variables correlation ---------------------------------------

summary(df %>% select_if(is.numeric))

# Numerical relevant columns
vars_num_names <- names(df)[sapply(df, is.numeric)]
# 
# numer <- c("rent_full", "Micro_rating", "Micro_rating_NoiseAndEmission", 
#            "Micro_rating_Accessibility", "Micro_rating_DistrictAndArea", 
#            "Micro_rating_SunAndView", "Micro_rating_ServicesAndNature", "Noise_max", "apoth_pix_count_km2", 
#            "dist_to_4G", "dist_to_5G", "dist_to_train_stat", "restaur_pix_count_km2", 
#            "superm_pix_count_km2", "dist_to_river")

cor_matrix <- cor(df[vars_num_names], use = "complete.obs")
corrplot(cor_matrix, method = "circle")

# Set the size of the plotting device
png("Correlation_plot_numeric.png", width = 1600, height = 1600)

# Calculate the correlation matrix

# Plot the correlation matrix
corrplot.mixed(cor_matrix, 
               order = "hclust", 
               upper = "circle", 
               lower = "number", 
               tl.pos = "lt", 
               tl.col = "black", 
               lower.col = "black",
               number.cex = 1,
               main = "Correlation Matrix for numerical variables",
               mar = c(1,1,0.5,0.5)
)

# Close the plotting device
dev.off()

# List and remove correlations above abs(0.95) -------
# Set a threshold for correlation
threshold <- abs(0.95)

# Set diagonal elements to 0
diag(cor_matrix) <- 0

# Find pairs of variables with correlation above the threshold
high_corr_pairs <- which(upper.tri(cor_matrix, diag = TRUE) & abs(cor_matrix) > threshold, arr.ind = TRUE)

# List the pairs
high_corr_vars <- data.frame(Variable1 = rownames(cor_matrix)[high_corr_pairs[, 1]],
                             Variable2 = colnames(cor_matrix)[high_corr_pairs[, 2]],
                             Correlation = cor_matrix[high_corr_pairs[, 1], high_corr_pairs[, 2]])

# Print or inspect the high correlated variables
print(high_corr_vars)

# Remove one variable from each pair
variables_to_remove <- unique(c( high_corr_vars$Variable2))

# Remove the variables from the data frame
df_filtered <- df[, !names(df) %in% variables_to_remove]



# Categorical variables correlation --------------------------------------------
# Save binary variables

vars_binary_names <- names(df)[sapply(df, function(x) is.factor(x) && length(levels(x)) == 2)]
vars_cat_names <- names(df)[sapply(df, is.factor) | sapply(df, is.logical) | (names(df) %in% vars_binary_names)]

# Set the size of the plotting device
png("Correlation_plot_categorical.png", width = 800, height = 800)

# Create a contingency table for the binary variables
contingency_table <- table(df[, vars_cat_names, drop = FALSE])

# Perform chi-squared test
chi2_result <- chisq.test(contingency_table)

# Print the chi-squared test result
print(chi2_result)

# Plot the chi-squared correlation matrix
corrplot(chi2_result$stdres, method = "circle", type = "upper", order = "hclust", tl.pos = "lt", tl.col = "black")

# Close the plotting device

dev.off()