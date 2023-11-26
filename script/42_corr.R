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
png("Correlation_plot_numeric.png", width = 800, height = 800)

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


# Binary variables correlation --------------------------------------------
# Save binary variables

vars_binary_names <- names(df)[sapply(df, function(x) is.factor(x) && length(levels(x)) == 2)]

vars_cat_names <- names(df)[sapply(df, is.factor)]
vars_cat_names <- names(df)[sapply(df, is.logical)]

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