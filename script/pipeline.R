library(readr)
library(dplyr)

library(rstudioapi)
library(outliers)
# Getting the path of your current open file
current_path = rstudioapi::getActiveDocumentContext()$path 
setwd(dirname(current_path ))
print( getwd() )

# Upload data
path_train <- "../data/after_google.csv"
path_test <- "../data/test_after_all.csv"
training_data <- read_csv(path_train)
x_test <- read_csv(path_test)

library(readr)
library(dplyr)
library(rstudioapi)
library(caret)
library(randomForest)
library(glmnet)
library(corrplot)
library(mice)
library(Metrics)
library(xgboost)


# mining ------------------------------------------------------------------------------------------

# Add data to temporary variable -----------------------------------------

data <-  training_data


final <- data
# Remove unusual, outliers-----------------------------------------------------

final <- final %>% 
  filter(area > 20, area < 381) # 56801

# Plot rent_full against area
plot(final$area, final$rent_full, main="Rental Price vs Area", xlab="Area", ylab="Rental price", pch=19, col="blue")

# Outliers
Q1 <- quantile(final$rent_full, 0.25)
Q3 <- quantile(final$rent_full, 0.75)
IQR <- Q3 - Q1

lower_bound <- Q1 - 1.5 * IQR
upper_bound <- Q3 + 1.5 * IQR

final <- final %>% filter(rent_full > lower_bound, final$rent_full < upper_bound) #54328

#final <- final[-outliers, ] #56287
summary(final$rent_full)
# Remove to old buildings
final <- final %>% 
  filter(year_built > 1800 | is.na(year_built)) #56053



#################### Imputation ----------------------------------------------------
data <- final
################## is_new
# Column is_new building
data <- data %>%
  mutate(is_new = ifelse(year_built >= 2017 & year_built <= 2021, 1, 0))
data$is_new[is.na(data$is_new)] <- 0
sum(is.na(data$is_new))
# Remove other old, new building columns
data <- subset(data, select = -c(oldbuilding, newly_built))
data$is_new <- as.factor(data$is_new)

########################################## area, rooms, balcony
imputarea <- mice(data[, c("area", "rooms", "balcony")], method = 'pmm', m = 5, maxit = 5)
completed_area <- complete(imputarea)
# area
summary(completed_area$area)
data$area <- ifelse(is.na(data$area), completed_area$area, data$area)
sum(is.na(data$area))
# rooms
data$rooms <- ifelse(is.na(data$rooms), completed_area$rooms, data$rooms)
sum(is.na(data$rooms))

##########################################  anteil_efh, Avg_size_household, Avg_age, Anteil_auslaend
# Imputation for anteil_efh, Avg_size_household, Avg_age, Anteil_auslaend
imputed_anteil <- mice(data[, c("anteil_efh", "Avg_size_household", "Avg_age", "Anteil_auslaend")], method = 'pmm', m = 5, maxit = 5)
completed_anteil <- complete(imputed_anteil)
# Check results 
summary(data$Avg_age)
summary(completed_anteil$Avg_age)
data$anteil_efh <- ifelse(is.na(data$anteil_efh), completed_anteil$anteil_efh, data$anteil_efh)
data$Avg_age <- ifelse(is.na(data$Avg_age), completed_anteil$Avg_age, data$Avg_age)
data$Avg_size_household <- ifelse(is.na(data$Avg_size_household), completed_anteil$Avg_size_household, data$Avg_size_household)
data$Anteil_auslaend <- ifelse(is.na(data$Anteil_auslaend), completed_anteil$Anteil_auslaend, data$Anteil_auslaend)

########################################## avg_anzhl_geschosse, floors
# Imputation for avg_anzhl_geschosse
imputed_geschosse <- mice(data[, c("avg_anzhl_geschosse", "floors")], method = 'pmm', m = 5, maxit = 5)
completed_geschosse <- complete(imputed_geschosse)
# Check results 
summary(data$avg_anzhl_geschosse)
summary(completed_geschosse$avg_anzhl_geschosse)
data$avg_anzhl_geschosse <- ifelse(is.na(data$avg_anzhl_geschosse), completed_geschosse$avg_anzhl_geschosse, data$avg_anzhl_geschosse)

# floors
summary(data$floors)
summary(completed_geschosse$floors)
data$floors <- ifelse(is.na(data$floors), completed_geschosse$floors, data$floors)

########################################## wgh_avg_sonnenklasse_per_egid
# Imputation for wgh_avg_sonnenklasse_per_egid
imputed_sonnen <- mice(data[, c("wgh_avg_sonnenklasse_per_egid", "Micro_rating_SunAndView")], method = 'pmm', m = 5, maxit = 5)
completed_sonnen <- complete(imputed_sonnen)
# Check results 
summary(data$wgh_avg_sonnenklasse_per_egid)
summary(completed_sonnen$wgh_avg_sonnenklasse_per_egid)
data$wgh_avg_sonnenklasse_per_egid <- ifelse(is.na(data$wgh_avg_sonnenklasse_per_egid), 
                                             completed_sonnen$wgh_avg_sonnenklasse_per_egid, data$wgh_avg_sonnenklasse_per_egid)

############################################ bauperiode
imputed_bauperiode <- mice(data[, c("year_built", "avg_bauperiode", "GDENAMK")], method = 'pmm', m = 5, maxit = 5)
completed_bauperiode <- complete(imputed_bauperiode)
# Check results 
summary(data$avg_bauperiode)
summary(completed_bauperiode$avg_bauperiode)
data$avg_bauperiode <- ifelse(is.na(data$avg_bauperiode), completed_bauperiode$avg_bauperiode, data$avg_bauperiode)

############################################ raised_groundfloor
# Assuming no indication means negation: Turn NAs into 0
data$raised_groundfloor[is.na(data$raised_groundfloor)] <- 0
sum(is.na(data$raised_groundfloor))

############################################ water
# Assuming no indication means confirmation: Turn NAs into 1
data$water[is.na(data$water)] <- 1
sum(is.na(data$water))

############################################ shower
# Assuming no indication means confirmation: Turn NAs into 1
data$shower[is.na(data$shower)] <- 1
sum(is.na(data$shower))


############################################ toilets
# Assuming no indication means confirmation: Turn NAs into 1
data$toilets[is.na(data$toilets)] <- 1
sum(is.na(data$toilets))



###### ---------------------------------------------------------#### Preprocessing ###########
library(dplyr)

df <- data
#df <- test_after_all

# List all NA variables ------------------------------------------------------
all_na_vars <- sapply(df, function(col) all(is.na(col)))
all_na_var_names <- names(all_na_vars)[all_na_vars]
print(all_na_var_names)

# Drop all NA variables and create new variable df
df <- df[, !all_na_vars]


# List vars with NA ------------------------
# Assuming your dataframe is named 'df'
na_counts <- colSums(is.na(df))

# Create a data frame for variables with NAs and their counts
na_data <- data.frame(
  Variable = names(na_counts),
  NAs = na_counts
)

# Print the table
print(na_data)

# Drop vars with NA over threshold is 80%----
# Set the threshold 
threshold <- 0.8
# Identify variables
variables_to_remove_NA_threshold <- colnames(df) [colMeans(is.na(df)) > threshold]

# Print the names of variables to be removed
cat( paste(variables_to_remove_NA_threshold, collapse = ", "), "\n")

# Create a new data frame without the removed variables
df <- df[, colMeans(is.na(df)) <= threshold]


# List all no variability variables ------------------------------------------
no_variability_vars <- sapply(df, function(col) length(unique(col, na.rm = TRUE)) == 1)
no_variability_var_names <- names(no_variability_vars)[no_variability_vars]
print(no_variability_var_names)

# Drop no variability vars 
df <- df[, !no_variability_vars]


# kids_friendly, water,shower, toilets -no variability

sapply(df, is.numeric)
sapply(df, is.factor)

df[, c("msregion", "home_type")] <- lapply(df[, c("msregion", "home_type")], as.factor)

for (i in colnames(df)) {
  unique_values <- unique(df[[i]])
  variety <- length(unique(df[[i]]))
  
  if (variety > 2) {
    message(i, " has variability (", variety, " unique values)")
  } else if (variety == 2) {
    # Count the occurrences of each value
    valA <- sum(df[[i]] == unique_values[1])
    valB <- sum(df[[i]] == unique_values[2])
    
    # Calculate the percentage of each value
    percent_valA <- valA / length(df[[i]]) * 100
    percent_valB <- valB / length(df[[i]]) * 100
    
    message(i, " has two values. ", valA, ": ", percent_valA, "% (", valA, " occurrences), ", valB, ": ", percent_valB, "% (", valB, " occurrences)")
    
    
    if (percent_valA < 6 | percent_valB < 6) {
      df[[i]] <- NULL
      print(df[[i]])
    } else {
      df[[i]] <- as.factor(df[[i]])
    }
  }
}
colnames(df)

# Remove  variables  -----------------------------------------------------------
# Remove ID columns 
df <- df %>% 
  select(-c(key))

df <- df %>% 
  select(-c('...1', '...2', '...3'))

# Remove date columns
df <- df %>% 
  select(-c(date, month ))

# Remove geographical columns
df <- df %>% 
  select(-c(GDENAMK, GDENR, address, lat, lon, KTKZ, quarter_general, quarter_specific))

# Remove description
df <- df %>% 
  select(-descr)

# Remove year_built since there is is_new and avg_bauperiode
df <- df %>% 
  select(-year_built)

# Data types ----------------------------------------------------------
df_variable_types <- sapply(df, function(col) class(col))

print(df_variable_types)
str(df)
# Sort out 

# Categoricals ----------------------
# List columns
cols_to_factorize <- c("home_type", "msregion")#, #"raised_groundfloor")

# Apply as.factor to the selected columns
df <- df %>% 
  mutate(across(
    .cols = all_of(cols_to_factorize),
    .fns = as.factor
  ))

# Factorize vars again ----------------------
cols_to_factor <-  c("parking", "balcony", "basement", "dishwasher", "dryer", "minergie", 
                     "laundry", "oven", "elevator", "furnished", "pets", "pool", "shared_flat", 
                     "kids_friendly", "cabletv", "cheminee", "playground", "wheelchair", "is_new")

df <- df %>% 
  mutate(across(
    .cols = any_of(cols_to_factor),
    .fns = as.factor
  ))

########## Correlation --------------------------------------------------------------------------------------------

# Select numerical columns and remove rows with NA
vars_num <- sapply(df, is.numeric)
df_no_na <- na.omit(df[vars_num])

# Calculate the correlation matrix
cor_matrix <- cor(df_no_na, use = "complete.obs")

# Plotting the correlation matrix
#png("Correlation_plot_numeric.png", width = 1600, height = 1600)
corrplot.mixed(cor_matrix, 
               order = "hclust", 
               upper = "circle", 
               lower = "number", 
               tl.pos = "lt", 
               tl.col = "black", 
               lower.col = "black",
               number.cex = 1,
               main = "Correlation Matrix for Numerical Variables",
               mar = c(1,1,0.5,0.5)
)
dev.off()

# Identifying high correlations
threshold <- 0.80
diag(cor_matrix) <- 0
high_corr_pairs <- which(upper.tri(cor_matrix, diag = FALSE) & abs(cor_matrix) > threshold, arr.ind = TRUE)

# Listing high correlation pairs
high_corr_vars <- data.frame(Variable1 = rownames(cor_matrix)[high_corr_pairs[, 1]],
                             Variable2 = colnames(cor_matrix)[high_corr_pairs[, 2]],
                             Correlation = cor_matrix[high_corr_pairs[, 1], high_corr_pairs[, 2]])

print(high_corr_vars)

# # Remove one variable from each pair
# variables_to_remove_corr_num <- unique(c( high_corr_vars$Variable2))
# 
# # Remove the variables from the data frame
# final <- df[, !names(df) %in% variables_to_remove_corr_num]



############## LASSO ----

# Convert factors to dummy variables
data_numeric <- data.frame(model.matrix(~ . - 1, data = df))

# Separate the response variable and predictors
response_variable <- data_numeric$rent_full
predictors <- data_numeric[, !colnames(data_numeric) %in% 'rent_full']

# Convert predictors to a matrix
predictor_matrix <- as.matrix(predictors)

# Fit Lasso model with cross-validation
set.seed(123) # for reproducibility
cv_lasso <- cv.glmnet(predictor_matrix, response_variable, alpha = 1)

# Identify the best lambda
best_lambda <- cv_lasso$lambda.min

# Fit the final Lasso model with the best lambda
final_lasso_model <- glmnet(predictor_matrix, response_variable, alpha = 1, lambda = best_lambda)
# Extract coefficients at the best lambda
coefficients <- coef(final_lasso_model, s = best_lambda)




# Predict using the final Lasso model
predictions <- predict(final_lasso_model, newx = predictor_matrix, s = best_lambda)
# Calculate MAE and RMSE
mae_value <- mae(response_variable, predictions)
rmse_value <- rmse(response_variable, predictions)

# Output the MAE and RMSE
print(paste("MAE:", mae_value)) # "MAE: 261.949107367746"" with correlated
print(paste("RMSE:", rmse_value)) # "RMSE: 385.713890224657"


# Remove not important 
dfl <- subset(final, select = -c(Noise_max))

# 51_splits script --------------------------------------------------


#61_RF_ML_nogrig 1)rmse: 393, mae :258. 2) 
#62_XGB_nogrid 1)rmse: 332, mae: 222
# results <- bind_cols(test_data, predictions = xgb_pred$.pred)
# mae_value <- mae(data = results, truth = rent_full, estimate = predictions)
# print(mae_value)
# rmse_value <- rmse(data = results, truth = rent_full, estimate = predictions)


