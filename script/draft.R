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

#################### Google API ------
# training_data <- "/Users/viktoriiatantsiura/Downloads/University_Luzern/3 Semester/Machine learning/Final Project/after_api.csv"
# training_data <- read_csv(training_data)

#################### Text mining ----------

# Getting the path of your current open file
# current_path = rstudioapi::getActiveDocumentContext()$path 
# setwd(dirname(current_path ))
# print( getwd() )
# 
# # Setup 
# # Run script to set up functions
# source("100_textmining_setup.R")
# 
# # Run text mining scripts 
# # Run script that looks up parking
# source("101_parking.R")
# 
# # Run script that looks up balcony, terrace and veranda
# source("102_balcony.R")
# 
# # Run script that looks up basement
# source("103_basement.R")
# 
# # Run script that looks up dishwasher
# source("104_dishwasher.R")
# 
# # Run script that looks up dryer
# source("105_dryer.R")
# 
# # Run script that looks up laundry
# source("106_laundry.R")
# 
# # Run script that looks up oven (NA -> 1)
# source("107_oven.R")
# 
# # Run script that looks up elevator
# source("108_elevator.R")
# 
# # Run script that looks up furnished
# source("109_furnished.R")
# 
# # Run script that looks up pets ( no NA change!)
# source("110_pets.R")
# 
# # Run script that looks up pool
# source("111_pool.R")
# 
# # Run script that looks up shared_flat (NA -> 0)
# source("112_shared_flat.R")
# 
# # Run script that looks up kids_friendly (NA -> 1)
# source("113_kids_friendly.R")
# 
# # Run script that looks up cable tv (NA -> 1)
# source("114_cabletv.R")
# 
# # Run script that looks up cheminee
# source("115_cheminee.R")
# 
# # Run script that looks up minergie
# source("116_minergie.R")
# 
# # Run script that looks up playgrounds
# source("117_playground.R")
# 
# # Run script that looks up wheelchair accessibility
# source("118_wheelchair.R")
#write.csv(data, "/Users/viktoriiatantsiura/Downloads/University_Luzern/3 Semester/Machine learning/Final Project/sup_ml/script/data/textmining_google.csv")



#################### Cleaning -------

final <- "/Users/viktoriiatantsiura/Downloads/University_Luzern/3 Semester/Machine learning/Final Project/sup_ml/script/data/textmining_google.csv"
final <- read_csv(final)

# Remove unusual, outliers
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

#################### Imputation ----
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

#################### More cleaning -----

final <- data
na_count <- final %>%
  summarise_all(~sum(is.na(.)))

# Getting names of columns where the sum of NAs is 0
col_na <- colnames(na_count)[na_count == 0]

# Printing the column names
print(col_na)

# Remove columns with NAs (heating, garden, )
final <- final[col_na]

# Remove columns we don't need
final <- subset(final, select = -c(lat, lon, month, address, quarter_general, quarter_specific, GDENR, 
                                 date, GDENAMK, ...1, ...2, year, key, kids_friendly, water,shower, toilets))
# kids_friendly, water,shower, toilets -no variability

sapply(final, is.numeric)
sapply(final, is.factor)

final[, c("KTKZ", "home_type")] <- lapply(final[, c("KTKZ", "home_type")], as.factor)

for (i in colnames(final)) {
  unique_values <- unique(final[[i]])
  variety <- length(unique(final[[i]]))
  
  if (variety > 2) {
    message(i, " has variability (", variety, " unique values)")
  } else if (variety == 2) {
    # Count the occurrences of each value
    valA <- sum(final[[i]] == unique_values[1])
    valB <- sum(final[[i]] == unique_values[2])
    
    # Calculate the percentage of each value
    percent_valA <- valA / length(final[[i]]) * 100
    percent_valB <- valB / length(final[[i]]) * 100
    
    message(i, " has two values. ", valA, ": ", percent_valA, "% (", valA, " occurrences), ", valB, ": ", percent_valB, "% (", valB, " occurrences)")
    
    
    if (percent_valA < 6 | percent_valB < 6) {
      final[[i]] <- NULL
      print(final[[i]])
    } else {
        final[[i]] <- as.factor(final[[i]])
      }
    }
}
colnames(final)



######## Correlation -----
df <- final

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
data_numeric <- data.frame(model.matrix(~ . - 1, data = data_fin))

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

# 51_splits script
#61_RF_ML_nogrig 1)rmse: 393, mae :258. 2) 
#62_XGB_nogrid 1)rmse: 332, mae: 222
# results <- bind_cols(test_data, predictions = xgb_pred$.pred)
# mae_value <- mae(data = results, truth = rent_full, estimate = predictions)
# print(mae_value)
# rmse_value <- rmse(data = results, truth = rent_full, estimate = predictions)


############## Prediction ----

set.seed(42)
data_fin <- final

# 51_splits script
# 61_RF_ML_nogrid # MAE 261, RMSE 396
# 62_XGB_nogrid

# Bind the predictions to the test dataset
results <- bind_cols(test_data, predictions = xgb_pred$.pred)
# Calculate MAE
mae_value <- mae(data = results, truth = rent_full, estimate = predictions)
rmse_value <- rmse(data = results, truth = rent_full, estimate = predictions)
# Print the RMSE, MAE value
print(rmse_value) # 336
print(mae_value) # 225



##################################### Test data ------
test <- "/Users/viktoriiatantsiura/Downloads/University_Luzern/3 Semester/Machine learning/Final Project/test_after_gapi_text.csv"
test_data <- read_csv(test)

data <- test_data
# imputations

# Remove columns we don't need
test_data_new <- subset(data, select = -c(month, address, quarter_general, quarter_specific, GDENR, 
                                   date, GDENAMK, ...1, ...2, year, key, kids_friendly, water,shower, toilets))

column <- c("KTKZ", "area", "balcony", "basement", "cheminee", "dishwasher", "elevator", 
            "floors", "home_type", "laundry", "msregion", "rooms", "Micro_rating", 
            "Micro_rating_NoiseAndEmission", "Micro_rating_Accessibility", "Micro_rating_DistrictAndArea", 
            "Micro_rating_SunAndView", "Micro_rating_ServicesAndNature", "wgh_avg_sonnenklasse_per_egid", 
            "Anteil_auslaend", "Avg_age", "Avg_size_household", "Noise_max", "anteil_efh", 
            "apoth_pix_count_km2", "avg_anzhl_geschosse", "avg_bauperiode", "dist_to_4G", "dist_to_5G", 
            "dist_to_haltst", "dist_to_highway", "dist_to_lake", "dist_to_main_stat", "dist_to_school_1", 
            "dist_to_train_stat", "restaur_pix_count_km2", "superm_pix_count_km2", "dist_to_river", 
            "parking", "is_new")

test_data_new <- test_data_new[column]

test_data_new[, c("KTKZ", "balcony", "basement", "cheminee", "dishwasher", "elevator", "home_type","laundry", "parking",  "is_new")] <- lapply(data[, c("KTKZ", "balcony", "basement", "cheminee", "dishwasher", "elevator", "home_type","laundry", "parking",  "is_new")], as.factor)
test_data_new %>%
  summarise_all(~sum(is.na(.)))
colnames(final)

# Make predictions on the test set
xgb_pred_test <- predict(xgb_fit, new_data = test_data_new)
summary(xgb_pred_test)

#Create result table
test_data_with_predictions <- bind_cols(data$key, .pred = xgb_pred_test$.pred)

# Assuming 'key' is the key column in test_data
test_data_with_predictions <- bind_cols(
  key = data$key,
  rent = xgb_pred_test$.pred
)
