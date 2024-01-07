library(readr)
library(dplyr)
library(caret)
library(randomForest)
library(glmnet)
library(corrplot)
library(mice)
library(Metrics)
library(xgboost)

data <- "/Users/viktoriiatantsiura/Downloads/University_Luzern/3 Semester/Machine learning/Final Project/sup_ml/script/data/final_data.csv"
data <- read_csv(data)

na_count <- data %>%
  summarise_all(~sum(is.na(.)))

# Getting names of columns where the sum of NAs is 0
col_na <- colnames(na_count)[na_count == 0]

# Printing the column names
print(col_na)

data <- data[col_na]

# Remove columns we don't need
data <- subset(data, select = -c(month, address, quarter_general, quarter_specific, GDENR, 
                                 date, GDENAMK, ...1, kids_friendly, water,shower, toilets, year, key))


sapply(data, is.numeric)
sapply(data, is.factor)

data[, c("KTKZ", "home_type")] <- lapply(data[, c("KTKZ", "home_type")], as.factor)

for (i in colnames(data)) {
  unique_values <- unique(data[[i]])
  variety <- length(unique(data[[i]]))
  
  if (variety > 2) {
    message(i, " has variability (", variety, " unique values)")
  } else if (variety == 2) {
    # Count the occurrences of each value
    valA <- sum(data[[i]] == unique_values[1])
    valB <- sum(data[[i]] == unique_values[2])
    
    # Calculate the percentage of each value
    percent_valA <- valA / length(data[[i]]) * 100
    percent_valB <- valB / length(data[[i]]) * 100
    
    message(i, " has two values. ", valA, ": ", percent_valA, "% (", valA, " occurrences), ", valB, ": ", percent_valB, "% (", valB, " occurrences)")
    
    
    if (percent_valA < 6 | percent_valB < 6) {
      data[[i]] <- NULL
    } else {
        data[[i]] <- as.factor(data[[i]])
      }
    }
}

lmodel <- lm(rent_full ~ area, data)
plot(lm_model)
plot(rent_full ~ area, data)

# Remove unusual, outliers
data <- data %>% 
  filter(area > 10 & area < 400, rent_full < 6001)

######## Correlation -----
df <- data

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
# df_corr <- df[, !names(df) %in% variables_to_remove_corr_num]


############## LASSO ----

dfl <- subset(data, select = -c(Noise_max, Anteil_auslaend))
# Convert factors to dummy variables
data_numeric <- data.frame(model.matrix(~ . - 1, data = data))

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
print(paste("MAE:", mae_value)) # "MAE: 284.667246778324" without correlated
print(paste("RMSE:", rmse_value)) # "RMSE: 422.859386628439"
# with correlated
# "MAE: 279.638598368163"
# "RMSE: 411.941202038712"

# Remove not important 
dfl <- subset(data, select = -c(Noise_max, Anteil_auslaend))

####### Predict ----
dfl <- data.frame(model.matrix(~ . - 1, data = dfl))
colnames(dfl)
# Assuming 'df' is your data frame and 'rent_full' is the response variable
set.seed(123) # for reproducibility
trainIndex <- createDataPartition(dfl$rent_full, p = .8, list = FALSE, times = 1)
train_data <- dfl[trainIndex, ]
test_data <- dfl[-trainIndex, ]

# Prepare data for xgboost (requires a matrix for predictors)
train_matrix <- as.matrix(train_data[, !colnames(train_data) %in% 'rent_full'])
test_matrix <- as.matrix(test_data[, !colnames(test_data) %in% 'rent_full'])
train_label <- train_data$rent_full
test_label <- test_data$rent_full

# XGBoost model
dtrain <- xgb.DMatrix(data = train_matrix, label = train_label)
dtest <- xgb.DMatrix(data = test_matrix, label = test_label)
xg_model <- xgboost(data = dtrain, nrounds = 100, objective = "reg:squarederror")
xg_pred <- predict(xg_model, dtest)

# Random Forest model
rf_model <- randomForest(rent_full ~ ., data = train_data, ntree = 100)
rf_pred <- predict(rf_model, test_data)

# Calculate MAE and RMSE for XGBoost
xg_mae <- mae(test_label, xg_pred)
xg_rmse <- rmse(test_label, xg_pred)

# Calculate MAE and RMSE for Random Forest
rf_mae <- mae(test_label, rf_pred)
rf_rmse <- rmse(test_label, rf_pred)

# Output the results
print(paste("XGBoost MAE:", xg_mae))
print(paste("XGBoost RMSE:", xg_rmse))
print(paste("Random Forest MAE:", rf_mae))
print(paste("Random Forest RMSE:", rf_rmse))

# WIth all correlated

# print(paste("XGBoost MAE:", xg_mae))
# [1] "XGBoost MAE: 224.461542433148"
# > print(paste("XGBoost RMSE:", xg_rmse))
# [1] "XGBoost RMSE: 337.510588117171"
# > print(paste("Random Forest MAE:", rf_mae))
# [1] "Random Forest MAE: 220.373366378372"
# > print(paste("Random Forest RMSE:", rf_rmse))
# [1] "Random Forest RMSE: 339.174908493728"
