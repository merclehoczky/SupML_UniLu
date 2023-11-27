#### LASSO ####
library(glmnet)
library(tidymodels)
library(dplyr)

# Assuming your data is in 'train_data' and the response variable is 'rent_full'
x <- model.matrix(rent_full ~ ., data = train_data)[, -1]  # Create model matrix
y <- train_data$rent_full

# Fit Lasso model
lasso_model <- cv.glmnet(x, y, alpha = 1)  # alpha = 1 for Lasso

# Identify the best lambda
best_lambda <- lasso_model$lambda.min
cat("Best Lambda:", best_lambda, "\n")

# Fit the final Lasso model with the best lambda
final_lasso_model <- glmnet(x, y, alpha = 1, lambda = best_lambda / 2)  # Adjust lambda value

# Extract coefficients
lasso_coef <- coef(final_lasso_model, s = best_lambda / 2)

library(broom)

# Extract coefficients and tidy them
lasso_coef_tidy <- tidy(final_lasso_model, s = best_lambda / 2)

# Display the tidy coefficients
print(lasso_coef_tidy)

# Extract absolute coefficients
lasso_coef_tidy <- lasso_coef_tidy %>%
  mutate(Abs_Coefficient = abs(estimate))

# Display the tidy coefficients with absolute values
print(lasso_coef_tidy)

# Extract most important predictors (optional)
lasso_predictors <- lasso_coef_tidy$term[lasso_coef_tidy$estimate != 0]
print(lasso_predictors)

# Make predictions on test data
test_x <- model.matrix(rent_full ~ ., data = test_data)[, -1]
lasso_predictions <- predict(final_lasso_model, newx = test_x, s = best_lambda / 2)

# Evaluate performance (e.g., using RMSE or other regression metrics)
rmse <- sqrt(mean((test_data$rent_full - lasso_predictions)^2))
cat("Root Mean Squared Error (RMSE):", rmse, "\n")
