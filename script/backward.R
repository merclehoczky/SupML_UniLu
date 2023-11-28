# Assuming your data_fin frame is named 'data_fin'
library(caret)
library(recipes)
library(glmnet)

set.seed(42)  # for reproducibility

# Split the data_fin into train and test sets
splitIndex <- createDataPartition(data_fin$rent_full, p = 0.7, list = FALSE)
train_data_fin <- data_fin[splitIndex, ]
test_data_fin <- data_fin[-splitIndex, ]

# Create a recipe
data_fin_recipe <- recipe(rent_full ~ ., data = train_data_fin) %>%
  step_naomit(all_predictors()) %>% 
  step_zv(all_predictors()) %>%
  step_dummy(all_nominal(), one_hot = TRUE) %>%  
  step_normalize(all_numeric()) %>%
  step_scale(all_numeric()) %>% 
  prep()



# Create a linear regression model
lm_model <- lm(rent_full ~ ., data = bake(data_fin_recipe, new_data = train_data_fin))

# Apply backward selection
backward_model <- step(lm_model, direction = "backward")

# Evaluate the model using cross-validation
cv_results <- train(
  rent_full ~ ., 
  data = bake(data_fin_recipe, new_data = train_data_fin), 
  method = "leapSeq",  # Backward selection for linear regression
  trControl = trainControl("cv", number = 5)
)

# Print the cross-validation results
print(cv_results)

# Make predictions on the test set
test_predictions <- predict(lm_model, newdata = bake(data_fin_recipe, new_data = test_data_fin))

# Evaluate the model on the test set
test_rmse <- sqrt(mean((test_predictions - test_data_fin$rent_full)^2))
cat("Test RMSE:", test_rmse, "\n")

# Get the summary of variable selection
variable_selection_summary <- summary(cv_results$finalModel)

# Print the summary
print(variable_selection_summary)


### try this
library(MASS)
# Create a linear regression model
lm_model <- lm(rent_full ~ ., data = bake(data_fin_recipe, new_data = train_data_fin))

# Apply backward selection using stepAIC
backward_model <- stepAIC(lm_model, direction = "backward")

# Print the summary of variable selection
summary(backward_model)

