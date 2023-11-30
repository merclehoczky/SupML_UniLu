# Start Parallelisation with n-1 cores -----------------------------------
cores <- parallel::detectCores(logical = FALSE)
num_cores <- max(1, cores - 1)
cl <- parallel::makePSOCKcluster(num_cores)
doParallel::registerDoParallel(cl)
showConnections()

library(vip)
library(caret)
# Define model ----
lm_model <- 
  linear_reg() %>%  # Model type: Linear Regression
  set_engine("lm") %>%  # Computational engine: lm
  set_mode("regression")

# Create a workflow with recipe and model ----
lm_workflow <- 
  workflow() %>%
  add_recipe(data_recipe) %>%
  add_model(lm_model)

# Fit the model with cross-validation using folds ----
set.seed(42)
system.time({
  lm_resamples <- 
    fit_resamples(lm_workflow, resamples = train_folds)
})

# Collect metrics
lm_metrics <- lm_resamples %>% collect_metrics()

# Print the results
print(lm_metrics)

# Access the best model (if needed)
best_lm <- select_best(lm_resamples, metric = "rmse")
final_workflow_lm <- 
  finalize_workflow(lm_workflow, best_lm)

# Fit the final model on the entire training data (if needed)
system.time({
  lm_fit <- fit(final_workflow_lm, data = train_data)
})

# Variable Importance (not applicable for linear regression) -----------------
# Linear regression doesn't have variable importance like tree-based models

# Predict --------------------------------------------------------------
lm_pred <- predict(lm_fit, new_data = test_data)

# Calculate performance metrics ----------------------------------------
predictions_lm <- test_data %>%
  select(rent_full) %>%
  bind_cols(.pred = lm_pred$.pred) %>%
  mutate(lm_pred = round(.pred, 0))

# Calculate performance metrics ----
rmse_result_lm <- RMSE(lm_pred$.pred, test_data$rent_full)

# Print the metrics ----
print(rmse_result_lm)



# End Parallelisation -----------------------------------------------------
parallel::stopCluster(cl)
doParallel::registerDoSEQ()
showConnections()
