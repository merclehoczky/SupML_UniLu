# Start Parallelisation with n-1 cores -----------------------------------
cores <- parallel::detectCores(logical = FALSE)
num_cores <- max(1, cores - 1)
cl <- parallel::makePSOCKcluster(num_cores)
doParallel::registerDoParallel(cl)
showConnections()

library(caret)


# Define model ----
xgb_model <- 
  boost_tree(
    trees = 100,
    tree_depth = 10,  # Set your desired tree depth
    min_n = 5,       # Set your desired min_n
    learn_rate = 0.5, # Set your desired learn_rate
    loss_reduction = 0.2  # Set your desired loss_reduction
  ) %>% 
  set_engine("xgboost", objective = "reg:squarederror") %>% 
  set_mode("regression")

# Create a workflow with recipe and model ----
xgb_workflow <- 
  workflow() %>%
  add_recipe(data_recipe) %>%
  add_model(xgb_model)

# Fit the model with cross-validation using folds ----
set.seed(42)
system.time({
  xgb_resamples <- 
    fit_resamples(xgb_workflow, resamples = train_folds)
})

# Collect metrics
xgb_metrics <- xgb_resamples %>% collect_metrics()

# Print the results
print(xgb_metrics)

# Access the best model (if needed)

# Finalize the model workflow ----
best_xgb <- select_best(xgb_resamples, metric = "rmse")

final_workflow_xgb <- 
  finalize_workflow(xgb_workflow, best_xgb)

# Fit the final model on the entire training data (if needed)
system.time({
  xgb_fit <- fit(final_workflow_xgb, data = train_data)
})

# Variable Importance -----------------------------------------------------
# Note: Variable importance for xgboost can be obtained differently, check xgboost documentation
# The variable importance plot below is just a placeholder
vi_df <- data.frame(Variable = colnames(train_data), Importance = 1:ncol(train_data))

# Plot variable importance ----
ggplot(vi_df, aes(x = reorder(Variable, Importance), y = Importance)) +
  geom_col(fill = "#0072B2") +
  coord_flip() +
  labs(title = "Variable Importance Plot",
       x = "Variable",
       y = "Importance")

# Predict --------------------------------------------------------------
xgb_pred <- predict(xgb_fit, new_data = test_data)

# # Calculate performance metrics ----------------------------------------
# predictions_xgb <- test_data %>%
#   select(rent_full) %>%
#   bind_cols(.pred = xgb_pred$.pred) %>%
#   mutate(xgb_pred = round(.pred, 0))
# 
# # Specify set of metrics to evaluate ----
# multi_metric_xgb <- metric_set(rmse, mae, mape, rsq, huber_loss)
# metrics_result_xgb <- multi_metric_xgb(predictions_xgb, truth = rent_full, estimate = xgb_pred)
# 
# # Print the metrics ----
# print(metrics_result_xgb)

# Calculate performance metrics ----
rmse_result_xgb <- RMSE(xgb_pred$.pred, test_data$rent_full)

# Print the metrics ----
print(rmse_result_xgb)


# End Parallelisation -----------------------------------------------------
parallel::stopCluster(cl)
doParallel::registerDoSEQ()
showConnections()
