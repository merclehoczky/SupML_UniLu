# Start Parallelisation with n-1 cores -----------------------------------
cores <- parallel::detectCores(logical = FALSE)
num_cores <- max(1, cores - 1)
cl <- parallel::makePSOCKcluster(num_cores)
doParallel::registerDoParallel(cl)
showConnections()

# Load Data --------------------------------------------------------------
# Assume data_recipe, train_data, test_data, and train_folds are already defined

# Define model ----
rf_model <- 
  rand_forest() %>%
  set_engine("randomForest") %>%
  set_mode("regression") %>%
  set_args(mtry = 3, trees = 100, min_n = 2, tree_depth = 10)  # Set fixed parameters

# Create a workflow with recipe and model ----
rf_workflow <- 
  workflow() %>%
  add_recipe(data_recipe) %>%
  add_model(rf_model)

# Fit and evaluate with cross-validation ----
set.seed(42)

system.time({
rf_resamples <- 
  rf_workflow %>% 
  fit_resamples(resamples = train_folds) %>% 
  collect_metrics()
})
# Print the best model ----
show_best(x = rf_resamples, metric = "rsq")

# Finalize the model workflow ----
best_rf <- select_best(x = rf_resamples, metric = "rsq")
final_workflow_rf <- 
  rf_workflow %>% 
  finalize_workflow(best_rf)

# Fit the final model ----
rf_fit <- fit(final_workflow_rf, data = train_data)

# Variable Importance -----------------------------------------------------
vi_df <- rf_fit %>%
  extract_fit_parsnip() %>%
  vi(scale = TRUE)

# Plot variable importance ----
ggplot(vi_df, aes(x = reorder(Variable, Importance), y = Importance)) +
  geom_col(fill = "#0072B2") +
  coord_flip() +
  labs(title = "Variable Importance Plot",
       x = "Variable",
       y = "Importance")

# Predict --------------------------------------------------------------
rf_pred <- predict(rf_fit, new_data = test_data)

# Calculate performance metrics ----------------------------------------
predictions <- test_data %>%
  select(rent_full) %>%
  bind_cols(.pred = rf_pred$.pred) %>%
  mutate(rf_pred = round(.pred, 0))

# Specify set of metrics to evaluate ----
multi_metric <- metric_set(rmse, mae, mape, rsq, huber_loss)
metrics_result <- multi_metric(predictions, truth = rent_full, estimate = rf_pred)

# Print the metrics ----
print(metrics_result)

# Confusion matrix ------------------------------------------------------
# Note: For regression problems, confusion matrix is not applicable

# End Parallelisation -----------------------------------------------------
parallel::stopCluster(cl)
doParallel::registerDoSEQ()
showConnections()
