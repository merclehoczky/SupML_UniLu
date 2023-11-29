# Start Parallelisation with n-1 cores -----------------------------------
cores <- parallel::detectCores(logical = FALSE)
num_cores <- max(1, cores - 1)
cl <- parallel::makePSOCKcluster(num_cores)
doParallel::registerDoParallel(cl)
showConnections()

library(kernlab)
library(yardstick)
library(caret)
# Define model ----
svm_model <- 
  svm_rbf() %>% 
  set_mode("regression")

# Create a workflow with recipe and model ----
svm_workflow <- 
  workflow() %>%
  add_recipe(data_recipe) %>%
  add_model(svm_model)

# Fit the model with cross-validation using folds ----
set.seed(42)
system.time({
  svm_resamples <- 
    fit_resamples(svm_workflow, resamples = train_folds)
})

# Collect metrics
svm_metrics <- svm_resamples %>% collect_metrics()

# Print the results
print(svm_metrics)

# Access the best model (if needed)

# Finalize the model workflow ----
best_svm <- select_best(svm_resamples, metric = "rmse")

final_workflow_svm <- 
  finalize_workflow(svm_workflow, best_svm)

# Fit the final model on the entire training data (if needed)
system.time({
  svm_fit <- fit(final_workflow_svm, data = train_data)
})



# Predict --------------------------------------------------------------
svm_pred <- predict(svm_fit, new_data = test_data)

# Calculate performance metrics ----------------------------------------
# # Specify set of metrics to evaluate ----
# multi_metric_svm <- metric_set(rmse, mae, mape, rsq, huber_loss)
# metrics_result_svm <- multi_metric_svm(svm_pred, truth = rent_full)
# 
# # Print the metrics ----
# print(metrics_result_svm)

# Calculate performance metrics ----

# Assuming 'rent_full' is the actual truth column
rmse_result_svm <- RMSE(svm_pred$.pred, test_data$rent_full)






# Print the metrics ----
print(rmse_result_svm)




# End Parallelisation -----------------------------------------------------
parallel::stopCluster(cl)
doParallel::registerDoSEQ()
showConnections()
