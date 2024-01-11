library(parallel) # install.packages("parallel")
library(doParallel) # install.packages("doParallel")
library(tidyverse)
library(tidymodels)
library(parsnip)
library(vip)
# Start Parallelisation with n-1 cores -----------------------------------

cores <- parallel::detectCores(logical = FALSE) # Check how many cores you have available
num_cores <- max(1, cores - 1)  # Use n-1 cores, but at least 1 core

cl <- parallel::makePSOCKcluster(num_cores) # Create a parallel cluster
doParallel::registerDoParallel(cl) # Register the parallel backend
showConnections() # Shows the connections currently active (should match core number - 1)

# Load Data --------------------------------------------------------------

# Define model ----
rf_model <- 
  rand_forest() %>% # Model type: Random Forest  
  set_engine("randomForest") %>%  # Computational engine: randomForest
  set_mode("regression") %>% 
  set_args(mtry = tune(), trees = tune(), min_n = tune(), tree_depth = tune())

# Create a grid for tuning parameters ----
rf_grid <- grid_regular(range_set(mtry(), c(2, 4)),
                        range_set(trees(), c(50, 60)),
                        range_set(min_n(), c(2, 3)),
                        range_set(tree_depth(), c(10, 20)))


# Train model with grid search ----
system.time({
  set.seed(42)
  rf_workflow <- 
    workflow() %>%
    add_recipe(data_recipe) %>%
    add_model(rf_model) 
})



# Evaluate resamples ----

system.time({
  set.seed(42)
  rf_resamples <- 
    rf_workflow %>% 
    tune_grid(resamples = train_folds, grid = rf_grid)
   
})

system.time({
  rf_results <- 
    rf_workflow }) #%>% 
    #collect_metrics()

rf_resamples %>%
   collect_metrics()


#autoplot(rf_resamples)

show_best(x = rf_resamples, metric = "rmse")

# Finalise model workflow -------------------------------------------------
#best_rf <- select_best(x = rf_results, metric = "mse")
# Identify the best model based on MSE ----
best_rf <- rf_resamples$.metrics[[1]] %>%
  filter(.metric == "rmse") %>%
  filter(.estimate == min(.estimate)) %>%
  select(mtry, trees, min_n, tree_depth)



# Extract the best tuning parameters ----
best_params <- best_rf %>%
  slice_min(order_by = "mean", n = 1) %>%
  select(mtry, trees, min_n, tree_depth)

# Print or use the best_params as needed
print(best_params)


system.time({
  set.seed(42)
  final_workflow_rf <- finalize_workflow(rf_workflow, best_rf)
})

# Fit ---------------------------------------------------------

rf_fit <- fit(final_workflow_rf, train_data)

# Variable Importance -----------------------------------------------------
library(vip)

vi_df <-  rf_fit %>% 
  extract_fit_parsnip() %>% # extract the fit object 
  vi(scale= TRUE) #scale the variable importance scores so that the largest is 100

ggplot(vi_df, aes(x = reorder(Variable, Importance), y = Importance)) +
  geom_col(fill = "#0072B2") +
  coord_flip() +
  labs(title = "Variable Importance Plot",
       x = "Variable",
       y = "Importance")


# Predict ------------------------------------------------

rf_pred <- predict(rf_fit, new_data = test_data)

predictions <- test_data %>% 
  select(rent_full) %>% #
  mutate(rf_pred = round(rf_pred$.pred, 0))

# Calculate performance metrics -------------------------------------------
predictions <- test_data %>%
  select(rent_full) %>%
  bind_cols(.pred = rf_pred$.pred) %>%
  mutate(rf_pred = round(.pred, 0))


# Specify set of metrics to evaluate

# Calculate performance metrics -------------------------------------------
metrics(predictions, truth = rent_full, estimate = rf_pred)

# Specify set of metrics to evaluate
multi_metric <- metric_set(rmse, mae, mape, rsq, huber_loss)

multi_metric(predictions, truth = rent_full, estimate = rf_pred)





# End Parallelisation -----------------------------------------------------

parallel::stopCluster(cl)
doParallel::registerDoSEQ()  # Register the sequential backend to avoid issues after stopping the cluster
showConnections() # Shows the connections currently active (should match core number)
