library(parallel) # install.packages("parallel")
library(doParallel) # install.packages("doParallel")
library(tidyverse)
library(tidymodels)
library(parsnip)
library(vip)
library(caret)
# Start Parallelisation with n-1 cores -----------------------------------

cores <- parallel::detectCores(logical = FALSE) # Check how many cores you have available
num_cores <- max(1, cores - 1)  # Use n-1 cores, but at least 1 core

cl <- parallel::makePSOCKcluster(num_cores) # Create a parallel cluster
doParallel::registerDoParallel(cl) # Register the parallel backend
showConnections() # Shows the connections currently active (should match core number - 1)

# Load Data --------------------------------------------------------------

# Define model ----
xgb_model <- 
    boost_tree(
      trees = tune(),
      tree_depth = tune(),  
      min_n = tune(),       
      learn_rate = 0.5, 
      loss_reduction = 0.2  
    ) %>% 
      set_engine("xgboost") %>% 
      set_mode("regression")

# Create a grid for tuning parameters ----
xgb_grid <- grid_regular(range_set(trees(), c(30, 100)),
                        range_set(min_n(), c(1, 5)),
                        range_set(tree_depth(), c(10, 30)))


# Train model with grid search ----
system.time({
  set.seed(42)
  xgb_workflow <- 
    workflow() %>%
    add_recipe(data_recipe) %>%
    add_model(xgb_model) 
})



# Evaluate resamples ----

system.time({
  set.seed(42)
  xgb_resamples <- 
    xgb_workflow %>% 
    tune_grid(resamples = train_folds, grid = xgb_grid)
  
})

system.time({
  xgb_results <- 
    xgb_workflow }) #%>% 
#collect_metrics()

xgb_resamples %>%
  collect_metrics()


#autoplot(xgb_resamples)

show_best(x = xgb_resamples, metric = "rmse")

# Finalise model workflow -------------------------------------------------
#best_xgb <- select_best(x = xgb_results, metric = "mse")
# Identify the best model based on MSE ----
best_xgb <- xgb_resamples$.metrics[[1]] %>%
  filter(.metric == "rmse") %>%
  filter(.estimate == min(.estimate)) %>%
  select(trees, min_n, tree_depth)



# Extract the best tuning parameters ----
best_params <- best_xgb %>%
  slice_min(order_by = "mean", n = 1) %>%
  select(trees, min_n, tree_depth)

# Print or use the best_params as needed
print(best_params)


system.time({
  set.seed(42)
  final_workflow_xgb <- finalize_workflow(xgb_workflow, best_xgb)
})

# Fit ---------------------------------------------------------

xgb_fit <- fit(final_workflow_xgb, train_data)

# Variable Importance -----------------------------------------------------
library(vip)

vi_df <-  xgb_fit %>% 
  extract_fit_parsnip() %>% # extract the fit object 
  vi(scale= TRUE) #scale the variable importance scores so that the largest is 100

ggplot(vi_df, aes(x = reorder(Variable, Importance), y = Importance)) +
  geom_col(fill = "#0072B2") +
  coord_flip() +
  labs(title = "Variable Importance Plot",
       x = "Variable",
       y = "Importance")


# Predict ------------------------------------------------

xgb_pred <- predict(xgb_fit, new_data = test_data)

predictions <- test_data %>% 
  select(rent_full) %>% #
  mutate(xgb_pred = round(xgb_pred$.pred, 0))

# Calculate pexgbormance metrics -------------------------------------------
predictions <- test_data %>%
  select(rent_full) %>%
  bind_cols(.pred = xgb_pred$.pred) %>%
  mutate(xgb_pred = round(.pred, 0))


# Specify set of metrics to evaluate

# Calculate pexgbormance metrics -------------------------------------------
metrics(predictions, truth = rent_full, estimate = xgb_pred)

# Specify set of metrics to evaluate
multi_metric <- metric_set(rmse, mae, mape, rsq, huber_loss)

multi_metric(predictions, truth = rent_full, estimate = xgb_pred)





# End Parallelisation -----------------------------------------------------

parallel::stopCluster(cl)
doParallel::registerDoSEQ()  # Register the sequential backend to avoid issues after stopping the cluster
showConnections() # Shows the connections currently active (should match core number)
