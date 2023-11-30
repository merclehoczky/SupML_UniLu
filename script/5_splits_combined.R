# Assuming 'rent_full' is the response variable in your model
# If not, replace 'rent_full' with the correct response variable
data_fin <- df_wo_mrs 
# Combine the training and testing datasets
combined_data <- bind_rows(
  mutate(data_fin, split = "train"),
  mutate(test_data_new, split = "test")
)

# Create a new recipe using the combined dataset
combined_recipe <- recipe(rent_full ~ ., data = combined_data) %>%
  step_naomit(all_predictors()) %>% 
  step_zv(all_predictors()) %>%
  step_dummy(all_nominal(), one_hot = TRUE) %>%  
  step_normalize(all_numeric(), -all_outcomes()) %>%
  step_scale(all_numeric(), -all_outcomes()) %>% 
  prep()

# Apply the same preprocessing steps to the combined data
combined_data_preprocessed <- bake(combined_recipe, new_data = combined_data)


# Filter the data based on split_test and split_train
test_data_preprocessed <- combined_data_preprocessed %>%
  filter(split_test == 1) %>%
  select(-split_test, -split_train)

train_data_preprocessed <- combined_data_preprocessed %>%
  filter(split_train == 1) %>%
  select(-split_test, -split_train)

# Cross validation -------

library(rsample)

# Define the number of folds
num_folds <- 5  # Adjust as needed

# Create cross-validation folds
train_folds <- vfold_cv(train_data_preprocessed, v = num_folds)
