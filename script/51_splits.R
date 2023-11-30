# Install and load necessary libraries
install.packages(c("caret", "tidymodels", "randomForest", "xgboost", "e1071", "gbm"))

library(caret)
library(tidymodels)
library(randomForest)
library(xgboost)
library(e1071)
library(gbm)

set.seed(42)

data_fin <- df_corr
#data_fin <- df_wo_svars
data_fin <- df_wo_mrs  #using this
#data_fin <- df_wo_mrs_others
#data_fin <- df_wo_variability
#data_fin <- df_new

# Create a 80-20 train-test split

df_split <- initial_split(data_fin, prop = 0.8)
train_data <- training(df_split)
test_data <- testing(df_split)


# Create a recipe for data preprocessing with normalization and scaling

data_recipe <- recipe(rent_full ~ ., data = train_data) %>%
  step_naomit(all_predictors()) %>% 
  step_zv(all_predictors()) %>%
  step_normalize(all_numeric(), -all_outcomes()) %>%
  step_scale(all_numeric(), -all_outcomes()) %>% 
  step_dummy(all_nominal()) %>%  
  prep()
  

# Apply the same preprocessing steps to the training data
#train_data_preprocessed <- bake(data_recipe, new_data = train_data)

# Define cross-validation with 4 folds
#train_folds <- vfold_cv(data = train_data_preprocessed, v = 4)

# Define cross-validation with 4 folds
train_folds <- vfold_cv(data = train_data, v = 5)
