# Install and load necessary libraries
install.packages(c("caret", "tidymodels", "randomForest", "xgboost", "e1071", "gbm"))

library(caret)
library(tidymodels)
library(randomForest)
library(xgboost)
library(e1071)
library(gbm)
library(rsample)
set.seed(42)

data_fin <- df
#data_fin <- df_corr
#data_fin <- df_wo_svars
#data_fin <- df_wo_mrs  #using this
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
  step_scale(all_numeric(), -all_outcomes()) %>% 
  step_dummy(all_nominal()) %>% 
  step_rm(starts_with('..')) %>% 
  prep()  
  
  


# Apply the preprocessing steps to the  data
train_data_baked <- bake(data_recipe, new_data = train_data)
test_data_baked <- bake(data_recipe, new_data = test_data)

# Removing columns starting with '..' 
train_data_baked <- train_data_baked %>%
  select(-starts_with('..'))

test_data_baked <- test_data_baked %>%
  select(-starts_with('..'))


# Define cross-validation with 4 folds
train_folds <- vfold_cv(data = train_data, v = 5)
