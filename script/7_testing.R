# Assuming 'rent_full' is the response variable in your model
# If not, replace 'rent_full' with the correct response variable

test_data_new <- test_after_all

# List all NA variables ------------------------------------------------------
all_na_vars <- sapply(test_data_new, function(col) all(is.na(col)))
all_na_var_names <- names(all_na_vars)[all_na_vars]
print(all_na_var_names)

# Drop all NA variables and create new variable test_data_new
test_data_new <- test_data_new[, !all_na_vars]


# List vars with NA ------------------------
# Assuming your dataframe is named 'test_data_new'
na_counts <- colSums(is.na(test_data_new))

# Create a data frame for variables with NAs and their counts
na_data <- data.frame(
  Variable = names(na_counts),
  NAs = na_counts
)

# Print the table
print(na_data)

# Drop vars with NA over threshold is 80%----
test_data_new <- test_data_new[, !names(test_data_new) %in% variables_to_remove_NA_threshold]




# Drop no variability vars ------------------------------------------
test_data_new <- test_data_new[, !no_variability_vars]



# Remove  variables  -----------------------------------------------------------
# Remove ID columns 
test_data_new <- test_data_new %>% 
  select(-c(key))

test_data_new <- test_data_new %>% 
  select(-c('...1'))

# Remove date columns
test_data_new <- test_data_new %>% 
  select(-c(date, month ))

# Remove geographical columns
test_data_new <- test_data_new %>% 
  select(-c(GDENAMK, GDENR, address, lat, lon, msregion, quarter_general, quarter_specific))

# Remove description
test_data_new <- test_data_new %>% 
  select(-descr)

# Remove year_built since there is is_new and avg_bauperiode
test_data_new <- test_data_new %>% 
  select(-year_built)

# Data types ----------------------------------------------------------
test_data_new_variable_types <- sapply(test_data_new, function(col) class(col))

print(test_data_new_variable_types)
str(test_data_new)
# Sort out 

# Categoricals ----------------------
# List columns
cols_to_factorize <- c("home_type", "KTKZ", "raised_groundfloor")

# Apply as.factor to the selected columns
test_data_new <- test_data_new %>% 
  mutate(across(
    .cols = all_of(cols_to_factorize),
    .fns = as.factor
  ))

# Factorize vars again ----------------------
cols_to_factor <-  c("parking", "basement", "dishwasher", "dryer", 
                     "laundry", "oven", "elevator", "furnished", "pets", "pool", "shared_flat", 
                     "kids_friendly", "cabletv", "cheminee", "playground", "wheelchair", "is_new")

test_data_new <- test_data_new %>% 
  mutate(across(
    .cols = any_of(cols_to_factor),
    .fns = as.factor
  ))


#### Remove correlating vars ------------------------------------------------------------------------------------------------
test_data_new <- test_data_new[, !names(test_data_new) %in% variables_to_remove_corr_num]


##### Subset the correct one --------------------------------------------------------------
test_data_new <- test_data_new %>% 
  select(- c( Micro_rating_Accessibility, Micro_rating_DistrictAndArea,
              Micro_rating_SunAndView, Micro_rating_ServicesAndNature))

####---------------------------------------------------------------------------------------------
# Apply the same preprocessing steps to the test data---------------------
test_data_preprocessed <- bake(data_recipe, new_data = test_data_new)

# Make predictions on the test set
xgb_pred_test <- predict(xgb_fit, new_data = test_data_preprocessed)

# If 'rent_full' is not present in the test_data, you can add the predictions to it
test_data_with_predictions <- bind_cols(test_data, .pred = xgb_pred_test$.pred)

# Now you can see the predicted values in the 'test_data_with_predictions' dataframe
# The predicted values are stored in the '.pred' column
