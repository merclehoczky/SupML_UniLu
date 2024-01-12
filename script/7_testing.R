

test_data_new <- x_test #18001


# Remove unusual, outliers-----------------------------------------------------

test_data_new <- test_data_new %>% 
  filter(area > 20, area < 381) # 56801

# Remove to old buildings
test_data_new <- test_data_new %>% 
  filter(year_built > 1800 | is.na(year_built)) #56053


#################### Imputation ----------------------------------------------------

################## is_new
# Column is_new building
test_data_new <- test_data_new %>%
  mutate(is_new = ifelse(year_built >= 2017 & year_built <= 2021, 1, 0))
test_data_new$is_new[is.na(test_data_new$is_new)] <- 0
sum(is.na(test_data_new$is_new))
# Remove other old, new building columns
test_data_new <- subset(test_data_new, select = -c(oldbuilding, newly_built))
test_data_new$is_new <- as.factor(test_data_new$is_new)

########################################## area, rooms, balcony
imputarea <- mice(test_data_new[, c("area", "rooms", "balcony")], method = 'pmm', m = 5, maxit = 5)
completed_area <- complete(imputarea)
# area
summary(completed_area$area)
test_data_new$area <- ifelse(is.na(test_data_new$area), completed_area$area, test_data_new$area)
sum(is.na(test_data_new$area))
# rooms
test_data_new$rooms <- ifelse(is.na(test_data_new$rooms), completed_area$rooms, test_data_new$rooms)
sum(is.na(test_data_new$rooms))

##########################################  anteil_efh, Avg_size_household, Avg_age, Anteil_auslaend
# Imputation for anteil_efh, Avg_size_household, Avg_age, Anteil_auslaend
imputed_anteil <- mice(test_data_new[, c("anteil_efh", "Avg_size_household", "Avg_age", "Anteil_auslaend")], method = 'pmm', m = 5, maxit = 5)
completed_anteil <- complete(imputed_anteil)
# Check results 
summary(test_data_new$Avg_age)
summary(completed_anteil$Avg_age)
test_data_new$anteil_efh <- ifelse(is.na(test_data_new$anteil_efh), completed_anteil$anteil_efh, test_data_new$anteil_efh)
test_data_new$Avg_age <- ifelse(is.na(test_data_new$Avg_age), completed_anteil$Avg_age, test_data_new$Avg_age)
test_data_new$Avg_size_household <- ifelse(is.na(test_data_new$Avg_size_household), completed_anteil$Avg_size_household, test_data_new$Avg_size_household)
test_data_new$Anteil_auslaend <- ifelse(is.na(test_data_new$Anteil_auslaend), completed_anteil$Anteil_auslaend, test_data_new$Anteil_auslaend)

########################################## avg_anzhl_geschosse, floors
# Imputation for avg_anzhl_geschosse
imputed_geschosse <- mice(test_data_new[, c("avg_anzhl_geschosse", "floors")], method = 'pmm', m = 5, maxit = 5)
completed_geschosse <- complete(imputed_geschosse)
# Check results 
summary(test_data_new$avg_anzhl_geschosse)
summary(completed_geschosse$avg_anzhl_geschosse)
test_data_new$avg_anzhl_geschosse <- ifelse(is.na(test_data_new$avg_anzhl_geschosse), completed_geschosse$avg_anzhl_geschosse, test_data_new$avg_anzhl_geschosse)

# floors
summary(test_data_new$floors)
summary(completed_geschosse$floors)
test_data_new$floors <- ifelse(is.na(test_data_new$floors), completed_geschosse$floors, test_data_new$floors)

########################################## wgh_avg_sonnenklasse_per_egid
# Imputation for wgh_avg_sonnenklasse_per_egid
imputed_sonnen <- mice(test_data_new[, c("wgh_avg_sonnenklasse_per_egid", "Micro_rating_SunAndView")], method = 'pmm', m = 5, maxit = 5)
completed_sonnen <- complete(imputed_sonnen)
# Check results 
summary(test_data_new$wgh_avg_sonnenklasse_per_egid)
summary(completed_sonnen$wgh_avg_sonnenklasse_per_egid)
test_data_new$wgh_avg_sonnenklasse_per_egid <- ifelse(is.na(test_data_new$wgh_avg_sonnenklasse_per_egid), 
                                             completed_sonnen$wgh_avg_sonnenklasse_per_egid, test_data_new$wgh_avg_sonnenklasse_per_egid)

############################################ bauperiode
imputed_bauperiode <- mice(test_data_new[, c("year_built", "avg_bauperiode", "GDENAMK")], method = 'pmm', m = 5, maxit = 5)
completed_bauperiode <- complete(imputed_bauperiode)
# Check results 
summary(test_data_new$avg_bauperiode)
summary(completed_bauperiode$avg_bauperiode)
test_data_new$avg_bauperiode <- ifelse(is.na(test_data_new$avg_bauperiode), completed_bauperiode$avg_bauperiode, test_data_new$avg_bauperiode)

############################################ raised_groundfloor
# Assuming no indication means negation: Turn NAs into 0
test_data_new$raised_groundfloor[is.na(test_data_new$raised_groundfloor)] <- 0
sum(is.na(test_data_new$raised_groundfloor))

############################################ water
# Assuming no indication means confirmation: Turn NAs into 1
test_data_new$water[is.na(test_data_new$water)] <- 1
sum(is.na(test_data_new$water))

############################################ shower
# Assuming no indication means confirmation: Turn NAs into 1
test_data_new$shower[is.na(test_data_new$shower)] <- 1
sum(is.na(test_data_new$shower))


############################################ toilets
# Assuming no indication means confirmation: Turn NAs into 1
test_data_new$toilets[is.na(test_data_new$toilets)] <- 1
sum(is.na(test_data_new$toilets))

# List all NA variables ------------------------------------------------------

# Drop all NA variables and create new variable test_data_new
test_data_new <- test_data_new[, !names(test_data_new) %in% all_na_var_names]


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

test_data_new <- test_data_new[, !names(test_data_new) %in% no_variability_var_names]

# Fix datatypes

sapply(test_data_new, is.numeric)
sapply(test_data_new, is.factor)


test_data_new[, c("msregion", "home_type")] <- lapply(test_data_new[, c("msregion", "home_type")], as.factor)

#remove under 6% variability
test_data_new <- test_data_new[, !names(test_data_new) %in% variables_to_remove_undersix]


# Remove  variables  -----------------------------------------------------------
# Remove ID columns 
test_data_new <- test_data_new %>% 
  select(-c(key))

test_data_new <- test_data_new %>% 
  select(-starts_with('..'))

# Remove date columns
test_data_new <- test_data_new %>% 
  select(-c(date, month ))

# Remove geographical columns
test_data_new <- test_data_new %>% 
  select(-c(GDENAMK, GDENR, address, lat, lon, KTKZ, quarter_general, quarter_specific))

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
cols_to_factorize <- c("home_type", "msregion")#, "raised_groundfloor")

# Apply as.factor to the selected columns
test_data_new <- test_data_new %>% 
  mutate(across(
    .cols = all_of(cols_to_factorize),
    .fns = as.factor
  ))

# Factorize vars again ----------------------
cols_to_factor <-  c("parking", "balcony",  "basement", "dishwasher", "dryer", "minergie", 
                     "laundry", "oven", "elevator", "furnished", "pets", "pool", "shared_flat", 
                     "kids_friendly", "cabletv", "cheminee", "playground", "wheelchair", "is_new")

test_data_new <- test_data_new %>% 
  mutate(across(
    .cols = any_of(cols_to_factor),
    .fns = as.factor
  ))


#### Remove correlating vars ------------------------------------------------------------------------------------------------
#test_data_new <- test_data_new[, !names(test_data_new) %in% variables_to_remove_corr_num]


##### Subset the correct one --------------------------------------------------------------
#test_data_new <- test_data_new %>% 
#  select(- c( Micro_rating_Accessibility, Micro_rating_DistrictAndArea,
#              Micro_rating_SunAndView, Micro_rating_ServicesAndNature))

####---------------------------------------------------------------------------------------------



# Apply the same preprocessing steps to the test data---------------------
#test_data_new_preprocessed <- bake(data_recipe, new_data = test_data_new)


# Make predictions on the test set
xgb_pred_test <- predict(xgb_fit, new_data = test_data_new)

summary(xgb_pred_test)

#Create result table
test_data_with_predictions <- bind_cols(x_test$key, .pred = xgb_pred_test$.pred)

# Assuming 'key' is the key column in test_data
test_data_with_predictions <- bind_cols(
  key = x_test$key,
  rent = xgb_pred_test$.pred
)

# Save result file

write.csv(test_data_with_predictions, file = '../result/final_predictions_Lehoczky_Tantsiura.csv', row.names = FALSE)

