#### Preprocessing ###########
library(dplyr)

df <- data
#df <- test_after_all

# List all NA variables ------------------------------------------------------
all_na_vars <- sapply(df, function(col) all(is.na(col)))
all_na_var_names <- names(all_na_vars)[all_na_vars]
print(all_na_var_names)

# Drop all NA variables and create new variable df
df <- df[, !all_na_vars]


# List vars with NA ------------------------
# Assuming your dataframe is named 'df'
na_counts <- colSums(is.na(df))

# Create a data frame for variables with NAs and their counts
na_data <- data.frame(
  Variable = names(na_counts),
  NAs = na_counts
)

# Print the table
print(na_data)

# Drop vars with NA over threshold is 80%----
# Set the threshold 
threshold <- 0.8
# Identify variables
variables_to_remove_NA_threshold <- colnames(df) [colMeans(is.na(df)) > threshold]

# Print the names of variables to be removed
cat( paste(variables_to_remove_NA_threshold, collapse = ", "), "\n")

# Create a new data frame without the removed variables
df <- df[, colMeans(is.na(df)) <= threshold]


# List all no variability variables ------------------------------------------
no_variability_vars <- sapply(df, function(col) length(unique(col, na.rm = TRUE)) == 1)
no_variability_var_names <- names(no_variability_vars)[no_variability_vars]
print(no_variability_var_names)

# Drop no variability vars 
df <- df[, !no_variability_vars]



# Remove  variables  -----------------------------------------------------------
# Remove ID columns 
df <- df %>% 
  select(-c(key))

df <- df %>% 
  select(-c('...1'))

# Remove date columns
df <- df %>% 
  select(-c(date, month ))

# Remove geographical columns
df <- df %>% 
  select(-c(GDENAMK, GDENR, address, lat, lon, msregion, quarter_general, quarter_specific))

# Remove description
df <- df %>% 
  select(-descr)

# Remove year_built since there is is_new and avg_bauperiode
df <- df %>% 
  select(-year_built)

# Data types ----------------------------------------------------------
df_variable_types <- sapply(df, function(col) class(col))

print(df_variable_types)
str(df)
# Sort out 

# Categoricals ----------------------
# List columns
cols_to_factorize <- c("home_type", "KTKZ", "raised_groundfloor")

# Apply as.factor to the selected columns
df <- df %>% 
  mutate(across(
    .cols = all_of(cols_to_factorize),
    .fns = as.factor
  ))

# Factorize vars again ----------------------
cols_to_factor <-  c("parking", "balcony", "basement", "dishwasher", "dryer", "minergie", 
                     "laundry", "oven", "elevator", "furnished", "pets", "pool", "shared_flat", 
                     "kids_friendly", "cabletv", "cheminee", "playground", "wheelchair", "is_new")

df <- df %>% 
  mutate(across(
    .cols = any_of(cols_to_factor),
    .fns = as.factor
  ))

