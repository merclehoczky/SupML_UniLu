#### Preprocessing ###########
df <- data
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

# Drop vars with NA over threshold is 80%
threshold <- 0.8
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
  select(-key)

# Remove date columns
df <- df %>% 
  select(-date, month )

# Remove geographical columns
df <- df %>% 
  select(-GDENAMK, GDENR, address, lat, lon, msregion, quarter_general, quarter_specific)

# Remove description
df <- df %>% 
  select(-descr)

# Data types ----------------------------------------------------------
df_variable_types <- sapply(df, function(col) class(col))

print(df_variable_types)
str(df)
# Sort out 

# Categoricals
df <- df %>% 
  mutate(home_type = as.factor(home_type))

         
         
         