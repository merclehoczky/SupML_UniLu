library(readr)
library(dplyr)

library(rstudioapi)
library(outliers)
# Getting the path of your current open file
current_path = rstudioapi::getActiveDocumentContext()$path 
setwd(dirname(current_path ))
print( getwd() )

# Upload data
path_train <- "../data/final_data.csv"
path_test <- "../data/X_test.csv"
training_data <- read_csv(path_train)
x_test <- read_csv(path_test)


#### Price per sqm ----
# Price range
boxplot(training_data$rent_full)
hist(training_data$rent_full)

summary(training_data$rent_full)
sd(training_data$rent_full)

# Sqm range
boxplot(training_data$area)
hist(training_data$area) 

summary(training_data$area)
sd(training_data$area)

# Price per sqm
plot(x = training_data$rent_full, y = training_data$area, 
     xlab = "Price", ylab = "Area (sqm)", main = "Price per sqm")

# Remove outliers (IQR)
#replace_with_thresholds_iqr()
#rent_full

# Calculate the IQR and lower/upper bounds
iqr_value <- IQR(training_data$rent_full)
lower_bound <- quantile(training_data$rent_full, 0.25) - 1.5 * iqr_value
upper_bound <- quantile(training_data$rent_full, 0.75) + 1.5 * iqr_value

# Replace outliers with NA
training_data$rent_full[training_data$rent_full < lower_bound | training_data$rent_full > upper_bound] <- NA


#area
iqr_value <- IQR(training_data$area)
lower_bound <- quantile(training_data$area, 0.25) - 1.5 * iqr_value
upper_bound <- quantile(training_data$area, 0.75) + 1.5 * iqr_value

# Replace outliers with NA
training_data$area[training_data$area < lower_bound | training_data$area > upper_bound] <- NA


#### Area-room ----
summary(training_data$rooms)
sd(training_data$rooms)
hist(training_data$rooms)
boxplot(training_data$rooms)

plot(x = training_data$area, y = training_data$rooms, 
     xlab = "Area (sqm)", ylab ="Number of rooms", main = "Number of rooms vs area")

# Remove where 3+ rooms on 30 sqm

# Replace rooms with NA 
training_data$rooms[training_data$rooms >= 3 & training_data$area <= 30] <- NA


#### year_built ----
summary(training_data$year_built)
hist(training_data$year_built)
boxplot(training_data$year_built)
sd(training_data$year_built)

# Create categories


