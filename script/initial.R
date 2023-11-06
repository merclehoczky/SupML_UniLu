library(readr)
library(dplyr)

# Upload data
path_train <- "/Users/viktoriiatantsiura/Downloads/University_Luzern/3 Semester/Machine learning/Final Project/training_data.csv"
path_test <- "/Users/viktoriiatantsiura/Downloads/University_Luzern/3 Semester/Machine learning/Final Project/X_test.csv"
training_data <- read_csv(path_train)
x_test <- read_csv(path_test)

View(training_data %>%
       summarise_all(~sum(is.na(.))))