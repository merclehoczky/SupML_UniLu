library(readr)
library(dplyr)

library(rstudioapi)

# Getting the path of your current open file
current_path = rstudioapi::getActiveDocumentContext()$path 
setwd(dirname(current_path ))
print( getwd() )

# Upload data
path_train <- "../data/training_data.csv"
path_test <- "../data/X_test.csv"
training_data <- read_csv(path_train)
x_test <- read_csv(path_test)

View(training_data %>%
       summarise_all(~sum(is.na(.))))
