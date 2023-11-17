library(readr)
library(dplyr)
library(ggplot2)
library(caret)
library(randomForest)
library(glmnet)
library(corrplot)
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

# Data types of each column
str(training_data)
# Summary statistics for numerical variables
summary(training_data %>% select_if(is.numeric))
# Check categorical vars
cat <- sapply(training_data, is.character)

# Distribution of rent prices
ggplot(training_data, aes(x = rent_full)) +
  geom_histogram(binwidth = 100, fill = "blue", color = "black") +
  theme_minimal() +
  labs(title = "Distribution of Rent Full", x = "Rent Full", y = "Frequency")
#
ggplot(training_data, aes(x = rent_full)) +
  geom_density(fill = "blue", alpha = 0.5) +
  theme_minimal() +
  labs(title = "Density of Rent Full", x = "Rent Full", y = "Density")

# Mean price per canton
mean_rent <- aggregate(rent_full ~ KTKZ, data = training_clean, mean)
ggplot(mean_rent, aes(x = KTKZ, y = rent_full)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(title = "Mean Rent per Canton (KTKZ)", x = "Canton (KTKZ)", y = "Mean Rent (rent_full)")
#### Cor plot ----
# Numerical relevant columns
numer <- c("rent_full", "Micro_rating", "Micro_rating_NoiseAndEmission", 
           "Micro_rating_Accessibility", "Micro_rating_DistrictAndArea", 
           "Micro_rating_SunAndView", "Micro_rating_ServicesAndNature", "Noise_max", "apoth_pix_count_km2", 
           "dist_to_4G", "dist_to_5G", "dist_to_train_stat", "restaur_pix_count_km2", 
           "superm_pix_count_km2", "dist_to_river")

cor_matrix <- cor(training_data[numer], use = "complete.obs")
corrplot(cor_matrix, method = "circle")

#### Lasso ----
# Converting specified columns to factors
training_data <- training_data %>% 
  mutate(KTKZ = as.factor(KTKZ),
         newly_built = as.factor(newly_built),
         home_type = as.factor(home_type))
str(training_data$KTKZ)
str(training_data$newly_built)

data_matrix <- model.matrix(rent_full ~ msregion + home_type + newly_built  + 
                              Micro_rating + Micro_rating_NoiseAndEmission + Micro_rating_Accessibility + 
                              Micro_rating_DistrictAndArea + Micro_rating_SunAndView + 
                              Micro_rating_ServicesAndNature + apoth_pix_count_km2 + 
                              dist_to_4G + dist_to_5G + dist_to_train_stat + restaur_pix_count_km2 + 
                              superm_pix_count_km2 + dist_to_river, 
                            data = training_clean) # + tried with cantons instead of msregion

# Lasso regression with cross-validation
set.seed(123) # for reproducibility
cv_lasso <- cv.glmnet(data_matrix, training_clean$rent_full, alpha = 1, type.measure = "mse", nfolds = 10)
best_lambda <- cv_lasso$lambda.min
lasso <- glmnet(data_matrix, training_clean$rent_full, alpha = 1, lambda = best_lambda)
plot(cv_lasso)
# Extracting the coefficients of the model
selected_var <- coef(lasso, s = best_lambda)
# Splitting the training data into train and test sets for model validation

index <- createDataPartition(training_data$rent_full, p = 0.8, list = FALSE)
train_set <- training_data[index, ]
test_set <- training_data[-index, ]
lm_model <- train(rent_full ~ msregion + home_type + newly_built  + 
                    Micro_rating_NoiseAndEmission + Micro_rating_Accessibility + 
                    Micro_rating_DistrictAndArea + Micro_rating_SunAndView + 
                    Micro_rating_ServicesAndNature + 
                    dist_to_4G + dist_to_5G + dist_to_train_stat + restaur_pix_count_km2 + 
                    superm_pix_count_km2 + dist_to_river, data = train_set, method = "lm")
summary(lm_model)