library(dplyr)
library(ggplot2)

# View NAs
View(training_data %>%
       summarise_all(~sum(is.na(.))))

# Data types of each column
str(training_data)

# Summary statistics for numerical variables ----------------------------
vars_num_names <- names(training_data)[sapply(training_data, is.numeric)]
summary(training_data %>% select_if(is.numeric))

# Check categorical vars
cat <- sapply(training_data, is.character)

# Plots --------------------------------------------------------------------
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
mean_rent <- aggregate(rent_full ~ KTKZ, data = training_data, mean)
ggplot(mean_rent, aes(x = KTKZ, y = rent_full)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(title = "Mean Rent per Canton (KTKZ)", x = "Canton (KTKZ)", y = "Mean Rent (rent_full)")