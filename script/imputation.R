library(mice)
library(visdat)

# Visualizing missing data 
vis_miss(data, warn_large_data = FALSE)

##########################################
# Column new_old building
data <- data %>%
  mutate(new_old = ifelse(year_built >= 2017 & year_built <= 2021, 1, 0))
data$new_old[is.na(data$new_old)] <- 0
sum(is.na(data$new_old))
# Remove other old, new building columns
data <- subset(data, select = -c(oldbuilding, newly_built))

##########################################
imputarea <- mice(data[, c("area", "rooms", "balcony")], method = 'pmm', m = 5, maxit = 5)
completed_area <- complete(imputarea)
summary(completed_area$area)
data$area <- completed_area$area
data$rooms <- completed_area$rooms
sum(is.na(data$area))
sum(is.na(data$rooms))

##########################################
# Imputation for anteil_efh, Avg_size_household, Avg_age, Anteil_auslaend
imputed_anteil <- mice(data[, c("anteil_efh", "Avg_size_household", "Avg_age", "Anteil_auslaend")], method = 'pmm', m = 5, maxit = 5)
completed_anteil <- complete(imputed_anteil)
# Check results 
summary(data$Avg_age)
summary(completed_anteil$Avg_age)
data$anteil_efh <- completed_anteil$anteil_efh
data$Avg_age <- completed_anteil$Avg_age
data$Avg_size_household <- completed_anteil$Avg_size_household
data$Anteil_auslaend <- completed_anteil$Anteil_auslaend
##########################################
# Imputation for avg_anzhl_geschosse
imputed_geschosse <- mice(data[, c("avg_anzhl_geschosse", "floors")], method = 'pmm', m = 5, maxit = 5)
completed_geschosse <- complete(imputed_geschosse)
# Check results 
summary(data$avg_anzhl_geschosse)
summary(completed_geschosse$avg_anzhl_geschosse)
data$avg_anzhl_geschosse <- completed_geschosse$avg_anzhl_geschosse
##########################################
# Imputation for wgh_avg_sonnenklasse_per_egid
imputed_sonnen <- mice(data[, c("wgh_avg_sonnenklasse_per_egid", "Micro_rating_SunAndView")], method = 'pmm', m = 5, maxit = 5)
completed_sonnen <- complete(imputed_sonnen)
# Check results 
summary(data$wgh_avg_sonnenklasse_per_egid)
summary(completed_sonnen$wgh_avg_sonnenklasse_per_egid)
data$wgh_avg_sonnenklasse_per_egid <- completed_sonnen$wgh_avg_sonnenklasse_per_egid

############################################
imputed_bauperiode <- mice(data[, c("year_built", "avg_bauperiode", "GDENAMK")], method = 'pmm', m = 5, maxit = 5)
completed_bauperiode <- complete(imputed_bauperiode)
# Check results 
summary(data$avg_bauperiode)
summary(completed_bauperiode$avg_bauperiode)
data$avg_bauperiode <- completed_sonnen$avg_bauperiode


############################################
