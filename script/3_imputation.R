#### Imputation ####

library(mice)
library(visdat)

# Visualizing missing data 
vis_miss(data, warn_large_data = FALSE)

########################################## is_new
# Column is_new building
data <- data %>%
  mutate(is_new = ifelse(year_built >= 2017 & year_built <= 2021, 1, 0))
data$is_new[is.na(data$is_new)] <- 0
sum(is.na(data$is_new))
# Remove other old, new building columns
data <- subset(data, select = -c(oldbuilding, newly_built))
data$is_new <- as.factor(data$is_new)

########################################## area, rooms, balcony
imputarea <- mice(data[, c("area", "rooms", "balcony")], method = 'pmm', m = 5, maxit = 5)
completed_area <- complete(imputarea)
# area
summary(completed_area$area)
data$area <- ifelse(is.na(data$area), completed_area$area, data$area)
sum(is.na(data$area))
#rooms
data$rooms <- ifelse(is.na(data$rooms), completed_area$rooms, data$rooms)
sum(is.na(data$rooms))

##########################################  anteil_efh, Avg_size_household, Avg_age, Anteil_auslaend
# Imputation for anteil_efh, Avg_size_household, Avg_age, Anteil_auslaend
imputed_anteil <- mice(data[, c("anteil_efh", "Avg_size_household", "Avg_age", "Anteil_auslaend")], method = 'pmm', m = 5, maxit = 5)
completed_anteil <- complete(imputed_anteil)
# Check results 
summary(data$Avg_age)
summary(completed_anteil$Avg_age)
data$anteil_efh <- ifelse(is.na(data$anteil_efh), completed_anteil$anteil_efh, data$anteil_efh)
data$Avg_age <- ifelse(is.na(data$Avg_age), completed_anteil$Avg_age, data$Avg_age)
data$Avg_size_household <- ifelse(is.na(data$Avg_size_household), completed_anteil$Avg_size_household, data$Avg_size_household)
data$Anteil_auslaend <- ifelse(is.na(data$Anteil_auslaend), completed_anteil$Anteil_auslaend, data$Anteil_auslaend)


########################################## avg_anzhl_geschosse, floors
# Imputation for avg_anzhl_geschosse
imputed_geschosse <- mice(data[, c("avg_anzhl_geschosse", "floors")], method = 'pmm', m = 5, maxit = 5)
completed_geschosse <- complete(imputed_geschosse)
# Check results 
summary(data$avg_anzhl_geschosse)
summary(completed_geschosse$avg_anzhl_geschosse)
data$avg_anzhl_geschosse <- ifelse(is.na(data$avg_anzhl_geschosse), completed_geschosse$avg_anzhl_geschosse, data$avg_anzhl_geschosse)

# floors
summary(data$floors)
summary(completed_geschosse$floors)
data$floors <- ifelse(is.na(data$floors), completed_geschosse$floors, data$floors)



########################################## wgh_avg_sonnenklasse_per_egid
# Imputation for wgh_avg_sonnenklasse_per_egid
imputed_sonnen <- mice(data[, c("wgh_avg_sonnenklasse_per_egid", "Micro_rating_SunAndView")], method = 'pmm', m = 5, maxit = 5)
completed_sonnen <- complete(imputed_sonnen)
# Check results 
summary(data$wgh_avg_sonnenklasse_per_egid)
summary(completed_sonnen$wgh_avg_sonnenklasse_per_egid)
data$wgh_avg_sonnenklasse_per_egid <- ifelse(is.na(data$wgh_avg_sonnenklasse_per_egid), 
                                             completed_sonnen$wgh_avg_sonnenklasse_per_egid, data$wgh_avg_sonnenklasse_per_egid)

############################################ bauperiode
imputed_bauperiode <- mice(data[, c("year_built", "avg_bauperiode", "GDENAMK")], method = 'pmm', m = 5, maxit = 5)
completed_bauperiode <- complete(imputed_bauperiode)
# Check results 
summary(data$avg_bauperiode)
summary(completed_bauperiode$avg_bauperiode)
data$avg_bauperiode <- ifelse(is.na(data$avg_bauperiode), completed_bauperiode$avg_bauperiode, data$avg_bauperiode)

############################################ raised_groundfloor
# Assuming no indication means negation: Turn NAs into 0
data$raised_groundfloor[is.na(data$raised_groundfloor)] <- 0
sum(is.na(data$raised_groundfloor))

############################################ water
# Assuming no indication means confirmation: Turn NAs into 1
data$water[is.na(data$water)] <- 1
sum(is.na(data$water))


############################################ shower
# Assuming no indication means confirmation: Turn NAs into 1
data$shower[is.na(data$shower)] <- 1
sum(is.na(data$shower))


############################################ toilets
# Assuming no indication means confirmation: Turn NAs into 1
data$toilets[is.na(data$toilets)] <- 1
sum(is.na(data$toilets))


# Save dataset ------------------
write.csv(data, "../data/after_imput.csv", row.names = FALSE)
