# Master Script

# Add data to temporary variable --------------------------
#data <-  training_data

# Run distances scripts with Google API
# Run script to set up Google API usage and functions
source("201_distance.R")

# Run script that calculates distance to nearest lake
source("202_dist_lake.R")

# Run script that calculates distance to nearest primary school
source("203_dist_school.R")

# Run script that calculates distance to nearest public transport stop
source("204_nearpublic.R")

# Run script that calculates distance to nearest train station
source("205_totrain.R")

# Save dataset ------------------
write.csv(data, "../data/after_api.csv", row.names = FALSE)

