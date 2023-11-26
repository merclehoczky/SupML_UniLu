# Master Script

# Add data to temporary variable --------------------------
#data <-  training_data

# Run distances scripts with Google API
# Run script to set up Google API usage and functions
source("distance.R")

# Run script that calculates distance to nearest lake
source("dist_lake.R")

# Run script that calculates distance to nearest primary school
source("dist_school.R")

# Run script that calculates distance to nearest public transport stop
source("nearpublic.R")

# Run script that calculates distance to nearest train station
source("totrain.R")

# Save dataset ------------------
write.csv(data, "../data/after_api.csv", row.names = FALSE)

