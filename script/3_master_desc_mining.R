#### Master Script for text mining from the variable "desc" ############### 

# Arrange WD -------------
library(rstudioapi)

# Getting the path of your current open file
current_path = rstudioapi::getActiveDocumentContext()$path 
setwd(dirname(current_path ))
print( getwd() )

# Add data to temporary variable --------------------------
#data <-  training_data

# Setup ----------------------------
# Run script to set up functions
source("100_textmining_setup.R")

# Run text mining scripts -------------------
# Run script that looks up parking
source("101_parking.R")

# Run script that looks up balcony, terrace and veranda
source("102_balcony.R")

# Run script that looks up basement
source("103_basement.R")

# Run script that looks up dishwasher
source("104_dishwasher.R")

# Run script that looks up dryer
source("105_dryer.R")

# Run script that looks up laundry
source("106_laundry.R")

# Run script that looks up oven (NA -> 1)
source("107_oven.R")

# Run script that looks up elevator
source("108_elevator.R")

# Run script that looks up furnished
source("109_furnished.R")

# Run script that looks up pets ( no NA change!)
source("110_pets.R")

# Run script that looks up pool
source("111_pool.R")

# Run script that looks up shared_flat (NA -> 0)
source("112_shared_flat.R")

# Run script that looks up kids_friendly (NA -> 1)
source("113_kids_friendly.R")

# Run script that looks up cable tv (NA -> 1)
source("114_cabletv.R")

# Run script that looks up cheminee
source("115_cheminee.R")

# Run script that looks up minergie
source("116_minergie.R")
