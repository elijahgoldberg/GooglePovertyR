#######################################
# Make predictions                    #
#                                     #
# Author: Elijah Goldberg             #
# Contact: goldberg.elijah@gmail.com  #
#######################################

# Set working directory
dir <- "C:/Users/Elijah Goldberg/Dropbox/Documents/Research/Poverty Prediction/new/data/"
setwd(dir)

# Load and prepare necessary data
pr <- read.table("master.csv", header = T)
pr$finalDate <- as.Date(pr$finalDate); pr$year <- as.integer(pr$year); pr$month <- as.integer(pr$month)
pr$r1 = c(NA, pr$rate[1:(nrow(pr)-1)]);
pr$r12 = c(rep(NA, 12), pr$rate[1:(nrow(pr)-12)]);

#######################################
# Load models                         #
#######################################

dir("../models")
load("../models/m.ar.manual.rda")

