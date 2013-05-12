#######################################
# Generate poverty levels             #
#                                     #
# Author: Elijah Goldberg             #
# Contact: goldberg.elijah@gmail.com  #
#######################################

# This calculates the rate of poverty based on SIPP data
# Not necessary to conduct analysis, computed values are available at "poverty.csv"

# Load survey design library
library(survey)

#######################################
# Create master frame of SIPP data    #
#######################################

# Replace with correct directory
dir <- "C:/Users/Elijah Goldberg/Dropbox/Documents/Research/Poverty Prediction/new/data/"
setwd(dir)


# Load list of all months covered by SIPP data
months <- read.csv("months.csv", header = F)
waves <- read.csv("waves.csv", header = T)

# SIPP columsn: "SSUSEQ","SREFMON","RHCALMN","RHCALYR","WHFNWGT","EFNP","TFTOTINC","RFPOV","EENTAID","EPPPNUM","EPPINTVW","TPTOTINC"


for(i in 1:nrow(waves)) {
  
  # Set the path to the SIPP wave
  path = paste(dir, "poverty/wave0", waves[i,1], ".", waves[i,2], sep = "")
  setwd(path)
  print(path)
  
  # Get all files in repository
  f <- list.files(".")
  for(j in 1:length(f)) {
    print(f[j])
    
    # Read data from file
    temp <- read.csv(f[j], header = T)
    
    # Create frame for this wave if doesn't exist
    if(!exists('dat')) {
      dat <- temp
    } else {
      dat <- rbind(dat, temp)
    }
    
    print(paste("Read section ", j, " of 0", waves[i,1], " wave ", waves[i,2], sep=""))
    rm(temp)
  }
  
  # Create master frame of all waves if doesn't exist
  if(!exists('master')) {
    master <- dat
  } else {
    master <- rbind(master, dat)
  }
  print("Wave merged")
  rm(dat)
}
rm(f, i, j, path, w)


#######################################
# Generate poverty status             #
#######################################

# CALCULATE POVERTY STATUS
# TRUE if observation total income less than poverty line
master$INPOV <- master$TFTOTINC < master$RFPOV

# SET DUMMY VARIABLE
master$DUMMY <- 1

# CREATE SURVEY DESIGN
# id: cluster id
# strata: data stratification
# weights: weighting of each observation
  # FYI: clusters & stratas don't change observed values, just standard errors, which is why manual
  # calculation below without them still works.  Weighting does change observed values.
# nest = TRUE: forces clusters to nest within strata
# This will take a while...
mclus <- svydesign(id = ~GHLFSAM, strata = ~GVARSTR, weights = ~WHFNWGT, data = master, nest = TRUE)

# CALCULATE RATIO IN POVERTY
povRate <- svyratio(~INPOV, ~DUMMY, mclus)

# GENERATE RATIO MANUALLY
# ... to check svydesign is working
observations <- nrow(master) # Number of observations
weighted <- sum(master[,'WHFNWGT']) # Weighted total of observations
p <- master[(master$TFTOTINC < master$RFPOV),c('WHFNWGT')] # Weights of all individuals in poverty
povObsWeighted <- sum(p) # Total weight of individuals in poverty
povRate.manual <- povObsWeighted / weighted

povRate
povRate.manual
# They're the same!
rm(observations, weighted, p, povObsWeighted, povRate, povRate.manual)


# CALCULATE POVERTY RATES BY MONTH

# Create a data frame with the necessary columns
# Year, month, number of observations, weighted total of observations, calculated poverty rate, poverty rate standard error, poverty rate confidence interval high, poverty rate confidence interval low
pov <- data.frame(year = numeric(), month = numeric(), 
                  observations = numeric(), weighted = numeric(), 
                  povRate = numeric(), povSE = numeric(),
                  povCIH = numeric(), povCIL = numeric())

# Get list of dates
y <- data.frame(master$RHCALYR, master$RHCALMN)
frames <- unique(y); row.names(frames) <- NULL
colnames(frames) <- c("year", "month")
frames <- frames[order(frames[,2]),]; frames <- frames[order(frames[,1]),]
rm(y)

for(j in 1:nrow(frames)) {
  mn <- frames[j,]$month
  yr <- frames[j,]$year
  observations <- nrow(master[(master$RHCALMN == mn & master$RHCALYR == yr),]) # Total obs for that frame
  weighted <- sum(master[(master$RHCALMN == mn & master$RHCALYR == yr),'WHFNWGT']) # Weighted frame obs
  p <- svyratio(~INPOV, ~DUMMY, subset(mclus, (RHCALMN == mn & RHCALYR == yr))) # Frame poverty ratio
  povRate <- as.numeric(p[1]) # Get the poverty rate
  povSE <- as.numeric(SE(p)) # Get the standard error for the poverty rate
  c <- confint(p, level = .9) # Calculate a 90% confidence interval
  povCIH <- c[2]
  povCIL <- c[1]
  pov[j,] <- c(yr, mn, observations, weighted, povRate, povSE, povCIH, povCIL) # Add it all to pov dframe
  print(paste(yr, mn, povRate))
  rm(mn, yr, observations, weighted, p, povRate, povSE, c, povCIH, povCIL)
}
rm(j)





setwd(dir)

# Write to file
write.table(pov, "poverty.csv", sep=",")