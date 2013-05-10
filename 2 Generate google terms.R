#######################################
# Generate Google Terms               #
#                                     #
# Author: Elijah Goldberg             #
# Contact: goldberg.elijah@gmail.com  #
#######################################


# Combines individual term files
# Not necessary to conduct analysis, computed values are available at "terms.csv"


#######################################
# Create master frame of terms        #
#######################################

# Replace with correct directory
dir <- "C:/Users/Elijah Goldberg/Dropbox/Documents/Research/Poverty Prediction/new/data/"
setwd(dir)

# Read all months until November 2012
pr <- read.csv("months.all.csv", header = F)

colnames(pr) <- c("date")
setwd("./terms/US") # Directory with US terms

# Get list of all terms we're using as predictors
f <- list.files(".")

# Cycle through all terms
for(j in 1:length(f)) {
  
  # Load predictor from CSV
  predictor <- read.csv(f[j], header = F)
  nc <- ncol(predictor)
  
  # Get some parameters about the data
  sp <- strsplit(f[j], "--")
  term <- strsplit(sp[[1]][2], "\\.")[[1]][1]
  state <- sp[[1]][1]
  
  # If data doesn't exist
  if(ncol(predictor) == 2) {
    V3 <- predictor[,2]
    predictor <- cbind(predictor, V3)
  }
  
  # Reset NAs with 0
  predictor$V3[is.na(predictor$V3)] <- 0
  
  # Reformat to add year column
  for(i in 1:nrow(predictor)) {
    val <- predictor[i,1]
    val <- strsplit(as.character(val), "/")[[1]]
    val <- paste(val[2], "-", val[1], sep="")
    if(nchar(val) != 7) {
      val <- strsplit(as.character(predictor[i,1]), "-")[[1]]
      val <- paste(val[1], "-", val[2], sep="")
    }
    predictor[i,4] <- val
    rm(val)
  }
  predictor <- predictor[,c(3, 4)]
  
  colnames(predictor) <- c("rate", "date")
  months = c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12")
  years = c(2004, 2005, 2006, 2007, 2008, 2009, 2010, 2011, 2012)
  # Add the predictor to the end of the rates data frame
  for(i in 1:length(years)) {
    for(k in 1:length(months)) {
      y <- years[i]
      m <- months[k]
      d <- paste(y, "-", m, sep="")
      r <- mean(predictor[predictor$date == d,]$rate)
      pr[pr$date == d[1],term] <- r
    }
  }
}

setwd(dir)
write.table(pr, "terms.csv", sep=",")