#######################################
# Exploratory data analysis           #
#                                     #
# Author: Elijah Goldberg             #
# Contact: goldberg.elijah@gmail.com  #
#######################################

# Set working directory
dir <- "C:/Users/Elijah Goldberg/Dropbox/Documents/Research/Poverty Prediction/new/data/"
setwd(dir)

# Load necessary data
t <- read.csv("terms.csv", header = T)
p <- read.csv("poverty.csv", header = T)


#######################################
# Initial data manipulation           #
#######################################

# Adding and removing some columns, dropping missing values

for(i in 1:nrow(t)) {
  d <- t[i,]$date
  d <- strsplit(as.character(d), "-")
  t[i,'year'] <- d[[1]][1]
  t[i,'month'] <- d[[1]][2]
}
c <- t
d <- t[,(names(t) %in% c("year","month","date"))]
for(i in 1:nrow(p)) {
  y <- p[i,]$year
  m <- as.character(p[i,]$month)
  if(nchar(m) == 1) {
    m <- paste("0", m, sep = "")
  }
  r <- p[i,]$povRate
  r2 <- p[i,]$povCIH
  r3 <- p[i,]$povCIL
  c[(c$year == y & c$month == m),'rate'] <- r
  d[(d$year == y & d$month == m),'rate'] <- r
  d[(d$year == y & d$month == m),'upper'] <- r2
  d[(d$year == y & d$month == m),'lower'] <- r3
}

pr <- c[complete.cases(c),] # predictors
pov <- d[complete.cases(d),] # poverty rate
rm(c, p, t, d, i, m, r, y)

write.table(pr, file="master.csv")


#######################################
# EDA                                 #
#######################################


# PLOT POVERTY RATE BY MONTH
pov$fullDate <- paste(pov$date, "01", sep="-"); pov$fullDate <- as.Date(pov$fullDate, "%Y-%m-%d")
windows()
plot(pov$fullDate, pov$rate, main="Monthly poverty rate with confidence intervals, 2004-2011", 
     ylab="Poverty rate", xlab="Date", pch=19, col="dodgerblue4", fg="gray85", bg="gray40",
     col.lab="gray20", col.main="gray20", col.axis="gray20")
points(pov$fullDate, pov$upper, pch=20, col="lightblue3")
points(pov$fullDate, pov$lower, pch=20, col="lightblue3")

# Smoothed plot
lines(lowess(pov$rate ~ pov$fullDate,f=.5), col = "dodgerblue4", lwd=3)
lines(lowess(pov$upper ~ pov$fullDate,f=.5), col = "lightblue3", lwd=2)
lines(lowess(pov$lower ~ pov$fullDate,f=.5), col = "lightblue3", lwd=2)


# PLOT SELECTED TERMS
pr$finalDate <- pov$fullDate
windows()
par(mfrow=c(2,2))
plot(pr$finalDate, pr$foodStamps, main="Trends for query 'food stamps'", ylab="Percentage change",
     col="dodgerblue4", fg="gray85", bg="gray40",
     col.lab="gray20", col.main="gray20", col.axis="gray20")
lines(lowess(pr$foodStamps ~ pr$finalDate,f=.5), col = 2)
plot(fastFoodJob ~ finalDate, pr, main="Trends for query 'fast food job'", ylab="Percentage change",
     col="dodgerblue4", fg="gray85", bg="gray40",
     col.lab="gray20", col.main="gray20", col.axis="gray20")
lines(lowess(pr$fastFoodJob ~ pr$finalDate,f=.5), col = 2)
plot(freeClinic ~ finalDate, pr, main="Trends for query 'free clinic'", ylab="Percentage change",
     col="dodgerblue4", fg="gray85", bg="gray40",
     col.lab="gray20", col.main="gray20", col.axis="gray20")
lines(lowess(pr$freeClinic ~ pr$finalDate,f=.5), col = 2)
plot(getOutOfDebt ~ finalDate, pr, main="Trends for query 'get out of debt'", ylab="Percentage change",
     col="dodgerblue4", fg="gray85", bg="gray40",
     col.lab="gray20", col.main="gray20", col.axis="gray20")
lines(lowess(pr$getOutOfDebt ~ pr$finalDate,f=.5), col = 2)


windows()
pairs(pr[,c(5:7,62)], col="dodgerblue4", fg="gray85", bg="gray40",
      col.lab="gray20", col.main="gray20", col.axis="gray20")



# PLOT AUTOCORRELATION OF POVERTY RATES
windows()
acf(log(pr[,62]), main="Time lag plot illustrating the time correlation of poverty rates", 
    xlab="Months after observation", ylab="Autocorrelation between observation and following months", 
    col="dodgerblue4", fg="gray85", bg="gray40",
    col.lab="gray20", col.main="gray20", col.axis="gray20")


# PLOT AUTOCORRELATION OF PREDICTOR TERMS
windows()
par(mfrow=c(2,1))
acf(log(pr[,10]), main="Autocorrelation of 'cheapApartment'", col="dodgerblue4", fg="gray85", bg="gray40",
    col.lab="gray20", col.main="gray20", col.axis="gray20")
acf(log(pr[,35]), main="Autocorrelation of 'homelesShelter'",col="dodgerblue4", fg="gray85", bg="gray40",
    col.lab="gray20", col.main="gray20", col.axis="gray20")
