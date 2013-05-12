#######################################
# Data analysis                       #
#                                     #
# Author: Elijah Goldberg             #
# Contact: goldberg.elijah@gmail.com  #
#######################################

# Load necessary libraries
library(survey)
library(rpart)
library(rpart.plot)

# Create RMSE function for comparing methods
rmse <- function(yhat,y) sqrt(mean((yhat-y)^2))

# Set working directory
dir <- "C:/Users/Elijah Goldberg/Dropbox/Documents/Research/Poverty Prediction/new/data/"
setwd(dir)

# Load and prepare necessary data
pr <- read.table("master.csv", header = T)
pr$finalDate <- as.Date(pr$finalDate); pr$year <- as.integer(pr$year); pr$month <- as.integer(pr$month)
pr$r1 = c(NA, pr$rate[1:(nrow(pr)-1)]);
pr$r12 = c(rep(NA, 12), pr$rate[1:(nrow(pr)-12)]);

#######################################
# Generate train and test sets        #
#######################################

pr.possible <- pr[-(1:12),-c(1,60,61)] # Remove factors that don't have previous yearly data
n <- nrow(pr.possible)
set.seed(1) # Make results reproducible
smpl <- sample(1:n, n*.80)
pr.train <- pr.possible[smpl,]
pr.test <- pr.possible[-smpl,]

#######################################
# Generate results table              #
#######################################

models <- c("Autoregressive, full", "Autoregressive, stepped", "Autoregressive, manual", "Tree", "PCA", "Selective AR")
rmses <- c("Train", "Test")

results <- data.frame(matrix(nrow=length(models),ncol=length(rmses)))
colnames(results) <- rmses
rownames(results) <- models

rm(models, rmses)


#######################################
# Fit seasonal autoregressive         #
# Full model                          #
#######################################

# Seasonal auto-regressive model, as demonstrated by Choi and Varian
pr.fit <- lm(rate ~ . , pr.train)
summary(pr.fit)

pr.predict <- predict(pr.fit, pr.test)

# Calculate, save RMSE
results[1,1] <- rmse(pr.fit$fit, pr.train$rate)
results[1,2] <- rmse(pr.predict, pr.test$rate) # This is pretty high: not a good predictive model

rm(pr.fit, pr.predict)


#######################################
# Fit seasonal autoregressive         #
# Stepped variable selection          #
#######################################

pr.fit <- lm(rate ~ . , pr.train)
summary(pr.fit)

# Remove non-significant variables
pr.stepped1 <- step(pr.fit)
pr.stepped2 <- step(pr.stepped1, direction="both") 
pr.stepped3 <- step(pr.fit, direction="both", k=log(nrow(pr)))

# Select highest r-squared
c <- c(summary(pr.stepped1)[8],
       summary(pr.stepped2)[8],
       summary(pr.stepped3)[8])
max <- which.max(c)

# Diagnostic plot
windows()
par(mfrow=c(2,2))
plot(pr.stepped1)

# Test predictive power
pr.predict <- predict(pr.stepped1, pr.test)
windows()
plot(pr$finalDate, pr$rate, main="Predicted vs actual")
lines(lowess(pr$rate ~ pr$finalDate,f=.5), col = 1)
points(pr.test$finalDate, pr.test$rate, col=2, pch=2)
lines(lowess(pr.test$rate ~ pr.test$finalDate,f=.5), col = 2)
points(pr.test$finalDate, pr.predict, col=3, pch=3)
lines(lowess(pr.predict ~ pr.test$finalDate,f=.5), col = 3) # Not a very good fit...

# Calculate, save RMSE
results[2,1] <- rmse(pr.stepped1$fit, pr.train$rate)
results[2,2] <- rmse(pr.predict, pr.test$rate) # This is pretty high, as expected

rm(c, max, n, pr.fit, pr.predict, pr.stepped1, pr.stepped2, pr.stepped3)


#######################################
# Fit seasonal autoregressive         #
# Manual variable selection           #
#######################################

pr.fit <- lm(rate ~ . , pr.train)
summary(pr.fit)

# Remove variables (pseudo-)manually, removing the variable with the highest p-value first
repeat {
  pvals <- summary(pr.fit)$coefficients[,4]
  if(max(pvals) > .1) {
    formula <- paste(". ~ . - ", names(which.max(pvals)), sep="")
    pr.fit <- update(pr.fit, formula)
  }
  else {
    break
  }
}
summary(pr.fit)

pr.fit.nr <- update(pr.fit, . ~ .)

pr.predict <- predict(pr.fit.nr, pr.test)
pr.train.predict <- predict(pr.fit.nr, pr.train)

windows()
plot(pr$finalDate, pr$rate, main="Prediction vs actual")
points(pr.test$finalDate, pr.test$rate, col=2, pch=2)
points(pr.test$finalDate, pr.predict, col=3, pch=3)

# Calculate, save RMSE
results[3,1] <- rmse(pr.fit.nr$fit, pr.train$rate)
results[3,2] <- rmse(pr.predict, pr.test$rate) # Now we're getting somewhere


#######################################
# Fit tree                            #
#######################################

pr.tr <- rpart(rate ~ ., pr.train,cp=.000000001)
plotcp(pr.tr)
printcp(pr.tr)

pr.tr1 <- prune(pr.tr, cp=.0001)
pr.tr1


train.hat <- predict(pr.tr1, pr.train)
results[4,1] <- rmse(train.hat, pr.train$rate)

test.hat <- predict(pr.tr1, pr.test)
results[4,2] <- rmse(test.hat, pr.test$rate)


windows()
par(mar=c(0,0,0,0))
rpart.plot(pr.tr1, main="Tree structure", split.col="gray40", branch.col="dodgerblue4", box.col="white",
           border.col="dodgerblue4", under.col="blue", nn.box.col="white", varlen=21)

rm(pr.tr, pr.tr1, test.hat, train.hat, n)


#######################################
# Principal component analysis        #
#######################################

tr.pc <- prcomp(pr.train[,c(-60, -59)])
windows()
plot(tr.pc$sdev)
summary(tr.pc)

matplot(1:60, tr.pc$rot[,1:4],type="l")

m.1 <- lm(rate ~ tr.pc$x[,1:2], pr.train)
summary(m.1)
r.train.1 <- rmse(m.1$fit, pr.train$rate)

m.2 <- lm(rate ~ tr.pc$x[,1:4], pr.train)
summary(m.2)
r.train.2 <- rmse(m.2$fit, pr.train$rate)

m.3 <- lm(rate ~ tr.pc$x[,1:20], pr.train)
summary(m.3)
r.train.3 <- rmse(m.3$fit, pr.train$rate)

test.ct <- sweep(pr.test[,c(-60, -59)], 2, tr.pc$center)
test.pc <- as.matrix(test.ct)%*%tr.pc$rot[,1:2]
r.test.1 <- rmse(cbind(1,test.pc)%*%m.1$coef, pr.test$rate)

test.ct.2 <- sweep(pr.test[,c(-60, -59)], 2, tr.pc$center)
test.pc.2 <- as.matrix(test.ct.2)%*%tr.pc$rot[,1:4]
r.test.2 <- rmse(cbind(1,test.pc.2)%*%m.2$coef, pr.test$rate)

test.ct.3 <- sweep(pr.test[,c(-60, -59)], 2, tr.pc$center)
test.pc.3 <- as.matrix(test.ct.3)%*%tr.pc$rot[,1:20]
r.test.3 <- rmse (cbind(1,test.pc.3)%*%m.3$coef, pr.test$rate)

results[5,1] <- r.train.2
results[5,2] <- r.test.2

rm(test.ct, test.ct.2, test.ct.3, test.pc, test.pc.2, test.pc.3, m.1, m.2, m.3, r.test.1, r.test.2, r.test.3, r.train.1, r.train.2, r.train.3, tr.pc)



#######################################
# Select best AR model                #
#######################################

# Manual selection worked pretty well, but how about fitting different individual variables to train/test?

# Reformat in order
pr.possible.formatted <- cbind(pr.possible[,1:58], pr.possible[,61:62], pr.possible[,59:60])

# Single variable
perms.single <- seq(from=1, to=60)
# Double variable
perms.double <- combn(perms.single, 2, simplify=FALSE)
# Triple variable
perms.triple <- combn(perms.single, 3, simplify=FALSE)
perms <- c(perms.single, perms.double, perms.triple); rm(perms.single, perms.double, perms.triple)

formulas <- data.frame()
for(j in 1:length(perms)) {
  form <- "rate ~"
  for(k in 1:length(perms[j][[1]])) {
    c <- perms[j][[1]][k]
    form <- paste(form, paste("+", colnames(pr.possible)[c], sep=" "), sep=" ")
  }
  formulas[j,1] <- form
  rm(form)
  if(round(j/1000) == j/1000) { print(j) } # To keep track
} rm(perms, c, j, k)


# Risk is that with so many combinations, a particular combination may perfectly fit the data
# To mitigate this risk, we will use 10 bootstrapped samples for each model
rm(smpls)
for(i in 1:10) {
  set.seed(i) # Make results reproducible
  smpl <- sample(1:nrow(pr.possible.formatted), nrow(pr.possible.formatted)*.80)
  if(!exists("smpls")) {
    smpls <- smpl
  } else {
    smpls <- cbind(smpls, smpl)
  }
}

# Create function to perform regressions
lm.perm <- function(formula) {
  rmse.train <- rep(0,10)
  rmse.test <- rep(0,10)
  for(i in 1:10) {
    pr.train.perm <- pr.possible.formatted[smpls[,i],]
    pr.test.perm <- pr.possible.formatted[-smpls[,i],]
    m <- lm(formula, pr.train.perm)
    rmse.train[i] <- rmse(m$fit, pr.train.perm$rate)  
    test.hat <- predict(m, pr.test.perm)
    rmse.test[i] <- rmse(test.hat, pr.test.perm$rate)
  }
  return(cbind(formula,mean(rmse.train),mean(rmse.test)))
}

# Compute all formulas
n <- ceiling(nrow(formulas)/1000)
for(i in 1:n) {
  l <- (i-1)*1000 + 1
  f.min <- formulas[l:(i*1000),]
  res <- sapply(f.min, lm.perm); rm(f.min, l)
  if(!exists("results.selective")) {
    results.selective <- res
  } else {
    results.selective <- cbind(results.selective, res)
  }
  print(i); rm(res)
}

# Manipulate
results.selective <- t(results.selective)
rownames(results.selective) <- NULL
colnames(results.selective) <- c("Formula", "Train RMSE", "Test RMSE")

# View best
results.selective <- results.selective[order(results.selective[,3]),]
# Select best
results[6,1] <- results.selective[which.min(results.selective[,3]),2]
results[6,2] <- results.selective[which.min(results.selective[,3]),3]
rownames(results)[6] <- paste("Selected AR: ", results.selective[which.min(results.selective[,3]),1])

#######################################
# Select best overall model           #
#######################################

best <- which.min(results[,2])
rownames(results)[best]