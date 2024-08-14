library(ISLR)
#Data on MLB players with response variable salary
names(Hitters)
head(Hitters)
dim(Hitters)

#Check for missing values
sum(is.na(Hitters))
sum(is.na(Hitters$Salary))
#Omit missing entries
Hitters <- na.omit(Hitters)

#Best Subset Selection
library(leaps)
best <- regsubsets(Salary~., Hitters)
summary(best)
#Only has up to model M_8
#To use all 19 predictors...
best <- regsubsets(Salary~., data=Hitters, nvmax=19)
results <- summary(best)
names(results)

RSS <- results$rss
r2 <- results$rsq
Cp <- results$cp
BIC <- results$bic
Adj_r2 <- results$adjr2
cbind(RSS,r2,Cp,BIC,Adj_r2)

#Plot how RSS and R-squared change with number of predictors
par(mfrow=c(1,2))
plot(RSS, type='l', lwd=2)
plot(r2, type='l', lwd=2)

#Finding how many predictors in optimal models under different
#selection criteria
which.min(Cp)
which.min(BIC)
which.max(Adj_r2)
#And plotting...
par(mfrow = c(1, 3))
plot(Cp, xlab = "Number of Predictors", ylab = "Cp", 
     type = 'l', lwd = 2)
points(10, Cp[10], col = "red", cex = 2, pch = 8, lwd = 2)
plot(BIC, xlab = "Number of Predictors", ylab = "BIC", 
     type = 'l', lwd = 2)
points(6, BIC[6], col = "red", cex = 2, pch = 8, lwd = 2)
plot(Adj_r2, xlab = "Number of Predictors", ylab = "Adjusted RSq", 
     type = "l", lwd = 2)
points(11, Adj_r2[11],  col = "red", cex = 2, pch = 8, lwd = 2)
par(mfrow=c(1,1))

#Visualisation of BIC for different combinations of predictors
#Top row = best model, bottom row = worst model 
plot(best, scale="bic")

#Extract model coefficients
coef(best,10) #Cp
coef(best,8) #BIC
coef(best,11) #Adj_r2


#Forward Stepwise Selection
fwd <- regsubsets(Salary~., data=Hitters, nvmax=19, method="forward")
summary(fwd)

coef(best,6)
coef(fwd,6)
#Agreement
coef(best,7)
coef(fwd,7)
#Disagreement

#Validation for Best Subset Selection
predict.regsubsets = function(object, newdata, id, ...){
  form = as.formula(object$call[[2]])
  mat = model.matrix(form, newdata)
  coefi = coef(object, id = id)
  xvars = names(coefi)
  mat[, xvars]%*%coefi
}

#Splitting up in to training and testing data
set.seed(10)
dim(Hitters)
training.obs <- sample(1:263, 175)
Hitters.train <- Hitters[training.obs,]
Hitters.test <- Hitters[-training.obs,]
dim(Hitters.train)

#Apply regsubsets to training set
best.val <- regsubsets(Salary~., data=Hitters.train, nvmax=19)
#Create empty vector where we will store 19 validation MSEs
val.error<-c()
for (i in 1:19){
  pred <- predict.regsubsets(best.val, Hitters.test, i)
  val.error[i] <- mean((Hitters.test$Salary-pred)^2)
}

val.error
which.min(val.error)

#Now to make inferences, train full data set with the 10 chosen
#predictors
coef(best.val, 10)
ls10 <- lm(Salary ~ AtBat+Hits+Runs+Walks+CAtBat+CRuns+CRBI+
             CWalks+Division+PutOuts, data = Hitters)
summary(ls10)
confint(ls10)

#Results depend on the random splitting into train and test
#Will check how different the results are with 50 different
#random splits
min.valid = c()

for(n in 1:50){
  set.seed(n)          
  training.obs = sample(1:263, 175)
  Hitters.train = Hitters[training.obs,  ]
  Hitters.test = Hitters[-training.obs,  ]
  
  best.val = regsubsets(Salary~., data = Hitters.train, nvmax = 19)
  
  val.error<-c()
  for(i in 1:19){
    pred = predict.regsubsets(best.val, Hitters.test, i)
    val.error[i] = mean((Hitters.test$Salary - pred)^2)
  }
  val.error
  min.valid[n] = which.min(val.error)
}

hist(min.valid, col = 3, breaks = seq( from = 0.5, to = 19.5, length = 20 ), 
     xlab = 'Number of Predictors', 
     main = 'Best Subset Selection with validation')
abline(v = mean(min.valid), col = 2, lwd = 4)
legend('topright', legend=c('Average selection'),bty = 'n', lty = 1, 
       lwd = 4, col = 2)

#See lecture notes for cross-validation techniques

#Body Fat Data
#Forward Stepwise Regression
library('leaps')
fwd=regsubsets(brozek~., data=fat1, method='forward', nvmax=14)
results = summary(fwd)
results

RSS = results$rss
r2 = results$rsq
Cp = results$cp
BIC = results$bic
Adj_r2 = results$adjr2

criteria_values <- cbind(RSS, r2, Cp, BIC, Adj_r2)

which.min(Cp)
which.min(BIC)
which.max(Adj_r2)
coef(fwd,4)
coef(fwd,8)

par(mfrow = c(1, 3))
plot(Cp, xlab = "Number of Predictors", ylab = "Cp", type = 'l', lwd = 2)
points(8, Cp[8], col = "red", cex = 2, pch = 8, lwd = 2)
plot(BIC, xlab = "Number of Predictors", ylab = "BIC", type = 'l', lwd = 2)
points(4, BIC[4], col = "red", cex = 2, pch = 8, lwd = 2)
plot(Adj_r2, xlab = "Number of Predictors", ylab = "Adjusted RSq", 
     type = "l", lwd = 2)
points(8, Adj_r2[8],  col = "red", cex = 2, pch = 8, lwd = 2)

par(mfrow=c(1,1))
plot(fwd, scale="Cp")

#Best Subset and Backward Selection
best = regsubsets(brozek~., data=fat1, nvmax=14)
bwd = regsubsets(brozek~., data=fat1, method='backward', nvmax=14)

which.min(summary(best)$cp)
which.min(summary(best)$bic)
which.max(summary(best)$adjr2)

which.min(summary(bwd)$cp)
which.min(summary(bwd)$bic)
which.max(summary(bwd)$adjr2)

coef(fwd,8)
coef(best,8)
coef(bwd,8)

coef(fwd,4)
coef(best,4)
coef(bwd,8)

