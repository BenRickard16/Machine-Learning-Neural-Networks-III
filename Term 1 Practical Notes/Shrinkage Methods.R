#MLB data with salary as reponse variable
library(ISLR)
Hitters <- na.omit(Hitters)
dim(Hitters)

library(glmnet)
y <- Hitters$Salary
x <- model.matrix(Salary~., Hitters)[,-1]
head(x)

#Ridge Regression
ridge <- glmnet(x,y,alpha=0) #default alpha=1 for lasso
names(ridge)

ridge$lambda
dim(ridge$beta)#19 rows of predictors, 100 columns of lambda
ridge$beta[,1:3] #Only first 3 columns

plot(ridge, xvar='lambda')

#Lasso Regression
lasso <- glmnet(x,y)
lasso$lambda
dim(lasso$beta)
lasso$beta[,1:3]
coef(lasso)[,1:3]

plot(lasso, xvar='lambda')

#Validation to choose lambda (ridge regression)
set.seed(1)
ridge.cv <- cv.glmnet(x,y,alpha=0)
names(ridge.cv)
ridge.cv$lambda.min #optimal lambda
ridge.cv$lambda.1se #max value within 1 se interval of optimal lambda

#Comparing coefficients
round(cbind(
coef(ridge.cv, s='lambda.min'),
coef(ridge.cv, s='lambda.1se')),
digits=3)

#Vertical lines are lambda.min and lambda.1se
plot(ridge.cv)
abline(h=ridge.cv$cvup[ridge.cv$index[1]],lty=4)
#Horizontal line at upper limit of 1se interval of MSE at min lambda

#Plot ridge regularisation paths with the lambda values
plot(ridge, xvar='lambda')
abline(v=log(ridge.cv$lambda.min), lty=3)
abline(v=log(ridge.cv$lambda.1se), lty=3)

#Post-hoc analysis to set some coeffs to zero
beta.1se <- coef(ridge.cv, s='lambda.1se')[-1]
rank.coef <- sort(abs(beta.1se), decreasing=TRUE)
rank.coef

#Validation to choose lambda (lasso regression)
set.seed(1)
lasso.cv = cv.glmnet(x,y)
lasso.cv$lambda.min
lasso.cv$lambda.1se # Bigger lambda means smaller coeffs

round(cbind(
  coef(lasso.cv, s='lambda.min'),
  coef(lasso.cv, s='lambda.1se')),
  digits=3)
#1se chooses less predictors

par(mfrow=c(1,2))
plot(lasso.cv)
lasso = glmnet(x, y)
plot(lasso, xvar = 'lambda')
abline(v = log(lasso.cv$lambda.min), lty = 3) 
abline(v = log(lasso.cv$lambda.1se), lty = 3)

#Testing predictive performance of both lambdas
repetitions = 50
mse.1 = c()
mse.2 = c()

set.seed(1)                
for(i in 1:repetitions){
  
  # Step (i) data splitting
  training.obs = sample(1:263,  175)
  y.train = Hitters$Salary[training.obs]
  x.train = model.matrix(Salary~., Hitters[training.obs, ])[,-1]
  y.test = Hitters$Salary[-training.obs]
  x.test = model.matrix(Salary~., Hitters[-training.obs, ])[,-1]
  
  # Step (ii) training phase
  lasso.train = cv.glmnet(x.train, y.train)
  
  # Step (iii) generating predictions
  predict.1 = predict(lasso.train, x.test, s = 'lambda.min')
  predict.2 = predict(lasso.train, x.test, s = 'lambda.1se')
  
  # Step (iv) evaluating predictive performance
  mse.1[i] = mean((y.test-predict.1)^2)
  mse.2[i] = mean((y.test-predict.2)^2)
}

par(mfrow=c(1,1))
boxplot(mse.1, mse.2, names = c('min-CV lasso','1-se lasso'), 
        ylab = 'Test MSE', col = 7)


#Data exploring how car seat position is changed for
#different size drivers
seatpos
#Exploratory Data Analysis
dimnames(seatpos)
#Hipcenter will be used as response variable as only variable
#in dataset that considers the location of the person within
#the car
sum(is.na(seatpos))
pairs(seatpos)

cor(seatpos[,9],seatpos[-9])
cor(seatpos[,-9])
#All predictors highly correlated except age

#Ridge Regression
y <- seatpos$hipcenter
x <- model.matrix(hipcenter~., seatpos)[,-1]

library('glmnet')
?glmnet
ridge=glmnet(x,y, alpha=0, nlambda=200)
plot(ridge, xvar='lambda')

#Lasso Regression
lasso = glmnet(x,y)
par(mfrow=c(1,2))
plot(lasso, xvar='lambda')
plot(lasso)

lasso$lambda
lasso$beta