df <- read.csv( file ="https://www.maths.dur.ac.uk/users/georgios.karagiannis/first%20computer%20exam%20dataset/WineQuality_sml.csv")
df$color <- as.factor(df$color)
df$quality <- as.numeric(df$quality)
cont_variables = df[ ,-c(1)] 
pairs(cont_variables)
dim(df)
summary(df)

#Split into test and train
data_train=df[1:2000,]
data_test=df[2001:dim(df)[1],]

#SLR alcohol predictor quality response
fit1 <- lm(quality~alcohol, data=data_train)
summary(fit1)
coef(fit1)
summary(fit1)$adj.r.squared

#95% prediction interval for wine with alcohol=10
newdata1 <- data.frame(alcohol=10)
predict(fit1, newdata=newdata1, interval='prediction', level=0.95)

#MLR with all predictors
fit2 <- lm(quality~., data=data_train)
summary(fit2)

#MSE of model over test set
pred1 <- predict(fit2, newdata=data_test)
mean((data_test$quality-pred1)^2)

#Forward selection
library(leaps)
fwd <- regsubsets(quality~., data=data_train, nvmax=13, method="forward")

which.min(summary(fwd)$bic) #6 predictors gives min. BIC
coef(fwd,6) #Find out which predictors are used
mean((lm(quality~color+volatile.acidity+residual.sugar+density+sulphates+alcohol, data=data_test)$fit-data_test$quality)^2)

#Prediction with regsubsets function from lecture notes
predict.regsubsets = function(object, newdata, id, ...){
  form = as.formula(object$call[[2]])
  mat = model.matrix(form, newdata)
  coefi = coef(object, id = id)
  xvars = names(coefi)
  mat[, xvars]%*%coefi
}

#Calculating MSE over test data
pred2 <- predict.regsubsets(fwd, data_test, 6)
mean((data_test$quality-pred2)^2)

#Backward and Best Subset Selection
bwd <- regsubsets(quality~., data=data_train, nvmax=13, method="backward")
best <- regsubsets(quality~., data=data_train, nvmax=13)

#How many predictors give min. Cp in each case?
which.min(summary(fwd)$cp)
which.min(summary(bwd)$cp)
which.min(summary(best)$cp)

#Principal component analysis all predictors except color with scaling
data_train[c(2:13)] #Dropped color column

pr1 <- prcomp(data_train[c(2:13)], scale=TRUE) #Have scaled variances
summary(pr1)

#Second order polynomial alcohol only predictor
poly2 <- lm(quality ~ poly(alcohol,  2), data=data_train)
summary(poly2)

#GAM with:
#smoothing spline df=3 for density
#natural spline df=5 for alcohol
#step function for color
library(gam)
data_train$color <- as.numeric(data_train$color)#Require numerical values for step function

gam1 <- gam(quality ~ s(density, df=3)+ns(alcohol, df=5)+
             cut(color,2), data=data_train)

#MSE over test data
data_test$color <- as.numeric(data_test$color)
pred3 <- predict(gam, newdata=data_test)
mean((pred3-data_test$quality)^2)

#Regression Tree
library(tree)
tree_fit <- tree(quality~., data=data_train)
summary(tree_fit)

#Prune the tree to size 6
tree_fit_prune <- prune.tree(tree_fit, best=6)
summary(tree_fit_prune)

#MSE of pruned tree over test data
pred4 <- predict(tree_fit_prune, newdata=data_test)
mean((pred4-data_test$quality)^2)
