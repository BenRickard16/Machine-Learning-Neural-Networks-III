library(faraway)
?seatpos

#Data exploring how car seat position is changed for 
#different size drivers
#EDA
dimnames(seatpos)
#Hipcenter will be used as response variable as only
#variable in dataset that considers the location of the
#person within the car
sum(is.na(seatpos))
pairs(seatpos)

cor(seatpos[,9],seatpos[-9])
cor(seatpos[,-9])
#All predictors highly correlated except age


#Ridge regression
y <- seatpos$hipcenter
x=model.matrix(hipcenter~., seatpos)[,-1]

#nstall.packages('glmnet')
library(glmnet)
?glmnet

ridge = glmnet(x,y,alpha=0,nlambda=200)
plot(ridge, xvar='lambda')

#Lasso regression
lasso=glmnet(x,y)
par(mfrow=c(1,2))
plot(lasso,xvar='lambda')
plot(lasso)

lasso$lambda
#Can see which variable included under each lambda
lasso$beta

#Large lamba only includes Ht in model
#We next add leg then age
#Age coming third despite reasonably small correlation
#with response is due to other predictors showing
#collinearity, whereas age contains alternative
#predictive information
#HtShoes enter for small lambda where it's coeff
#increased whilst Ht coeffs decrease to zero
#This is because of the high correlation between them
#even for very small lambda, we don't need both

#Principal component analysis
s <- apply(x, 2, sd) #Calc col sd
x.s <- sweep(x,2,s,'/') #/ all columns by sd
seatpos.pr <- prcomp(x.s)

seatpos.pr
seatpos.pr$rotation[,1]

par(mfrow=c(1,1))
plot(seatpos.pr)

summary(seatpos.pr)
#4 components are needed to capture at least 95%
#of the total variance

T <- t(seatpos.pr$x[,c(1,2,3,4)])
ms <- colMeans(x.s)
R <- t(ms + seatpos.pr$rot[,c(1,2,3,4)]%*%T)
plot(rbind(x.s[,1:2], R[,1:2]), col=c(rep(1,38),rep(3,38)))

#Principal Component Regression
#install.packages('pls')
library('pls')

#Standardising variables and using cross-validation
pcr.fit=pcr(hipcenter~., data=seatpos,scale=TRUE,validation='CV')
summary(pcr.fit)
#Suggests use of 2 principal components

#Plot of mean squared validation error
validationplot(pcr.fit, val.type='MSEP')

#Coefficient estimates in terms of original beta hats
coef(pcr.fit, ncomp=2)
#Age had pos correlation with hipcenter so coef is pos,
#for remaining variables we have neg

#Predictive performance of methods
library(leaps)

#Predict function for regsubsets
predict.regsubsets = function(object, newdata, id, ...){
  form = as.formula(object$call[[2]])
  mat = model.matrix(form, newdata)
  coefi = coef(object, id = id)
  xvars = names(coefi)
  mat[, xvars]%*%coefi
}

repetitions = 50
cor.bss = c()
cor.ridge = c()
cor.lasso = c()
cor.pcr = c()

set.seed(1)

for(i in 1:repetitions){
  #Step 1 - data splitting
  training.obs = sample(1:38, 28)
  y.train = seatpos$hipcenter[training.obs]
  x.train = model.matrix(hipcenter~., seatpos[training.obs,])[,-1]
  y.test = seatpos$hipcenter[-training.obs]
  x.test = model.matrix(hipcenter~., seatpos[-training.obs,])[,-1]
  
  #Step 2 - training phase
  bss.train = regsubsets(hipcenter~., data=seatpos[training.obs,], nvmax=8)
  min.cp = which.min(summary(bss.train)$cp)
  ridge.train = cv.glmnet(x.train, y.train, alpha=0, nfolds=5)
  lasso.train = cv.glmnet(x.train, y.train, nfold=5)
  pcr.train = pcr(hipcenter~., data=seatpos[training.obs,], scale=TRUE, validation='CV')
  min.pcr = which.min(MSEP(pcr.train)$val[1,1,]) - 1
  
  #Step 3 - generating predictions
  predict.bss = predict.regsubsets(bss.train, seatpos[-training.obs, ], min.cp)
  predict.ridge = predict(ridge.train, x.test, s = 'lambda.min')
  predict.lasso = predict(lasso.train, x.test, s = 'lambda.min')
  predict.pcr = predict(pcr.train,seatpos[-training.obs, ], ncomp = min.pcr )
  
  # Step 4 - evaluating predictive performance
  cor.bss[i] = cor(y.test, predict.bss)
  cor.ridge[i] = cor(y.test, predict.ridge)
  cor.lasso[i] = cor(y.test, predict.lasso)
  cor.pcr[i] = cor(y.test, predict.pcr)  
}

boxplot(cor.bss, cor.ridge, cor.lasso, cor.pcr, 
        names = c('BSS','Ridge', 'Lasso', 'PCR'), 
        ylab = 'Test correlation', col = 2:5)
