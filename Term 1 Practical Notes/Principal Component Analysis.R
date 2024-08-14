#Fuel consumption data
fuelcons <- read.table("https://www.maths.dur.ac.uk/users/hailiang.du/data/FUEL.dat", header=TRUE)
fuel <- fuelcons[,c("TAX", "DLIC", "INC", "ROAD")]
fuel.pr <- prcomp(fuel)
#Scree plot
plot(fuel.pr)

fuel.pr
fuel.pr$sdev
#Standard deviations contain ordered sqrt(eigvals)
fuel.pr$rot
#Eigvectors

fuel.pr$sdev^2#Eigenvectors
eigen(var(fuel))$values

#Proportions of variance explained
fuel.pr$sdev^2/sum(fuel.pr$sdev^2)
summary(fuel.pr)

#With scaling
fuel.pr1 <- prcomp(fuel, scale=TRUE)
fuel.pr1
plot(fuel.pr1)
sum(fuel.pr1$sdev^2) #=4 as expected

#Data compression
#First with a single observation
x1 <- fuel[1,]
x1 <- as.numeric(x1)

m <- colMeans(fuel)
t1 <- t(fuel.pr$rot[,c(1,2)])%*%(x1-m) #Score for case 1
r1 <- m+fuel.pr$rot[,c(1,2)]%*%t1 #Reconstruction for case 1

plot(rbind(fuel,t(r1)), col=c(2,rep(1,47),3))#red=original, green=approx

#This time full data matrix
t(fuel.pr$rot[,])%*%(t(fuel)-m) #All scores
#same as
T <- t(fuel.pr$x[,c(1,2)])
R <- t(m+fuel.pr$rot[,c(1,2)]%*%T) #All reconstructed values

plot(rbind(fuel,R), col=c(rep(1,48),rep(3,48)))

boxplot(fuel)
#PCA favours ROAD and DLIC which have largest variance

#Scaling this time
s <- apply(fuel,2,sd) #Column sd
fuel.s <- sweep(fuel,2,s,"/") #Divides all columnds by sd

boxplot(fuel.s)

fuel.pr1 <- prcomp(fuel.s) #Equal to prcomp(fuel, scale=TRUE)
plot(fuel.pr1) 
summary(fuel.pr1) #Suggests d=3

ms <- colMeans(fuel.s)
Ts <- t(fuel.pr1$x[,c(1,2,3)])
Rs <- t(ms+fuel.pr1$rot[,c(1,2,3)]%*%Ts)

plot(rbind(fuel.s, Rs), col=c(rep(1,48),rep(3,48)))

#Principal Component Regression
library(pls)
library(ISLR) #MLB Hitters data Salary response variable
set.seed(1)
pcr.fit <- pcr(Salary~.,data=Hitters, scale=TRUE, validation="CV")
summary(pcr.fit)

validationplot(pcr.fit, val.type='MSEP')

#Finding which PCR model minimises CV error
min.pcr <- which.min(MSEP(pcr.fit)$val[1,1,])-1
#Model with 7 PC minimises CV

coef(pcr.fit, ncomp=min.pcr)

head(predict(pcr.fit, ncomp=min.pcr))

#Principal Component Analysis
library(faraway)
head(seatpos)
#Column s.d.
s <- apply(x, 2, sd)
#Divide all columns by s.d.
x.s <- sweep(x, 2, s, "/")
seatpos.pr <- prcomp(x.s)
#First principal component
seatpos.pr$rotation[,1]
#Scree plot
par(mfrow=c(1,1))
plot(seatpos.pr)

summary(seatpos.pr)
#Hence 4 components needed to capture 95% of total variance

#Compress data with 4 PCs
T   <- t(seatpos.pr$x[,c(1,2,3,4)])
ms <- colMeans(x.s) 
R   <- t(ms + seatpos.pr$rot[,c(1,2,3,4)]%*% T) #reconstruction 
plot(rbind(x.s[,1:2], R[,1:2]), col=c(rep(1,38),rep(3,38)))

#Principal Component Regression
library(pls)
pcr.fit <- pcr(hipcenter~., data=seatpos, scale=TRUE,
               validation='CV')
summary(pcr.fit)
#Suggests use of 2 PCs
#Plot mean squared validation error
validationplot(pcr.fit, val.type='MSEP')

coef(pcr.fit, ncomp=2)

#Predictive Performance of Methods best subset, lasso, ridge, and pcr
# Load R libraries.
library(leaps)

# Predict function for regsubsets
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
  # Step (i) data splitting
  training.obs = sample(1:38,  28)
  y.train = seatpos$hipcenter[training.obs]
  x.train = model.matrix(hipcenter~., seatpos[training.obs, ])[,-1]
  y.test = seatpos$hipcenter[-training.obs]
  x.test = model.matrix(hipcenter~., seatpos[-training.obs, ])[,-1]
  
  # Step (ii) training phase
  bss.train = regsubsets(hipcenter~., data=seatpos[training.obs,], nvmax=8)
  min.cp = which.min(summary(bss.train)$cp)
  ridge.train = cv.glmnet(x.train, y.train, alpha = 0, nfolds = 5)
  lasso.train = cv.glmnet(x.train, y.train, nfold = 5)
  pcr.train = pcr(hipcenter~., data =seatpos[training.obs,], 
                  scale = TRUE, validation="CV")
  min.pcr = which.min(MSEP(pcr.train)$val[1,1, ] ) - 1
  
  # Step (iii) generating predictions
  predict.bss = predict.regsubsets(bss.train, seatpos[-training.obs, ], min.cp)
  predict.ridge = predict(ridge.train, x.test, s = 'lambda.min')
  predict.lasso = predict(lasso.train, x.test, s = 'lambda.min')
  predict.pcr = predict(pcr.train,seatpos[-training.obs, ], ncomp = min.pcr )
  
  # Step (iv) evaluating predictive performance
  cor.bss[i] = cor(y.test, predict.bss)
  cor.ridge[i] = cor(y.test, predict.ridge)
  cor.lasso[i] = cor(y.test, predict.lasso)
  cor.pcr[i] = cor(y.test, predict.pcr)
}

# Plot the resulting correlations as boxplots.
boxplot(cor.bss, cor.ridge, cor.lasso, cor.pcr, 
        names = c('BSS','Ridge', 'Lasso', 'PCR'), 
        ylab = 'Test correlation', col = 2:5)