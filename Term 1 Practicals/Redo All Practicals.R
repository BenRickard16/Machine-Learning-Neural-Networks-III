#Exploratory data analysis of data with variables waiting time
#between eruptions of the geyser, and the duration of eruptions
data(faithful)
w <- faithful$waiting
d <- faithful$eruptions
cor(w,d)

plot(y=w, x=d)

#Fitting a Linear Model
model <- lm(w~d)
#Coefficients
coef(model)
beta0hat <- coef(model)[1]
beta1hat <- coef(model)[2]
#Fitted values
fitted(model)
#Residuals
resid(model)

#Sum of squares of residuals
lsq.Q <- sum(resid(model)^2)

#Regression Summary
summ <- summary(model)
#Residuals
summ$residuals
#Coefficients
summ$coefficients
#Standard error
se <- summ$sigma
#Adjusted R-squared
rsq <- summ$r.squared
#Same as...
cor(w,d)^2

#Estimation and Prediction
newdata1 <- data.frame(d=3)
#95% CI for estimated waiting time W when d=3
predict(model, newdata=newdata1, interval='confidence', level=0.95)
#95% prediction interval for actual waiting time W
predict(model, newdata=newdata1, interval='prediction', level=0.95)

#Residual Analysis
par(mfrow=c(1,2))
plot(y=resid(model), x=fitted(model))
plot(y=resid(model), x=d)
#Should be no pattern as we have assumed residuals independent of
#X (plots look similar as fitted value is a linear transform of X)

hist(resid(model))
qqnorm(resid(model))
#Should be centered around 0 and normally distributed in order to
#state CIs for coefficients and predictions made with the model
par(mfrow=c(1,1))

#Data to predict bodyfat (brozek) using the other variables except
#siri, density, and free
#Exploratory data analysis
library("faraway")
names(fat)
str(fat)
head(fat)
summary(fat)
sum(is.na(fat))

fat1 <- fat[,-c(2,3,8)]

dim(fat1)
#Pairwise scatter plot
pairs(fat1)
#Add correlation coefficients in a correlation panel to check for
#correlation between predictors and body fat percentage
panel.cor <- function(x, y){
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r <- round(cor(x, y), digits=2)
  txt <- paste0(" ", r)
  text(0.5, 0.5, txt, cex = 0.8)
}
# Customize upper panel
upper.panel<-function(x, y){
  points(x,y, pch = 19)
}
# Create the plots
pairs(fat1, 
      lower.panel = panel.cor,
      upper.panel = upper.panel)

#Or use corrplot package
library(corrplot)
corrplot(cor(fat1), method='number', type='upper', diag=FALSE)
corrplot.mixed(cor(fat1), upper='ellipse', lower='number',
               number.cex=.7)

#Linear Regression Model Fitting
reg1 <- lm(brozek ~ adipos, data=fat1)
summary(reg1)
names(reg1)
coef(reg1)-reg1$coefficients

#Confidence and Prediction Intervals
#CI for the coefficient estimates
confint(reg1, level=0.95)
#CI and PI for brozek when adipos=22
newdata=data.frame(adipos=22)
predict(reg1, newdata=newdata, interval='confidence', level=0.95)
predict(reg1, newdata=newdata, interval='prediction', level=0.95)

#Regression Diagnostics
#Plotting brozek against adipos with the line of best fit
plot(fat1$adipos,fat1$brozek, pch=16, col="cornflowerblue")
abline(reg1,lwd=3,col="red")
#Residual plots
par(mfrow=c(1,2))
plot(reg1, which=1, pch=16, col="cornflowerblue")
plot(reg1, which=2, pch=16, col="cornflowerblue")
par(mfrow=c(1,1))


#Multiple Linear Regression
reg2 <- lm(brozek~adipos+age, data=fat1)
reg3 <- lm(brozek~., data=fat1)
reg4 <- lm(brozek~.-age, data=fat1)

summary(reg1)$r.sq
summary(reg2)$r.sq
summary(reg3)$r.sq
summary(reg4)$r.sq
#Use variance inflation factor to asses for multicollinearity
library(car)
vif(reg3)

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

#Principal Component Analysis
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

#Predictive Performance of Methods
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

#Polynomial and Step-Wise Function Regression
y <- seatpos$hipcenter
x <- seatpos$Ht
x.lab <- 'Height (bare foot) cm'
y.lab <- 'Hip center mm'
plot(x,y, cex.lab=1.1, col="darkgrey", pch=16, bty='l',
     xlab=x.lab, ylab=y.lab)

poly1 <- lm(y~poly(x,1))
summary(poly1)
poly2 <- lm(y~poly(x,2))
summary(poly2)

#Plotting polynomials degree 1 and 2 +_ 2 s.d. CIs
sort.x = sort(x) # sorted values of x.

# Predicted values.
pred1 = predict(poly1, newdata = list(x = sort.x), se = TRUE)
pred2 = predict(poly2, newdata = list(x = sort.x), se = TRUE)

# Confidence interval bands.
se.bands1 = cbind( pred1$fit - 2*pred1$se.fit, pred1$fit + 2*pred1$se.fit )
se.bands2 = cbind( pred2$fit - 2*pred2$se.fit, pred2$fit + 2*pred2$se.fit )

# Plot both plots on a single graphics device.
par(mfrow = c(1,2))

# Degree-1 polynomial plot.
plot(x, y, cex.lab = 1.1, col="darkgrey", xlab = x.lab, ylab = y.lab,
     main = "Degree-1 polynomial", bty = 'l')
lines(sort.x, pred1$fit, lwd = 2, col = "red")
matlines(sort.x, se.bands1, lwd = 2, col = "red", lty = 3)

# Degree-2 polynomial plot.
plot(x, y, cex.lab = 1.1, col="darkgrey", xlab = x.lab, ylab = y.lab,
     main = "Degree-2 polynomial", bty = 'l')
lines(sort.x, pred2$fit, lwd = 2, col = "red")
matlines(sort.x, se.bands2, lwd = 2, col = "red", lty = 3)

par(mfrow=c(1,1))
#Step function with 5 cut points
step6 = lm(y ~ cut(x, 6))
pred6 = predict(step6, newdata = list(x = sort(x)), se = TRUE)
se.bands6 = cbind(pred6$fit + 2*pred6$se.fit, pred6$fit-2*pred6$se.fit)

# Plot the results.
plot(x, y, cex.lab = 1.1, col="darkgrey", xlab = x.lab, ylab = y.lab,
     main = "5 cutpoints", bty = 'l')
lines(sort(x), pred6$fit, lwd = 2, col = "red")
matlines(sort(x), se.bands6, lwd = 1.4, col = "red", lty = 3)

# The fitted model can be more accurately fitted over the data as follows:
newx <- seq(from = min(x), to = max(x), length = 100)
pred6 = predict(step6, newdata = list(x = newx), se = TRUE)
se.bands6 = cbind(pred6$fit + 2*pred6$se.fit, pred6$fit-2*pred6$se.fit)
plot(x, y, cex.lab = 1.1, col="darkgrey", xlab = x.lab, ylab = y.lab,
     main = "5 cutpoints", bty = 'l')
lines(newx, pred6$fit, lwd = 2, col = "red")
matlines(newx, se.bands6, lwd = 1.4, col = "red", lty = 3)
#More step-wise due to newx

#Splines
summary(x)
cuts <- summary(x)[c(2,3,5)]
library('splines')
spline1 <- lm(y ~ bs(x, degree=1, knots=cuts))

# Sort the values of x from low to high.
sort.x <- sort(x)

# Obtain predictions for the fitted values along with confidence intervals.
pred1 = predict(spline1, newdata = list(x = sort.x), se = TRUE)
se.bands1 = cbind(pred1$fit + 2 * pred1$se.fit, pred1$fit - 2 * pred1$se.fit)

# Plot the fitted linear spline model over the data. 
plot(x, y, cex.lab = 1.1, col="darkgrey", xlab = x.lab, ylab = y.lab, 
     main = "Linear Spline", bty = 'l')
lines(sort.x, pred1$fit, lwd = 2, col = "red")
matlines(sort.x, se.bands1, lwd = 2, col = "red", lty = 3)

#Smoothing spline
smooth1 = smooth.spline(x, y, df = 3)

plot(x, y, cex.lab = 1.1, col="darkgrey", xlab = x.lab, ylab = y.lab, 
     main = "Smoothing Spline (3df)", bty = 'l')
lines(smooth1, lwd = 2, col = "brown")

#General Additive Models
library(gam)
gam <- gam(hipcenter ~ ns(Age,df=5)+s(Thigh,df=3)+Ht, data=seatpos)

par(mfrow=c(2,3))
plot(gam, se=TRUE, col="blue")
plot( seatpos$Age, seatpos$hipcenter, pch = 16, col = 2, 
      ylab = y.lab, xlab = "Age (years)" )
plot( seatpos$Thigh, seatpos$hipcenter, pch = 16, col = 2, 
      ylab = y.lab, xlab = "Thigh length (cm)" )
plot( seatpos$Ht, seatpos$hipcenter, pch = 16, col = 2, 
      ylab = y.lab, xlab = "Ht (bare foot) (cm)" )
par(mfrow=c(1,1))

#Boston Housing Data
library("MASS")
y <- Boston$medv
x <- Boston$indus
y.lab <- 'Median Property Value'
x.lab <- 'Non-Retail Business Acres Per Town'
cuts <- summary(x)[c(2,3,5)]
sort.x <- sort(x)

#Fit cubic spline
spline.bs <- lm(y ~ bs(x, knots=cuts))
pred.bs <- predict(spline.bs, newdata=list(x=sort.x), se=TRUE)
se.bands.bs <- cbind(pred.bs$fit + 2*pred.bs$se.fit,
                     pred.bs$fit - 2*pred.bs$se.fit)

#Natural cubic spline
spline.ns <- lm(y ~ ns(x, knots=cuts))
pred.ns <- predict(spline.ns, newdata=list(x=sort.x), se=TRUE)
se.bands.ns <- cbind(pred.ns$fit + 2*pred.ns$se.fit,
                     pred.ns$fit - 2*pred.ns$se.fit)

#Smoothing spline 3 effective df
spline.smooth <- smooth.spline(x, y, df=3)

par(mfrow=c(1,3))

# Plot the cubic spline.
plot(x, y, cex.lab = 1.1, col="darkgrey", xlab = x.lab, ylab = y.lab, 
     main = "Cubic Spline", bty = 'l')
lines(sort.x, pred.bs$fit, lwd = 2, col = "red")
matlines(sort.x, se.bands.bs, lwd = 2, col = "red", lty = 3)

# Plot the natural cubic spline.
plot(x, y, cex.lab = 1.1, col="darkgrey", xlab = x.lab, ylab = y.lab, 
     main = "Natural Cubic Spline", bty = 'l')
lines(sort.x, pred.ns$fit, lwd = 2, col = "darkred")
matlines(sort.x, se.bands.ns, lwd = 2, col = "darkred", lty = 3)

# Plot the smoothing spline.
plot(x, y, cex.lab = 1.1, col="darkgrey", xlab = x.lab, ylab = y.lab, 
     main = "Smoothing Spline (3 df)", bty = 'l')
lines(spline.smooth, lwd = 2, col = "brown")

#GAM
Boston1 <- Boston
Boston1$chas <- factor(Boston1$chas)

gam1 <- gam(medv ~ ns(lstat,df=5) + ns(nox,df=7) +
              s(indus,df=7) + poly(age,5) + chas, data=Boston1)

par(mfrow=c(2,3))
plot(gam1, se=TRUE, col="blue")

#Logistic Regression
admit <- read.csv("https://www.maths.dur.ac.uk/users/hailiang.du/data/admit.csv")
head(admit)

admit$rank <- factor(admit$rank)

glm.fit <- glm(admit~., data=admit, family="binomial")
summary(glm.fit)

glm.probs <- predict(glm.fit, type="response")
glm.probs[1:10]

glm.pred <- rep(0,400)
glm.pred[glm.probs>0.5] <- 1

table(glm.pred, admit$admit)
