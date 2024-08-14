#Infant Mortality and GDP
library(carData)
data(UN)
options(scipen=999)
newUN <- na.omit(UN)
str(newUN)

fit <- lm(infantMortality ~ ppgdp, data=newUN)
summary(fit)

plot(newUN$infantMortality ~ newUN$ppgdp, xlab="GDP per Capita",
     ylab="Infant Mortality", pch=16, col="cornflowerblue")
abline(fit, col="red")
#Clear not a linear relationship (R-squared=0.2656)

#Diagnostic plots
plot(fit, which=1, pch=16, col="cornflowerblue")
#Clear pattern between residuals and fitted values suggests 
#assumption of homogeneous variance not appropriate

#Applying log transformations
fit1 <- lm(log(infantMortality) ~ log(ppgdp), data=newUN)
summary(fit1)
#Better R-squared=0.7662

plot(log(newUN$infantMortality) ~ log(newUN$ppgdp),
     xlab="GDP per Capita", ylab="Infant Mortality", pch=16,
     col="cornflowerblue")
abline(fit1, col="red")

#Diagnositc plots
plot(fit1, which=1, pch=16, col="cornflowerblue")
#Much closer to a random scatter

par(mfrow=c(2,2))
#Before transform
plot(fit, which=2, pch=16, col="cornflowerblue")
hist(resid(fit), col="cornflowerblue",main="")
#After transform
plot(fit1, which=2, pch=16, col="hotpink3")
hist(resid(fit1), col="hotpink3",main="")

#Coefficients
coef(fit1)
#CI for coefficients
confint(fit1, level=0.95)

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
