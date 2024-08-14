install.packages("faraway")
library("faraway")

names(fat)
?fat
str(fat)
head(fat)
summary(fat)

sum(is.na(fat))
#no missing values

fat1 <- fat[,-c(2,3,8)]

dim(fat1)
#252 observations 15 variables

pairs(fat1)
#pairwise scatterplots of the predictors

panel.cor <- function(x,y){
  usr <- par("usr"); on.exit(par(usr))
  par(usr= c(0,1,0,1))
  r<- round(cor(x,y), digits=2)
  txt <- paste0(" ", r)
  text(0.5,0.5, txt, cex=0.8)
}

upper.panel <- function(x,y){
  points(x,y, pch=19)
}

pairs(fat1,
      lower.panel=panel.cor,
      upper.panel=upper.panel)
#pairwise scatterplots of predictors with associated
#correlation coefficient

install.packages("corrplot")
library(corrplot)

corrplot(cor(fat1), method="number", type="upper", diag=FALSE)
corrplot.mixed(cor(fat1), upper="ellipse", lower="number", number.cex=.7)
#2 plots depicting correlation coefficients between
#the predictor variables

#Linear regression model fitting
reg1 <- lm(brozek~adipos, data=fat1)
summary(reg1)
names(reg1)
coef(reg1)
reg1$coef

#Confidence and prediction intervals
confint(reg1, level=0.95)
newdata = data.frame(adipos=22)
predict(reg1, newdata=newdata, interval="confidence", level=0.95)
predict(reg1, newdata=newdata, level=0.95, interval="prediction")

#Regression diagnostics
plot(fat1$adipos, fat1$brozek, pch=16, col='cornflower blue')
abline(reg1, lwd=3, col='red')

par(mfrow=c(1,2))
plot(reg1, which=1, pch=16, col='cornflower blue')
plot(reg1, which=2, pch=16, col='cornflower blue')
par(mfrow=c(1,1))

#Multi linear regression model fitting
reg2 <- lm(brozek~adipos+age, data=fat1)
reg3 <- lm(brozek~., data=fat1)
reg4 <- lm(brozek~.-age, data=fat1)

summary(reg1)$r.sq
summary(reg2)$r.sq
summary(reg3)$r.sq
summary(reg4)$r.sq

install.packages("car")
library(car)
vif(reg3)

#Forward stepwise regression
install.packages("leaps")
library(leaps)

fwd<-regsubsets(brozek~., data=fat1, method="forward", nvmax=14)

results <- summary(fwd)

RSS = results$rss
r2 = results$rsq
Cp = results$cp
BIC = results$bic
Adj_r2 = results$adjr2

criteria_values <- cbind(RSS,r2,Cp,BIC,Adj_r2)

#Looking for model with lowest Cp, BIC and highest
#adjusted R^2
which.min(Cp)
which.min(BIC)
which.max(Adj_r2)

#Finding which predictors are in the relevant model
coef(fwd,4)
coef(fwd,8)

#Plotting Cp, BIC, and adjusted R^2 against number
#of predictors
par(mfrow=c(1,3))
plot(Cp, xlab = 'Number of Predictors', ylab='Cp', type='l', lwd=2)
points(8,Cp[8], col='red', cex=2, pch=8, lwd=2)
plot(BIC, xlab = 'Number of Predictors', ylab='BIC', type='l', lwd=2)
points(4,BIC[4], col='red', cex=2, pch=8, lwd=2)
plot(Adj_r2, xlab = 'Number of Predictors', ylab='Adjusted RSq', type='l', lwd=2)
points(8,Adj_r2[8], col='red', cex=2, pch=8, lwd=2)
par(mfrow=c(1,1))

plot(fwd,scale='Cp')

#Best subset and backward selection
best = regsubsets(brozek~., data=fat1, nvmax=14)
bwd = regsubsets(brozek~., data=fat1, method='backward', nvmax=14)

which.min(summary(best)$cp)
which.min(summary(best)$bic)
which.max(summary(best)$adjr2)

which.min(summary(bwd)$cp)
which.min(summary(bwd)$bic)
which.max(summary(bwd)$adjr2)

#So the 3 optimal models (under Cp, BIC and adjusted
#R^2) for forward stepwise, backward stepwise and best
#subset selection all have the same number of predictors

#Cp/adj R^2
coef(fwd,8)
coef(best,8)
coef(bwd,8)

#BIC
coef(fwd,4)
coef(best,4)
coef(bwd,4)

#Hence also use the same predictors