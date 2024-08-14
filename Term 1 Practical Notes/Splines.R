#Boston housing data again
library(MASS)
y = Boston$medv
x = Boston$lstat
y.lab = 'Median Property Value'
x.lab = 'Lower Status (%)'
plot(x, y, cex.lab = 1.1, col="darkgrey", xlab = x.lab, ylab = y.lab,
     main = "", bty = 'l')

library(splines)

#Will place knots at 25th, 50th and 75th percentiles of x
summary(x)
cuts <- summary(x)[c(2,3,5)]
cuts
sort.x <- sort(x)

#Linear spline
spline1 = lm(y ~ bs(x, degree = 1, knots = cuts))
pred1 = predict(spline1, newdata = list(x = sort.x), se = TRUE)
se.bands1 = cbind(pred1$fit + 2 * pred1$se.fit, 
                  pred1$fit - 2 * pred1$se.fit)
plot(x, y, cex.lab = 1.1, col="darkgrey", xlab = x.lab, ylab = y.lab, 
     main = "Linear Spline", bty = 'l')
lines(sort.x, pred1$fit, lwd = 2, col = "red")
matlines(sort.x, se.bands1, lwd = 2, col = "red", lty = 3)

#Specifying using df instead of cuts (df = d+1+K)
spline1df = lm(y ~ bs(x, degree = 1, df = 5))
pred1df = predict(spline1df, newdata = list(x = sort.x), se = TRUE)
se.bands1df = cbind( pred1df$fit + 2 * pred1df$se.fit, 
                     pred1df$fit - 2 * pred1df$se.fit )

par(mfrow = c(1, 2))
plot(x, y, cex.lab = 1.1, col="darkgrey", xlab = x.lab, ylab = y.lab, 
     main = "Linear Spline (with knots)", bty = 'l')
lines(sort.x, pred1$fit, lwd = 2, col = "red")
matlines(sort.x, se.bands1, lwd = 2, col = "red", lty = 3)

plot(x, y, cex.lab = 1.1, col="darkgrey", xlab = x.lab, ylab = y.lab, 
     main = "Linear Spline (with df)", bty = 'l')
lines(sort.x, pred1df$fit, lwd = 2, col = "darkred")
matlines(sort.x, se.bands1df, lwd = 2, col = "red", lty = 3)

#Splines degree 2 and 3
spline2 = lm(y ~ bs(x, degree = 2, df = 6))
pred2 = predict(spline2, newdata = list(x = sort.x), se = TRUE)
se.bands2 = cbind(pred2$fit + 2 * pred2$se.fit, pred2$fit - 2 * pred2$se.fit)

spline3 = lm(y ~ bs(x, degree = 3, df = 7))
pred3 = predict(spline3, newdata = list(x = sort.x), se = TRUE)
se.bands3 = cbind(pred3$fit + 2 * pred3$se.fit, pred3$fit - 2 * pred3$se.fit)

par(mfrow = c(1,3))
plot(x, y, cex.lab = 1.1, col="darkgrey", xlab = x.lab, ylab = y.lab, 
     main = "Linear Spline", bty = 'l')
lines(sort.x, pred1$fit, lwd = 2, col = "darkred")
matlines(sort.x, se.bands1, lwd = 2, col = "darkred", lty = 3)

plot(x, y, cex.lab = 1.1, col="darkgrey", xlab = x.lab, ylab = y.lab, 
     main = "Quadratic Spline", bty = 'l')
lines(sort.x, pred2$fit, lwd = 2, col = "darkgreen")
matlines(sort.x, se.bands2, lwd = 2, col = "darkgreen", lty = 3)

plot(x, y, cex.lab = 1.1, col="darkgrey", xlab = x.lab, ylab = y.lab, 
     main = "Cubic Spline", bty = 'l')
lines(sort.x, pred3$fit, lwd = 2, col = "darkblue")
matlines(sort.x, se.bands3, lwd = 2, col = "darkblue", lty = 3)

#Natural splines
spline.ns1 = lm(y ~ ns(x, df = 1))
pred.ns1 = predict(spline.ns1, newdata = list(x = sort.x), se = TRUE)
se.bands.ns1 = cbind(pred.ns1$fit + 2 * pred.ns1$se.fit, 
                     pred.ns1$fit - 2 * pred.ns1$se.fit)

spline.ns2 = lm(y ~ ns(x, df = 2))
pred.ns2 = predict(spline.ns2, newdata = list(x = sort.x), se = TRUE)
se.bands.ns2 = cbind(pred.ns2$fit + 2 * pred.ns2$se.fit, 
                     pred.ns2$fit - 2 * pred.ns2$se.fit)

spline.ns3 = lm(y ~ ns(x, df = 3))
pred.ns3 = predict(spline.ns3, newdata = list(x = sort.x), se = TRUE)
se.bands.ns3 = cbind(pred.ns3$fit + 2 * pred.ns3$se.fit, 
                     pred.ns3$fit - 2 * pred.ns3$se.fit)

spline.ns4 = lm(y ~ ns(x, df = 4))
pred.ns4 = predict(spline.ns4, newdata = list(x = sort.x), se = TRUE)
se.bands.ns4 = cbind(pred.ns4$fit + 2 * pred.ns4$se.fit, 
                     pred.ns4$fit - 2 * pred.ns4$se.fit)

par(mfrow = c(2, 2))

plot(x, y, cex.lab = 1.1, col="darkgrey", xlab = x.lab, ylab = y.lab, 
     main = "Natural Spline (1 df)", bty = 'l')
lines(sort.x, pred.ns1$fit, lwd = 2, col = "darkred")
matlines(sort.x, se.bands.ns1, lwd = 2, col = "darkred", lty = 3)

plot(x, y, cex.lab = 1.1, col="darkgrey", xlab = x.lab, ylab = y.lab, 
     main = "Natural Spline (2 df)", bty = 'l')
lines(sort.x, pred.ns2$fit, lwd = 2, col = "darkgreen")
matlines(sort.x, se.bands.ns2, lwd = 2, col = "darkgreen", lty = 3)

plot(x, y, cex.lab = 1.1, col="darkgrey", xlab = x.lab, ylab = y.lab, 
     main = "Natural Spline (3 df)", bty = 'l')
lines(sort.x, pred.ns3$fit, lwd = 2, col = "darkblue")
matlines(sort.x, se.bands.ns3, lwd = 2, col = "darkblue", lty = 3)

plot(x, y, cex.lab = 1.1, col="darkgrey", xlab = x.lab, ylab = y.lab, 
     main = "Natural Spline (4 df)", bty = 'l')
lines(sort.x, pred.ns4$fit, lwd = 2, col = "brown")
matlines(sort.x, se.bands.ns4, lwd = 2, col = "brown", lty = 3)

#Cubic vs natural cubic
par(mfrow = c(1,2))
plot(x, y, cex.lab = 1.1, col="darkgrey", xlab = x.lab, ylab = y.lab, 
     main = "Cubic Spline", bty = 'l')
lines(sort.x, pred3$fit, lwd = 2, col = "blue")
matlines(sort.x, se.bands3, lwd = 2, col = "blue", lty = 3)

plot(x, y, cex.lab = 1.1, col="darkgrey", xlab = x.lab, ylab = y.lab, 
     main = "Natural Spline (3 df)", bty = 'l')
lines(sort.x, pred.ns3$fit, lwd = 2, col = "darkblue")
matlines(sort.x, se.bands.ns3, lwd = 2, col = "darkblue", lty = 3)

#Smoothing splines
smooth1 = smooth.spline(x, y, df = 3)
smooth2 = smooth.spline(x, y, cv = TRUE)

par(mfrow = c(1,2))
plot(x, y, cex.lab = 1.1, col="darkgrey", xlab = x.lab, ylab = y.lab, 
     main = "Smoothing Spline (3 df)", bty = 'l')
lines(smooth1, lwd = 2, col = "brown")

plot(x, y, cex.lab = 1.1, col="darkgrey", xlab = x.lab, ylab = y.lab, 
     main = "Smoothing Spline (CV)", bty = 'l')
lines(smooth2, lwd = 2, col = "darkorange")

#Comparing cubic, natural cubic, and smoothing splines on Wage data
library("ISLR")
library("splines") # Required for regression and natural splines.
agelims=range(Wage$age)
age.grid=seq(from=agelims[1],to=agelims[2])

set.seed(1)

# Number of data points - change this to investigate 
# small, medium and large samples.
n <- 200 

# Take the a sample of n points from the data.
ind = sample(1:3000, n)
Wage1 = Wage[ind,]  # Label subset of data as Wage1.

# Cubic Spline
fitbs = lm(wage~bs(age, degree = 3, knots = c(30,40,60)), data = Wage1)
predbs = predict(fitbs, newdata = list(age = age.grid), se = T)

# Natural Spline
fitns = lm(wage~ns(age, knots = c(30,40,60)), data = Wage1)
predns = predict(fitns, newdata = list(age = age.grid), se = T)

# Smoothing Spline
fitss = smooth.spline(Wage1$age, Wage1$wage, cv = TRUE) 

# Generate the Plots.
par(mfrow=c(1,3))

# Cubic Spline
plot(Wage1$age, Wage1$wage, col = "darkgray", pch = 19, 
     main = 'Cubic spline', bty = 'l', 
     xlab = 'age', ylab = 'wage', cex.lab = 1.4)
lines(age.grid, predbs$fit, lwd = 2, col = 'darkgreen')
abline(v = c(30,40,60), lty = 'dashed')

# Natural Spline
plot(Wage1$age, Wage1$wage, col = "darkgray", pch = 19, 
     main = 'Natural cubic spline', bty = 'l', 
     xlab = 'age', ylab = 'wage', cex.lab = 1.4)
lines(age.grid, predns$fit, lwd = 2, col = 'darkviolet')
abline(v = c(30,40,60), lty = 'dashed')

# Smoothing Spline
plot(Wage1$age, Wage1$wage, col = "darkgray", pch = 19, 
     main = 'Smoothing spline', bty = 'l', 
     xlab = 'age', ylab = 'wage', cex.lab = 1.4)
lines(fitss, lwd = 2, col = 'brown')

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
