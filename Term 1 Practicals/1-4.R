library("MASS")
library("faraway")

head(seatpos)
y = seatpos$hipcenter
x = seatpos$Ht

y.lab = 'hip center (mm)'
x.lab = 'Height (bare foot) in cm'
plot( x, y, cex.lab = 1.1, col="darkgrey", xlab = x.lab, ylab = y.lab, 
      main = "", bty = 'l', pch = 16 )

poly1 = lm(y ~ poly(x,  1))
summary(poly1)

poly2 = lm(y ~ poly(x,  2))
summary(poly2)
#p-value for 2nd order term high so suggests not required

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

#Step function plot
par(mfrow = c(1,1))
step6 = lm(y ~ cut(x, 6))
pred6 = predict(step6, newdata = list(x = sort(x)), se = TRUE)
se.bands6 = cbind(pred6$fit + 2*pred6$se.fit, pred6$fit-2*pred6$se.fit)

# Plot the results.
plot(x, y, cex.lab = 1.1, col="darkgrey", xlab = x.lab, ylab = y.lab,
     main = "5 cutpoints", bty = 'l')
lines(sort(x), pred6$fit, lwd = 2, col = "red")
matlines(sort(x), se.bands6, lwd = 1.4, col = "red", lty = 3)

summary(step6)

newx <- seq(from = min(x), to = max(x), length = 100)
pred6 = predict(step6, newdata = list(x = newx), se = TRUE)
se.bands6 = cbind(pred6$fit + 2*pred6$se.fit, pred6$fit-2*pred6$se.fit)
plot(x, y, cex.lab = 1.1, col="darkgrey", xlab = x.lab, ylab = y.lab,
     main = "5 cutpoints", bty = 'l')
lines(newx, pred6$fit, lwd = 2, col = "red")
matlines(newx, se.bands6, lwd = 1.4, col = "red", lty = 3)


#Spline
cuts <- summary(x)[c(2,3,5)]
library("splines")
spline1 = lm(y ~ bs(x, degree = 1, knots = cuts))

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


#Fitting GAM for hipcentre
install.packages('gam')
library(gam)
gam = gam( hipcenter ~ ns( Age, df = 5 ) + s( Thigh, df = 3 ) + Ht, 
           data = seatpos )

par( mfrow = c(2,3) )
plot( gam,  se = TRUE, col = "blue" )
# Compare with the following plots.
plot( seatpos$Age, seatpos$hipcenter, pch = 16, col = 2, 
      ylab = y.lab, xlab = "Age (years)" )
plot( seatpos$Thigh, seatpos$hipcenter, pch = 16, col = 2, 
      ylab = y.lab, xlab = "Thigh length (cm)" )
plot( seatpos$Ht, seatpos$hipcenter, pch = 16, col = 2, 
      ylab = y.lab, xlab = "Ht (bare foot) (cm)" )


#Boston data
library("MASS")
library(splines)
y = Boston$medv
x = Boston$indus
y.lab = 'Median Property Value'
x.lab = 'Non-retail business acres per town'
cuts <- summary(x)[c(2,3,5)]
sort.x = sort(x)

# Fit a cubic spline
spline.bs = lm(y ~ bs(x, knots = cuts))
pred.bs = predict(spline.bs, newdata = list(x = sort.x), se = TRUE)
se.bands.bs = cbind(pred.bs$fit + 2 * pred.bs$se.fit, 
                    pred.bs$fit - 2 * pred.bs$se.fit)

# Fit a natural cubic spline.
spline.ns = lm(y ~ ns(x, knots = cuts))
pred.ns = predict(spline.ns, newdata = list(x = sort.x), se = TRUE)
se.bands.ns = cbind(pred.ns$fit + 2 * pred.ns$se.fit, 
                    pred.ns$fit - 2 * pred.ns$se.fit)

# Fit a smoothing spline, with 3 effective degrees of freedom.
spline.smooth = smooth.spline(x, y, df = 3)

# Split the plotting device into 3.
par(mfrow = c(1,3))

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


# Let's set chas to be a factor (you may want to make rad a factor as well, 
# if used).
Boston1 = Boston
Boston1$chas = factor(Boston1$chas)

# Let's fit a GAM - can you see how each predictor is contributing to 
# modelling the response?
gam1 = gam( medv ~ ns( lstat, df = 5 ) + ns( nox, df = 7 ) + 
              s( indus, df = 7 ) + poly( age, 5 ) + chas, data = Boston1 )
par(mfrow = c(2,3))
plot(gam1,  se = TRUE, col = "blue")


#Binary regression
admit <- read.csv("https://www.maths.dur.ac.uk/users/hailiang.du/data/admit.csv")
head(admit)

pairs(admit[,2:4], col=admit[,1]+1)
admit$rank <- factor(admit$rank)

glm.fit = glm(admit ~ ., data=admit, family="binomial")
summary(glm.fit)

glm.probs <- predict(glm.fit, type = "response")
glm.probs[1:10]
glm.pred=rep(0, 400)
glm.pred[glm.probs > .5] = 1

table(glm.pred, admit$admit)
(254 + 30) / 400
mean(glm.pred == admit$admit)
