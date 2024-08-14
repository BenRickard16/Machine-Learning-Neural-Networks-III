library(MASS)
head(Boston)
#Data on towns in Boston with response variable median value of
#owner-occupied homes, medv
y <- Boston$medv
x <- Boston$lstat
y.lab <- 'Median Property Value'
x.lab <- 'Lower Status (%)'

plot(x,y,cex.lab=1.1, xlab=x.lab, ylab=y.lab, main="", bty='l')

#Degree-2 polynomial
poly2 <- lm(y~x+I(x^2))
summary(poly2)

poly2 <- lm(y~poly(x,2,raw=TRUE))
summary(poly2)

#Plot best fit line with se bands
sort.x=sort(x)
pred2 <- predict(poly2, newdata=list(x=sort.x), se=TRUE)
names(pred2)
se.bands2 <- cbind(pred2$fit-2*pred2$se.fit,
                   pred2$fit+2*pred2$se.fit)

plot(x, y, cex.lab = 1.1, col="darkgrey", xlab = x.lab, ylab = y.lab,
     main = "Degree-2 polynomial", bty = 'l')
lines(sort.x, pred2$fit, lwd = 2, col = "red")
matlines(sort.x, se.bands2, lwd = 1.4, col = "red", lty = 3)

#Same up to degree-5
poly3 = lm(y ~ poly(x,  3))
poly4 = lm(y ~ poly(x,  4))
poly5 = lm(y ~ poly(x, 5))

pred3 = predict(poly3, newdata = list(x = sort.x), se = TRUE)
pred4 = predict(poly4, newdata = list(x = sort.x), se = TRUE)
pred5 = predict(poly5, newdata = list(x = sort.x), se = TRUE)

se.bands3 = cbind(pred3$fit + 2*pred3$se.fit, pred3$fit-2*pred3$se.fit)
se.bands4 = cbind(pred4$fit + 2*pred4$se.fit, pred4$fit-2*pred4$se.fit)
se.bands5 = cbind(pred5$fit + 2*pred5$se.fit, pred5$fit-2*pred5$se.fit)


par(mfrow = c(2,2))
# Degree-2
plot(x, y, cex.lab = 1.1, col="darkgrey", xlab = x.lab, ylab = y.lab,
     main = "Degree-2 polynomial", bty = 'l')
lines(sort.x, pred2$fit, lwd = 2, col = "red")
matlines(sort.x, se.bands2, lwd = 2, col = "red", lty = 3)

# Degree-3
plot(x, y, cex.lab = 1.1, col="darkgrey", xlab = x.lab, ylab = y.lab,
     main = "Degree-3 polynomial", bty = 'l')
lines(sort.x, pred3$fit, lwd = 2, col = "darkviolet")
matlines(sort.x, se.bands3, lwd = 2, col = "darkviolet", lty = 3)

# Degree-4
plot(x, y, cex.lab = 1.1, col="darkgrey", xlab = x.lab, ylab = y.lab,
     main = "Degree-4 polynomial", bty = 'l')
lines(sort.x, pred4$fit, lwd = 2, col = "blue")
matlines(sort.x, se.bands4, lwd = 2, col = "blue", lty = 3)

# Degree-5
plot(x, y, cex.lab = 1.1, col="darkgrey", xlab = x.lab, ylab = y.lab,
     main = "Degree-5 polynomial", bty = 'l')
lines(sort.x, pred5$fit, lwd = 2, col = "black")
matlines(sort.x, se.bands5, lwd = 2, col = "black", lty = 3)

#ANOVA test for sequential comparisons by F-test
poly1 <- lm(y ~ x)
poly6 <- lm(y ~ poly(x, 6))
anova(poly1, poly2, poly3, poly4, poly5, poly6)
#Degree-5 selected

#Can use multiple predictors
x1 <- Boston$lstat
x2 <- Boston$rm

polym1 <- lm(y~polym(x1,x2,degree=2)) #Includes interaction term
summary(polym1)
par(mfrow=c(1,1))

library(faraway)
seatpos
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
