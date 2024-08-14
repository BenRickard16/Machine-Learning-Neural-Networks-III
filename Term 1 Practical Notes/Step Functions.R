#Boston housing data again
library("MASS")
y <- Boston$medv
x <- Boston$lstat
y.lab <- 'Median Property Value'
x.lab <- 'Lower Status (%)'

step2 <- lm(y~cut(x,2)) #1 cut, 2 intervals
step3 <- lm(y~cut(x,3)) 
step4 <- lm(y~cut(x,4)) 
step5 <- lm(y~cut(x,5)) 

pred2 = predict(step2, newdata = list(x = sort(x)), se = TRUE)
pred3 = predict(step3, newdata = list(x = sort(x)), se = TRUE)
pred4 = predict(step4, newdata = list(x = sort(x)), se = TRUE)
pred5 = predict(step5, newdata = list(x = sort(x)), se = TRUE)

se.bands2 = cbind(pred2$fit + 2*pred2$se.fit, pred2$fit-2*pred2$se.fit)
se.bands3 = cbind(pred3$fit + 2*pred3$se.fit, pred3$fit-2*pred3$se.fit)
se.bands4 = cbind(pred4$fit + 2*pred4$se.fit, pred4$fit-2*pred4$se.fit)
se.bands5 = cbind(pred5$fit + 2*pred5$se.fit, pred5$fit-2*pred5$se.fit)

par(mfrow = c(2,2))

plot(x, y, cex.lab = 1.1, col="darkgrey", xlab = x.lab, ylab = y.lab,
     main = "1 cutpoint", bty = 'l')
lines(sort(x), pred2$fit, lwd = 2, col = "red")
matlines(sort(x), se.bands2, lwd = 1.4, col = "red", lty = 3)

plot(x, y, cex.lab = 1.1, col="darkgrey", xlab = x.lab, ylab = y.lab,
     main = "2 cutpoints", bty = 'l')
lines(sort(x), pred3$fit, lwd = 2, col = "darkviolet")
matlines(sort(x), se.bands3, lwd = 1.4, col = "darkviolet", lty = 3)

plot(x, y, cex.lab = 1.1, col="darkgrey", xlab = x.lab, ylab = y.lab,
     main = "3 cutpoints", bty = 'l')
lines(sort(x), pred4$fit, lwd = 2, col = "blue")
matlines(sort(x), se.bands4, lwd = 1.4, col = "blue", lty = 3)

plot(x, y, cex.lab = 1.1, col="darkgrey", xlab = x.lab, ylab = y.lab,
     main = "4 cutpoints", bty = 'l')
lines(sort(x), pred5$fit, lwd = 2, col = "black")
matlines(sort(x), se.bands5, lwd = 1.4, col = "black", lty = 3)

#Can manually pick the cutpoints
breaks4 <- c(min(x),10,20,30,max(x))
table(cut(x, breaks=breaks4))

step.new4 <- lm(y~cut(x, breaks=breaks4))
summary(step.new4)

#Predictions
newx <- c(10.56, 5.89)
preds <- predict(step.new4, newdata=list(x=newx), se=TRUE)
preds

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