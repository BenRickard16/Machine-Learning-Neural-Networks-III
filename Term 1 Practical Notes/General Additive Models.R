#Boston housing data
library(MASS)
y = Boston$medv
x = Boston$lstat
y.lab = 'Median Property Value'
x.lab = 'Lower Status (%)'

library(gam)
#Cubic spline 5 df for lstat, smoothing spline 5 df for indus, linear chas
gam = gam( medv ~ bs(lstat, degree = 3, df = 5) + s(indus, df = 5) + chas, 
           data = Boston )
par( mfrow = c(1,3) )
plot( gam,  se = TRUE, col = "blue" )

#chas is binary so want it as a factor
Boston1 = Boston
Boston1$chas = factor(Boston1$chas)

gam1 = gam( medv ~ bs(lstat, degree = 3, df = 5) + s(indus, df = 5) + chas, 
            data = Boston1 )
par(mfrow = c(1,3))
plot(gam1,  se = TRUE, col = "blue")

#Making predictions
preds <- predict( gam1, 
                  newdata = data.frame( chas = "0", indus = 3, lstat = 5 )  )
preds

#GAM
Boston1 <- Boston
Boston1$chas <- factor(Boston1$chas)

gam1 <- gam(medv ~ ns(lstat,df=5) + ns(nox,df=7) +
              s(indus,df=7) + poly(age,5) + chas, data=Boston1)

par(mfrow=c(2,3))
plot(gam1, se=TRUE, col="blue")
