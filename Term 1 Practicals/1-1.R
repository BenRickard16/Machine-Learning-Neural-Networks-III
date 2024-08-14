data(faithful)

#Exercise 1.1 - exploratory data analysis
w <- faithful$waiting
d <- faithful$eruptions

plot(y=w,x=d)
cor(w,d,)

#Exercise 1.2 - fit the linear model
model <- lm(w~d)

#Exercise 1.3 - extracting information
coef(model)
beta0hat <- coef(model)[1]
beta1hat <- coef(model)[2]

residuals <- resid(model)
lsq.Q <- sum(residuals^2)

#Exercsie 1.4 - summary function
summ <- summary(model)

#Column t value shows t-test associated with testing the
#significance of the parameter listed in first column
#Both coeffs. are significant, as p-values are very small

se <- summ$sigma
rsq <- summ$r.squared
cor(w,d)^2

#Exercise 1.5 - inference on the coefficients
se.beta1 <- summ$coefficients[2,2]

t.beta1 <- (beta1hat - 0)/se.beta1

n<- length(w)
pval <- 2*(1-pt(t.beta1, n-2))

conf <- beta1hat+c(-1,1)*qt(0.975,n-2)*se.beta1
confint(model, parm='d', level=0.975)

#Exercise 1.6 - estimation and prediction
predict(model, newdata = data.frame(d = 3), interval='confidence', level=0.95)
predict(model, newdata = data.frame(d = 3), interval='prediction', level=0.95)

#Exercise 1.7 - residual analysis
par(mfrow=c(2,2))
plot(y=resid(model), x=fitted(model))
plot(y=resid(model), x=d)

hist(residuals)
qqnorm(residuals)
