install.packages("ISLR2")
library("ISLR2")
#Data on credit card balance, income, and whether they've defaulted and are a
#student or not
data(Default)
head(Default)

pairs(Default)
boxplot(balance~default, data=Default)
boxplot(income~default, data=Default)

#Linear model
lm.fit <- lm(as.numeric(default=="Yes")~balance, data=Default)
summary(lm.fit)

plot(Default$balance, as.numeric(Default$default=="Yes"),col="red",xlab="balance",ylab="default")
abline(lm.fit, col="blue", lwd = 2)

par(mfrow=c(1,2))
plot(y=resid(lm.fit), x=fitted(lm.fit))
qqnorm(resid(lm.fit))
par(mfrow=c(1,1))

#Simple logistic regression
glm.fit = glm(as.numeric(Default$default=="Yes") ~ balance, data = Default,
              family = "binomial")
summary(glm.fit)
summary(glm.fit$fitted.values)

plot(Default$balance, as.numeric(Default$default=="Yes"),col="red",
     xlab="balance",ylab="default")
points(glm.fit$data$balance,glm.fit$fitted.values, col = "black", pch = 4)
curve(predict(glm.fit,data.frame(balance = x),type="resp"),col="blue",
      lwd=2,add=TRUE)

#Significance of coefficients is z-test now instead of t-test
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
