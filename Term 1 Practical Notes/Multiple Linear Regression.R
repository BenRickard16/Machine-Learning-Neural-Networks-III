#Used car data
Price<-c(85, 103,  70,  82,  89,  98,  66,  95, 169,  70,  48)
Age<- c(5, 4, 6, 5, 5, 5, 6, 6, 2, 7, 7)
Miles<-c(57,40,77,60,49,47,58,39,8,69,89)
carSales<-data.frame(Price=Price,Age=Age,Miles=Miles)


# Scatterplot matrix
# Customize upper panel
upper.panel<-function(x, y){
  points(x,y, pch=19, col=4)
  r <- round(cor(x, y), digits=3)
  txt <- paste0("r = ", r)
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  text(0.5, 0.9, txt)
}
pairs(carSales, lower.panel = NULL, 
      upper.panel = upper.panel)

#Multiple linear regression
reg <- lm(Price ~ Age + Miles, data=carSales)
summary(reg)
#CI for coefficients
confint(reg, level=0.95)

#Calculating variance inflation factors to check for multicollinearity
library(car)
vif(reg)



#Multiple Linear Regression with body fat data
library("faraway")
names(fat)
fat1 <- fat[,-c(2,3,8)]
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