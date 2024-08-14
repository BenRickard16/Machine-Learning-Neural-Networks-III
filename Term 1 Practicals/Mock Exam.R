install.packages("ISLR")
install.packages("tree")
library(ISLR)
library(tree)

attach(Carseats)
?Carseats

High=ifelse(Sales<=8,"No","Yes")
Carseats=data.frame(Carseats, High)
Carseats$High <- as.factor(Carseats$High)

tree.carseats <- tree(High~.-Sales, data=Carseats)
summary(tree.carseats)

par(mfrow=c(1,1))
plot(tree.carseats)
text(tree.carseats, pretty=0)

set.seed(743)
train_index <- sample(1:nrow(Carseats),250)
data_train <- Carseats[train_index,]
data_test <- Carseats[-train_index,]

tree.carseats <- tree(High~.-Sales, data_train)
plot(tree.carseats);text(tree.carseats,pretty=0)

tree.pred <- predict(tree.carseats, data_test, type="class")
table(tree.pred, data_test$High)

cv.carseats <- cv.tree(tree.carseats, FUN=prune.misclass)
cv.carseats

plot(cv.carseats)

prune.carseats <- prune.misclass(tree.carseats, best=14)
plot(prune.carseats);text(prune.carseats,pretty=0)

tree.pred_prune <- predict(prune.carseats, data_test, type="class")
table(tree.pred_prune, data_test$High)

install.packages("modeldata")
install.packages("randomForest")
install.packages("gbm")
library(modeldata)
library(randomForest)
library(gbm)

data(credit_data)
credit_data <- na.omit(credit_data)
head(credit_data)

set.seed(180)
train <- sample(1:nrow(credit_data), floor(nrow(credit_data)/2))
data_train <- credit_data[train,]
data_test <- credit_data[-train,]

tree.credit <- tree(Status ~ ., data_train)
summary(tree.credit)

plot(tree.credit)
text(tree.credit, pretty=0)

tree.pred <- predict(tree.credit, data_test, type="class")
class_table <- table(tree.pred, data_test$Status)
success_rate <- (class_table[1,1]+class_table[2,2])/sum(class_table)

prune.credit <- prune.misclass(tree.credit, best=4)
plot(prune.credit);text(prune.credit, pretty=0)

tree.pred <- predict(prune.credit, data_test, type="class")
class_table <- table(tree.pred, data_test$Status)
success_rate <- (class_table[1,1]+class_table[2,2])/sum(class_table)

set.seed(472)
rf.credit <- randomForest(Status~.,data=data_train,mtry=3,
                          ntree=100,importance=TRUE)
importance(rf.credit)
varImpPlot(rf.credit)

pred.rf <- predict(rf.credit, newdata=data_test, type="class")
class_table <- table(pred.rf, data_test$Status)
success_rate <- (class_table[1,1]+class_table[2,2])/sum(class_table)

boost.credit <- gbm(unclass(Status)-1~.,data=data_train,
                    distribution="bernoulli",n.trees=1000,
                    interaction.depth =2)
summary(boost.credit)


pred.boost <- predict(boost.credit,newdata=data_test,n.trees=1000,
                      type="response")
status_pred <- ifelse(pred.boost<=0.5,"bad","good")
class_table <- table(status_pred, data_test$Status)
success_rate <- (class_table[1,1]+class_table[2,2])/sum(class_table)
success_rate
