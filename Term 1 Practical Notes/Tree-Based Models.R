#Creating a synthetic dataset to explore cart
set.seed
data_surrogate <- data.frame(x = c(runif(200, 0, 0.4), runif(400, 0.4, 0.65), runif(300, 0.65, 1)),
                             y = c(rnorm(200, 1.5), rnorm(400, 0), rnorm(300, 2)))

plot(x=data_surrogate$x[1:200], y=data_surrogate$y[1:200], col='blue',
     xlab="X", ylab="Y", pch=19, xlim=c(0,1),ylim=c(-2,4))
points(x=data_surrogate$x[201:600], y=data_surrogate$y[201:600], col='red', pch=19)
points(x=data_surrogate$x[601:900], y=data_surrogate$y[601:900], col='green', pch=19)

#Building a regression tree
library(tree)
tree_fit <- tree(y~x, data_surrogate)
summary(tree_fit)
tree_fit

plot(tree_fit)
text(tree_fit, pretty=0)

#Making a test set
set.seed(347)
data_surrogate_test <- data.frame(x = c(runif(200, 0, 0.4), runif(400, 0.4, 0.65), runif(300, 0.65, 1)),
                                  y = c(rnorm(200, 1.5),    rnorm(400, 0),         rnorm(300, 2)))
tree_pred=predict(tree_fit,data_surrogate_test)

#Scatter plot with prediction
plot(x=data_surrogate_test$x[1:200], y=data_surrogate_test$y[1:200],
     col='blue', xlab="X", ylab="Y", pch=19, xlim=c(0,1), ylim=c(-2,4))
points(x=data_surrogate_test$x[201:600], y=data_surrogate_test$y[201:600], col='red', pch=19)
points(x=data_surrogate_test$x[601:900], y=data_surrogate_test$y[601:900], col='green', pch=19)
points(x=data_surrogate_test$x,y=tree_pred,col='black',pch=8)

pred_mse <- mean((data_surrogate_test$y-tree_pred)^2)

#Same but with less initial observations
set.seed(739)

data_surrogate_s <- data.frame(x = c(runif(20, 0, 0.4), runif(40, 0.4, 0.65), 
                                     runif(30, 0.65, 1)), y = c(rnorm(20, 1.5),    rnorm(40, 0),         rnorm(30, 2)))
plot(x=data_surrogate_s$x[1:20], y=data_surrogate_s$y[1:20], col='blue',
     xlab="X", ylab="Y", pch=19, xlim=c(0,1),ylim=c(-2,4))
points(x=data_surrogate_s$x[21:60], y=data_surrogate_s$y[21:60], col='red', pch=19)
points(x=data_surrogate_s$x[61:90], y=data_surrogate_s$y[61:90], col='green', pch=19)

tree_fit_s <- tree(y~x, data_surrogate_s)
summary(tree_fit_s)

plot(tree_fit_s)
text(tree_fit_s, pretty=0)

tree_pred_s=predict(tree_fit_s,data_surrogate_test)
plot(x=data_surrogate_test$x[1:200], y=data_surrogate_test$y[1:200],
     col='blue', xlab="X", ylab="Y", pch=19, xlim=c(0,1), ylim=c(-2,4))
points(x=data_surrogate_test$x[201:600], y=data_surrogate_test$y[201:600], col='red', pch=19)
points(x=data_surrogate_test$x[601:900], y=data_surrogate_test$y[601:900], col='green', pch=19)
points(x=data_surrogate_test$x,y=tree_pred_s,col='black',pch=8)

pred_mse <- mean((data_surrogate_test$y-tree_pred_s)^2)
pred_mse

#Have overfit the data
#Prune the tree with weakest link algorithm
tree_cv_prune <- cv.tree(tree_fit_s, FUN=prune.tree)
tree_cv_prune
plot(tree_cv_prune)

tree_fit_prune <- prune.tree(tree_fit_s, best=3)
plot(tree_fit_prune)
text(tree_fit_prune, pretty=0)

#Use pruned tree model to predict from test set and calculate MSE
tree_pred_prune=predict(tree_fit_prune,data_surrogate_test)
plot(x=data_surrogate_test$x[1:200], y=data_surrogate_test$y[1:200],
     col='blue', xlab="X", ylab="Y", pch=19, xlim=c(0,1), ylim=c(-2,4))
points(x=data_surrogate_test$x[201:600], y=data_surrogate_test$y[201:600], col='red', pch=19)
points(x=data_surrogate_test$x[601:900], y=data_surrogate_test$y[601:900], col='green', pch=19)
points(x=data_surrogate_test$x,y=tree_pred_prune,col='black',pch=8)

pred_mse_prune <- mean((data_surrogate_test$y-tree_pred_prune)^2)
pred_mse - pred_mse_prune

#Boston housing data
library(ISLR)
library(tree)
library(MASS)

#Divide the data set into half training and half test
set.seed(1)
train <- sample(1:nrow(Boston), nrow(Boston)/2)
data_train <- Boston[train,]
data_test <- Boston[-train,]

tree.boston <- tree(medv~., data_train)
summary(tree.boston)

plot(tree.boston)
text(tree.boston, pretty=0)

#Predicting medv with test set and calculate MSE
yhat <- predict(tree.boston, data_test)
mean((yhat - data_test$medv)^2)

#Prune tree
cv.boston <- cv.tree(tree.boston)
plot(cv.boston)

prune.boston <- prune.tree(tree.boston, best=6)
plot(prune.boston)
text(prune.boston, pretty=0)

#Predict medv with pruned tree
yhat <- predict(prune.boston, data_test)
mean((yhat - data_test$medv)^2)

#Bagging and random forests
library(randomForest)

set.seed(472)
bag.boston <- randomForest(medv~., data=data_train, mtry=12, ntree=10,
                           importance=TRUE)
bag.boston
#All predictors are considered, hence this is bagging
#10 bootstrap trees are generated
#MSR and %Var explained are based on out-of-bag estimates

#Evaluating performance of bagged model
pred.bag <- predict(bag.boston, newdata=data_test)
plot(pred.bag, data_test$medv)
abline(0,1)

mean((pred.bag-data_test$medv)^2)

#Check with 100 and 1000 bootstrap trees
bag.boston <- randomForest(medv~.,data=data_train,mtry=12,ntree=100,importance=TRUE)
pred.bag <- predict(bag.boston, newdata=data_test)
mean((pred.bag-data_test$medv)^2)

bag.boston <- randomForest(medv~.,data=data_train,mtry=12,ntree=1000,importance=TRUE)
pred.bag <- predict(bag.boston, newdata=data_test)
mean((pred.bag-data_test$medv)^2)

#Random forest
#Default p/3 variables but sometimes use sqrt(p)
rf.boston <- randomForest(medv~.,data=data_train,mtry=4,ntree=100,importance=TRUE)
pred.rf <- predict(rf.boston, newdata=data_test)
mean((pred.rf - data_test$medv)^2)

#Importance of each variable
importance(rf.boston)
varImpPlot(rf.boston)

#Write a for loop to record test set prediction MSE for all 12 possible
#values of mtry
test.err=double(12)
for(mtry_t in 1:12){
  fit=randomForest(medv~.,data=data_train,mtry=mtry_t,ntree=100)
  pred=predict(fit,data_test)
  test.err[mtry_t]=mean(( pred -data_test$medv)^2)
}

plot(test.err)

#Boosted regression trees
library(gbm)

set.seed(517)
boost.boston <- gbm(medv~.,data=data_train,distribution="gaussian",
                    n.trees=1000,interaction.depth=2)
summary(boost.boston)
#Gaussian for regression, bernoulli for binary classification
#1000 trees, 2 splits on each tree

pred.boost <- predict(boost.boston,newdata=data_test,n.trees=1000)
mean((pred.boost-data_test$medv)^2)
#Default shrinkage parameter lambda is 0.001 but can specify in gbm()

library(ISLR)
library(tree)

attach(Carseats) #Means we can call variables just by name
?Carseats
#Sales of car seats is response
#Continuous response variable, for a classification tree need a binary variable
High=ifelse(Sales<=8,"No","Yes")
Carseats=data.frame(Carseats, High)
Carseats$High <- as.factor(Carseats$High)

#Classification tree
tree.carseats <- tree(High~.-Sales, data=Carseats)
summary(tree.carseats)

par(mfrow=c(1,1))
plot(tree.carseats)
text(tree.carseats, pretty=0)

#Split data into test and train to evaluate model performance
set.seed(743)
train_index <- sample(1:nrow(Carseats),250)
data_train <- Carseats[train_index,]
data_test <- Carseats[-train_index,]

tree.carseats <- tree(High~.-Sales, data_train)
plot(tree.carseats);text(tree.carseats,pretty=0)

#Plot a table with predictions against true values
#Can calculate percentage of correct predictions
tree.pred <- predict(tree.carseats, data_test, type="class")
table(tree.pred, data_test$High)

#Pruning to reduce overfitting
cv.carseats <- cv.tree(tree.carseats, FUN=prune.misclass)
cv.carseats

plot(cv.carseats)

prune.carseats <- prune.misclass(tree.carseats, best=14)
plot(prune.carseats);text(prune.carseats,pretty=0)

tree.pred_prune <- predict(prune.carseats, data_test, type="class")
table(tree.pred_prune, data_test$High)

#Credit scoring looking to predict Status
library(modeldata)
library(randomForest)
library(gbm)

data(credit_data)
credit_data <- na.omit(credit_data)
head(credit_data)

#Divide data into training and testing
set.seed(180)
train <- sample(1:nrow(credit_data), floor(nrow(credit_data)/2))
data_train <- credit_data[train,]
data_test <- credit_data[-train,]

#Standard CART tree
tree.credit <- tree(Status ~ ., data_train)
summary(tree.credit)

plot(tree.credit)
text(tree.credit, pretty=0)

#Calculate percentage of correct predictions
tree.pred <- predict(tree.credit, data_test, type="class")
class_table <- table(tree.pred, data_test$Status)
success_rate <- (class_table[1,1]+class_table[2,2])/sum(class_table)

#Pruning with size 4
prune.credit <- prune.misclass(tree.credit, best=4)
plot(prune.credit);text(prune.credit, pretty=0)
#Calculate percentage of correct predictions
tree.pred <- predict(prune.credit, data_test, type="class")
class_table <- table(tree.pred, data_test$Status)
success_rate <- (class_table[1,1]+class_table[2,2])/sum(class_table)

#Random forest with 100 bootstrap samples with 4 predictors each split
set.seed(472)
rf.credit <- randomForest(Status~.,data=data_train,mtry=3,
                          ntree=100,importance=TRUE)
#Which predictor is most important to improve Gini index
importance(rf.credit)
varImpPlot(rf.credit)

#Prediction accuracy
pred.rf <- predict(rf.credit, newdata=data_test, type="class")
class_table <- table(pred.rf, data_test$Status)
success_rate <- (class_table[1,1]+class_table[2,2])/sum(class_table)

#Boosting model
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

set.seed(347)
data_surrogate_test <- data.frame(x = c(runif(200, 0, 0.4), runif(400, 0.4, 0.65), runif(300, 0.65, 1)),
                                  y = c(rnorm(200, 1.5),    rnorm(400, 0),         rnorm(300, 2)))
tree_pred=predict(tree_fit,data_surrogate_test)