install.packages('nnet')
library(nnet)

#Regression problem with one output
#Ozone dataset
library(faraway)
data(ozone)
ozone

#FFNN with one hidden layer with 2 units and linear activation output
nn.out.1 <- nnet(O3 ~ temp + ibh + ibt, ozone, size=2, linout=TRUE)

#Error function (squared error)
EF <- sum((nn.out.1$fitted.values-ozone$O3)^2)
EF  

#RSS - unexplained variation if we have no input features
RSS  <- sum((ozone$O3-mean(ozone$O3))^2)
RSS

#They are close, so our naively trained NN has no predictive ability
#This is possibly due to a careless training of the NN, aka, the estimated weights are 
#not good choices
#This is because training/learning NN is a non-convex learning problem, aka there are many
#local minima, some of them away from the global minimum. 
#Need to find a way to discover better values for the weights
#It can be done by standardizing the dataset values, and training the NN multiple times 
#with different starting values for the weights (aka seeds)

#Standardise the inputs
apply(ozone,2,sd)
ozone.rescaled <- scale(ozone) 
apply(ozone.rescaled,2,mean)
apply(ozone.rescaled,2,sd)

#Use rescaled ozone data to fit the FFNN 100 times using a different seed each time
#Find the one with smallest EF
Nrealizations <- 100

nn.out.1.best <- nnet(O3 ~ temp + ibh + ibt, ozone.rescaled, size=2, linout=T)
#EF.best <- sum((nn.out.1.best$fitted.values-ozone.rescaled[,1])^2)
EF.best <- nn.out.1.best$value
#
for (r in 1:Nrealizations) {
  #
  set.seed( r )
  #
  nn.out.1.new <- nnet(O3 ~ temp + ibh + ibt, ozone.rescaled, size=2, linout=T)
  EF.new <- nn.out.1.new$value
  #
  if (EF.new < EF.best) {
    #
    nn.out.1.best <- nn.out.1.new
    #
    EF.best <- EF.new
  }
}

EF.best

RSS  <- sum((ozone.rescaled[,1]-mean(ozone.rescaled[,1]))^2)
RSS

#Estimated weights
summary(nn.out.1.best)

#Plot the predicted O3 values for temp in the range (−3,3), while the other two input 
#variables ibh and ibt are fixed to points ibh=0, ibt=0.

#Create the input object x that will be used in the function predict() 
xx <- expand.grid(temp=seq(-3,3,0.1),ibh=0,ibt=0)

#Make the predictions
pred.1.best <- predict(nn.out.1.best,new=xx)

#Your trained model is scaled, you need to bring its unites (input/output) back to the
#natural scale
#Here is how you get the mean and variances of the original data set
ozmeans <- attributes(ozone.rescaled)$"scaled:center"
ozscales <- attributes(ozone.rescaled)$"scaled:scale"

#Apply the re-scaling back to the original data for the inputs
xx.rescaled <- xx$temp*ozscales['temp']+ozmeans['temp']

#Apply the re-scaling back to the original data for the inputs
pred.1.best.rescaled <- pred.1.best*ozscales['O3']+ozmeans['O3']

#Plot
plot(xx.rescaled, pred.1.best.rescaled,
     cex=2,xlab="Temp",ylab="O3")

#Do the same but with ibh and ibt as the predictors
#Create the input object x that will be used in the function predict() 
xx <-expand.grid(temp=0,ibh=seq(-3,3,0.1),ibt=0)

#Make the predictions
pred.1.best <- predict(nn.out.1.best,new=xx)

#Your trained model is scaled, you need to bring its unites (input/output) back to the 
#natural scale
#Here is how you get the mean and varances of the original data set
ozmeans <- attributes(ozone.rescaled)$"scaled:center"
ozscales <- attributes(ozone.rescaled)$"scaled:scale"

#Apply the re-scaling back to the original data for the inputs
xx.rescaled <- xx$ibh*ozscales['ibh']+ozmeans['ibh']

#Apply the re-scaling back to the original data for the inputs
pred.1.best.rescaled <- pred.1.best*ozscales['O3']+ozmeans['O3']

#Plot
plot(xx.rescaled, pred.1.best.rescaled,
     cex=2,xlab="ibh",ylab="O3")

#Create the input object x that will be used in the function predict() 
xx <-expand.grid(temp=0,ibh=0,ibt=seq(-3,3,0.1))  

#Make the predictions
pred.1.best <- predict(nn.out.1.best,new=xx)

#Your trained model is scaled, you need to bring its unites (input/output) back to the 
#natural scale
#Here is how you get the mean and varances of the original data set
ozmeans <- attributes(ozone.rescaled)$"scaled:center"
ozscales <- attributes(ozone.rescaled)$"scaled:scale"

#Apply the re-scaling back to the original data for the inputs
xx.rescaled <- xx$ibt*ozscales['ibt']+ozmeans['ibt']

#Apply the re-scaling back to the original data for the inputs
pred.1.best.rescaled <- pred.1.best*ozscales['O3']+ozmeans['O3']

#Plot
plot(xx.rescaled, pred.1.best.rescaled,
     cex=2,xlab="ibt",ylab="O3")

#Discontinuities may be due to unreasonably large weights
#NN training tends to produce large weights in order to optimize the fit against the 
#training data, but the predictions will be unstable, especially for extrapolation
#Implement a ridge shrinkage parameter decay=0.001
Nrealizations <- 100

nn.out.1.decay.best <- nnet(O3 ~ temp + ibh + ibt, ozone.rescaled, size=2, linout=T, decay=0.001)
#EF.decay.best <- sum((nn.out.1.decay.best$fitted.values-ozone.rescaled[,1])^2)
EF.decay.best <- nn.out.1.decay.best$value
#
for (r in 1:Nrealizations) {
  #
  set.seed( r )
  #
  nn.out.1.new <- nnet(O3 ~ temp + ibh + ibt, ozone.rescaled, size=2, linout=T, decay=0.001)
  EF.new <- nn.out.1.new$value
  #
  if (EF.new < EF.decay.best) {
    #
    nn.out.1.decay.best <- nn.out.1.new
    #
    EF.decay.best <- EF.new
  }
}

#Print error function and weights
EF.decay.best
summary(nn.out.1.decay.best)

#Plot the predicted, O3 values for ibt in the range (−3,3), while the other two input 
#variables are fixed to points ibh=0, temp=0
#Create the input object x that will be used in the function predict() 
xx <- expand.grid(temp=0,ibh=0,ibt=seq(-3,3,0.1))  

#Make the predictions
pred.1.best <- predict(nn.out.1.decay.best,new=xx)

#Your trained model is scaled, you need to bring its unites (input/output) back to the 
#natural scale
# Here is how you get the mean and varances of the original data set
ozmeans <- attributes(ozone.rescaled)$"scaled:center"
ozscales <- attributes(ozone.rescaled)$"scaled:scale"

#Apply the re-scaling back to the original data for the inputs
xx.rescaled <- xx$ibt*ozscales['ibt']+ozmeans['ibt']

#Apply the re-scaling back to the original data for the inputs
pred.1.best.rescaled <- pred.1.best*ozscales['O3']+ozmeans['O3']

#plot
plot(xx.rescaled, pred.1.best.rescaled,
     cex=2,xlab="ibt",ylab="O3",
     type="l")


#Multi-Class Classification Problem
data(iris)
iris

#Modifying the dataset
y <- class.ind( c(rep("s", 50), rep("c", 50), rep("v", 50)) )
x <-iris[,-c(4,5)] 

#Create training and validation dataset
ind <- c(sample(1:50,25), sample(51:100,25), sample(101:150,25))
y_train <- y[ind,]
x_train <- x[ind,]

y_valid <- y[-ind,]
x_valid <- x[-ind,]

#Train the NN
#For given feature x, classify it as s, c, or v
#Use one hidden layer with 2 neurons, softmax activation, decay=0.00001, max number of
#iterations for SGD is 200
nnet.2.out <- nnet(x_train, y_train, size = 2, decay = 5e-4, maxit = 200, softmax=T)

#Predict classifications from validation set and compare to true values
nnet.2.pred <- predict(nnet.2.out, x_valid)
max.col(nnet.2.pred)

max.col(y_valid)

match = mean(max.col(nnet.2.pred)==max.col(y_valid))
match 
