install.packages("e1071")
library("e1071")

#Load in data
set.seed(1)
x <- matrix(rnorm(20 * 2), ncol = 2)
y <- c(rep(-1, 10), rep(1, 10))
x[y == 1, ] <- x[y == 1, ] + 1
plot(x, col = (3 - y))

#See not linearly separable, so a soft SVM may be suitable
#To use e1071, need have a dataframe
dat = data.frame(x, y = as.factor(y))

#Fit SVM classifier with cost 10, linear kernel, and no scaling
svm.fit <- svm(y~., data=dat, cost=10, kernel ='linear', 
               scaling=FALSE)
summary(svm.fit)

plot(svm.fit, dat) #Plots as x are the support vectors

#Investigate the output
#Get the support vectors
print(svm.fit$SV)
#Index of the support vectors
print(svm.fit$index)

#Compute the linear coefficients w and b
X <- cbind(dat$X1, dat$X2)
w_est <- drop(t(svm.fit$coefs)%*%X[svm.fit$index,])
b_est <- svm.fit$rho

#Investigate the cost argument
#Cost = 0.1, smaller than previous
svmfit <- svm(y ~., data=dat, kernel='linear', cost=0.1,
              scale=FALSE)
plot(svmfit, dat)

svmfit$index
#Now cost parameter, obtain larger number of support vectors as
#the margin is now wider

#Cross validation for tuning
set.seed(1)

tune.out <- tune(METHOD=svm, y~., data=dat, kernel='linear',
                 ranges=list(cost=c(0.001,0.01,0.1,1,5,10,100)))

summary(tune.out)
#Cost=0.1 gives lowest c-v error rate

bestmod <- tune.out$best.model
summary(bestmod)

#Make predictions
xtest <- matrix(rnorm(20*2), ncol=2)
ytest <- sample(c(-1,1), 20, rep=TRUE)
xtest[ytest==1,] <- xtest[ytest==1,] + 1
testdat <- data.frame(xtest, y=as.factor(ytest))

ypred <- predict(bestmod, testdat)

#Gives confusion matrix (miss-classification table)
table(predict=ypred, truth=testdat$y)


#SVM with polynomial kernel
#Generating the data
set.seed(1)
N = 1000
xx = rnorm(N)
yy = 4 * xx^2 + 1 + rnorm(N)
class = sample(N, N/2)
yy[class] = yy[class] + 6
yy[-class] = yy[-class] - 6
x <- cbind(xx,yy)
plot(x[class,1], x[class,2], col = "red", xlab = "X", ylab = "Y", ylim = c(-6, 30)) +
  points(x[-class,1], x[-class,2], col = "blue")

y = rep(-1, N)
y[class] = 1
data = data.frame(x, y = as.factor(y))
train = sample(N, N/2)
data_train = data[train, ]
data_eval = data[-train, ]

#Fitting the SVM
svmfit <- svm(y ~ ., 
              data = data_train, 
              kernel = "polynomial",  
              degree = 2, 
              gamma = 1,
              coef0 = 0,
              cost = 1, 
              scale = FALSE)

plot(svmfit, data_train)

#CV to tune
set.seed(1)
tune.out <- tune(svm, y ~ ., data = data_train, 
                 kernel = "polynomial", 
                 ranges = list(
                   cost = c(0.01, 0.05, 0.1),
                   gamma = c(0.5,1,2.0),
                   coef0 = c(0,1,2),
                   degree = c(1,2,3)
                 )
)
summary(tune.out)

#With radial kernel
x <- matrix(rnorm(200*2), ncol = 2)
x[1:100,] <- x[1:100,] + 2.5
x[101:150,] <- x[101:150,] - 2.5
y <- c(rep(1,150), rep(2,50))
dat <- data.frame(x,y=as.factor(y))
plot(x, col = y + 1)

svmfit <- svm( factor(y) ~ ., 
               data = dat, 
               scale = FALSE, 
               kernel = "radial", 
               gamma = 1/2,
               cost = 10)

plot(svmfit, dat)
