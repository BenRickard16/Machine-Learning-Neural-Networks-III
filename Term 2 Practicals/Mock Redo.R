load(url("https://raw.githubusercontent.com/georgios-stats/Machine_Learning_and_Neural_Networks_III_Epiphany_2023/main/Exercises/misc/Homework_4/training.data.1.Rda"))

#Loss function
loss_fun <- function(w,z) {
  x <- z[1]
  y <- z[2]
  loss_fun <- abs( y -w[1] - w[2]*x )
}

#Empirical risk function
empirical_risk_fun <- function(w,z,n) {
  erf <- 0.0
  for (i in 1:n) {
    x <- z[i,1]
    y <- z[i,2]
    erf <- erf + abs( y -w[1] - w[2]*x )  
  }
  erf <- erf/n
  return( erf )
}

head(training.data.1)

#Gradient descent algorithm
z_obs <- as.matrix(training.data.1)
n_obs <- dim(z_obs)[1]

eta <- 0.5
Tmax <- 50
w_seed <- c(-0,0)
w <- w_seed
w_chain <- c()
Qstop <- 0 
t <- 0
while ( Qstop == 0 ) {
  # counter
  t <- t +  1
  cat( t ) ; cat( ' ' ) ## counter added for display reasons
  # step 1: update  
  erf_fun <- function(w, z = z_obs, n=n_obs) {
    return( empirical_risk_fun(w, z, n) ) 
  }
  w <- w - eta * numDeriv::grad( erf_fun, w )
  w_chain <- rbind(w_chain, w)
  # step 2: check for termination terminate
  if ( t>= Tmax ) {
    Qstop <- 1
  }
}

#Plot the {w_t}s 
par(mfrow=c(1,2))
plot(w_chain[,1], type='l') 
plot(w_chain[,2], type='l') 

#Find the best w
colMeans(w_chain[floor(0.75*Tmax):Tmax,])

#Stochastic gradient descent m=10
load(url("https://raw.githubusercontent.com/georgios-stats/Machine_Learning_and_Neural_Networks_III_Epiphany_2024/main/misc/mockCBE2/training.data.2.Rda"))

#Loss function
loss_fun <- function(w,z) {
  x <- z[1]
  y <- z[2]
  loss_fun <- abs( y -w[1] - w[2]*x )
}

#Empirical risk function
empirical_risk_fun <- function(w,z,n) {
  erf <- 0.0
  for (i in 1:n) {
    x <- z[i,1]
    y <- z[i,2]
    erf <- erf + abs( y -w[1] - w[2]*x )  
  }
  erf <- erf/n
  return( erf )
}

head(training.data.2)

#SGD algorithm
z_obs <- as.matrix(training.data.2)
n_obs <- dim(z_obs)[1]

m <- 10
eta <- 0.8
Tmax <- 100
w_seed <- c(-3,3)
w <- w_seed
w_chain <- c()
Qstop <- 0 
t <- 0
while ( Qstop == 0 ) {
  # counter
  t <- t +  1
  cat( t ) ; cat( ' ' ) ## counter added for display reasons
  # step 1: update  
  J <- sample.int(n = n_obs, size = m, replace = TRUE)
  if (m==1) {
    zbatch <- matrix(z_obs[J,],1,2)
  }
  else {
    zbatch <- z_obs[J,]
  }
  #eta <- learning_rate( t )
  erf_fun <- function(w, z = zbatch, n=m) {
    return( empirical_risk_fun(w, z, n) ) 
  }
  w <- w - eta * numDeriv::grad( erf_fun, w )
  w_chain <- rbind(w_chain, w)
  # step 2: check for rtermination terminate
  if ( t>= Tmax ) {
    Qstop <- 1
  }
}

#Plot the {w_t}
plot(w_chain[,1], type='l') 
plot(w_chain[,2], type='l')

#Optimal w
colMeans(w_chain[50:100,])

#Artificial Neural Networks

#Training data
load(url("https://raw.githubusercontent.com/georgios-stats/Machine_Learning_and_Neural_Networks_III_Epiphany_2024/main/misc/mockCBE2/train.data.3.Rda"))

#Evaluation data
load(url("https://raw.githubusercontent.com/georgios-stats/Machine_Learning_and_Neural_Networks_III_Epiphany_2024/main/misc/mockCBE2/valid.data.3.Rda"))

#Neural network 1 hidden layer of 10 neurons
#Decay 0.001
library(nnet)

y_train <- class.ind(train.data.3[,1])
x_train <- train.data.3[,-1]
y_valid<- class.ind(valid.data.3[,1])
x_valid <- valid.data.3[,-1]

Nrealizations <- 100

nnet.out.best <- nnet(x_train, y_train, size=10, decay=0.001)

nnet.out.pred <- predict(nnet.out.best, x_valid)
best.match.best <- mean(  max.col(nnet.out.pred) == max.col(y_valid) )

for (r in 1:Nrealizations) {
  set.seed( r )
  nnet.out.new <- nnet(x_train, y_train, 
                       size=10, 
                       decay=0.001)

  nnet.out.pred <- predict(nnet.out.new, x_valid)
  best.match.new <- mean(  max.col(nnet.out.pred) == max.col(y_valid) )

  if (best.match.best < best.match.new) {
    nnet.out.best <- nnet.out.new
    best.match.best <- best.match.new
  }
}  

best.match.best

#Make a prediction
load(url("https://raw.githubusercontent.com/georgios-stats/Machine_Learning_and_Neural_Networks_III_Epiphany_2024/main/misc/mockCBE2/future.data.3.Rda") )

x_new <- future.data.3
nnet.new <- predict(nnet.out.best, x_new)
max.col(nnet.new)

#Support vector machine
load(url("https://raw.githubusercontent.com/georgios-stats/Machine_Learning_and_Neural_Networks_III_Epiphany_2024/main/misc/mockCBE2/train.data.4.Rda"))

library(e1071)
tune.out <- tune(svm, y ~ ., data = train.data.4, 
                 kernel = "linear", 
                 ranges = list(
                   cost = seq(from = 0.1, to = 10, length.out=100)
                 )
)
summary(tune.out)

plot(tune.out$best.model, data = train.data.4)
