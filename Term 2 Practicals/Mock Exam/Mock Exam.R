load(url("https://raw.githubusercontent.com/georgios-stats/Machine_Learning_and_Neural_Networks_III_Epiphany_2023/main/Exercises/misc/Homework_4/training.data.1.Rda"))
training.data.1$y
library(numDeriv)

#Prediction rule
prediction_rule <- function(x,w) {
  h <- w[1]+w[2]*x
  return (h)
}

#Loss function
loss_fun <- function(w,z) {
  x = z[1]
  y = z[2]
  h <- prediction_rule(x,w)
  ell <- abs(y-h)
  return (ell)
}


#Empirical risk function
empirical_risk_fun <- function(w, z, n) {
  x <- z[, 1]
  y <- z[, 2]
  R <- 0.0
  for (i in 1:n) {
    R <- R + loss_fun(w, c(x[i], y[i]))
  }
  R <- R / n
  return(R)
}

#Gradient Descent algorithm
n_obs <- 100
eta <- 0.5
Tmax <- 50
w_seed <- c(1,-1)
w <- w_seed
w_chain <- c()
Qstop <- 0 
t <- 0

while ( Qstop == 0 ) {
  # counter
  t <- t +  1
  cat( t ) ; cat( ' ' ) ## counter added for display reasons
  # step 1: update  
  erf_fun <- function(w, z = training.data.1, n=n_obs) {
    return( empirical_risk_fun(w, z, n) ) 
  }
  w <- w - eta * grad( erf_fun, w )
  w_chain <- rbind(w_chain, w)
  # step 2: check for termination terminate
  if ( t>= Tmax ) {
    Qstop <- 1
  }
}

par(mfrow=c(1,2))
#Plot chain {w_1^(t)} against t
plot(w_chain[,1], type='l')
#Plot chain {w_2^(t)} against t
plot(w_chain[,2], type='l')


mean(w_chain[,1][30:50])
mean(w_chain[,2][30:50])


#Much larger dataset
load(url("https://raw.githubusercontent.com/georgios-stats/Machine_Learning_and_Neural_Networks_III_Epiphany_2024/main/misc/mockCBE2/training.data.2.Rda"))

#Batch stochastic gradient descent
n_obs <- 100000
m <- 10 #batch size
Tmax <- 100
w_seed <- c(1,0)
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
    zbatch <- matrix(training.data.2[J,],1,2)
  }
  else {
    zbatch <- training.data.2[J,]
  }
  #eta <- learning_rate( t )
  eta <- 1 / t
  erf_fun <- function(w, z = zbatch, n=m) {
    return( empirical_risk_fun(w, z, n) ) 
  }
  w <- w - eta * numDeriv::grad( erf_fun, w )
  #w <- w - eta * grad_risk_fun( w, zbatch, m )
  w_chain <- rbind(w_chain, w)
  # step 2: check for termination terminate
  if ( t>= Tmax ) {
    Qstop <- 1
  }
}

plot(w_chain[,1], type='l')
plot(w_chain[,2], type='l')

mean(w_chain[,1][60:100])
mean(w_chain[,2][60:100])

#Vector support machine
load(url("https://raw.githubusercontent.com/georgios-stats/Machine_Learning_and_Neural_Networks_III_Epiphany_2024/main/misc/mockCBE2/train.data.4.Rda"))
head(train.data.4)

library("e1071")

svm.fit <- svm(y~., data=train.data.4, cost=10, kernel ='linear', 
               scaling=FALSE)
summary(svm.fit)

plot(svm.fit, data=train.data.4)

tune.out <- tune(METHOD=svm, y~., data=train.data.4, kernel='linear',
                 ranges=list(cost=seq(0.1,10,by=0.1)))

summary(tune.out)     

svm.fit <- svm(y~., data=train.data.4, cost=0.3, kernel ='linear', 
               scaling=FALSE)
summary(svm.fit)

plot(svm.fit, data=train.data.4)
