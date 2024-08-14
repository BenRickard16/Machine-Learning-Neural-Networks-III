set.seed(2023)

#install.packages("nloptr")
#install.packages("numDeriv")

library(nloptr)
library(numDeriv)
#Binary classification problem output y E {0,1}

#Generates the dataset S_n = {z_i = (x_i, y_i)}
data_generating_model <- function(n,w) {
  z <- rep( NaN, times=n*2 )
  z <- matrix(z, nrow = n, ncol = 2)
  z[,1] <- runif(n, min = -10, max = 10)
  p <- w[1] + w[2]*z[,1] 
  p <- exp(p) / (1+exp(p))
  z[,2] <- rbinom(n, size = 1, prob = p)
  return(z)
}

n_obs <- 500
w_true <- c(0,1)  
z_obs <- data_generating_model(n = n_obs, w = w_true) 
w_true <- as.numeric(glm(z_obs[,2]~ 1+ z_obs[,1],
                    family = "binomial" )$coefficients)

#Prediction rule
prediction_rule <- function(x,w) {
  h <- w[1]+w[2]*x
  h <- exp(h) / (1.0 + exp(h) )
  return (h)
}

#Loss function
loss_fun <- function(w,z) {
  x = z[1]
  y = z[2]
  h <- prediction_rule(x,w)
  ell <- -y*log(h) - (1-y)*log(1-h)
  return (ell)
}

#Empirical risk function
empirical_risk_fun <- function(w,z,n) {
  x = z[,1]
  y = z[,2]
  R <- 0.0
  for (i in 1:n) {
    R <- R + loss_fun(w,z[i,])
  }
  R <- R / n
  return (R)
}

#Learning rate eta
learning_rate <-function(t,t0=3) {
  eta <- t0 / t
  return( eta )
}

#Returns gradient of the loss function at parameter value w and at 
#example value  z=(x,y)
grad_loss_fun <- function(w,z) {
  x = z[1]
  y = z[2]
  h <- prediction_rule(x,w)
  grd <- c(h-y, (h-y)*x)
  return (grd)
}

#Returns gradient of loss function at parameter value w and using
#dataset z size nx2
grad_risk_fun <- function(w,z,n) {
  grd <- 0.0
  for (i in 1:n) {
    grd <- grd + grad_loss_fun(w,z[i,])
  }
  grd <- grd / n
  return (grd)
}

#Gradient of empirical risk function at w=(-0.1,1.5)^T using whole 
#dataset z
gr <- grad_risk_fun(c(-0.1,1.5), z_obs, n_obs)

#Gradient of empirical risk function at w=(-0.3,3)^T using whole
#dataset z and library numDeriv
w <- c(-0.3,3)

erf_fun <- function(w, z = z_obs, n=n_obs) {
  return( empirical_risk_fun(w, z, n) ) 
}

gr <- grad( erf_fun, w )

#Code a gradient descent algorithm with constant learning rate 0.5, 
#that returns chain of all {w^(t)} produced. The termination
#criterion is such that the algorithm stops for total number of 
#iterations exceeding 300. Set seed w^(0)=(-0.3,3)^T
eta <- 0.5
Tmax <- 300
w_seed <- c(-0.3,3)
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
  w <- w - eta * grad( erf_fun, w )
  w_chain <- rbind(w_chain, w)
  # step 2: check for termination terminate
  if ( t>= Tmax ) {
    Qstop <- 1
  }
}

#Plot chain {w_1^(t)} against t
plot(w_chain[,1], type='l')
abline(h=w_true[1], col='red')
#Plot chain {w_2^(t)} against t
plot(w_chain[,2], type='l')
abline(h=w_true[2], col='red')

#Re-run the gradient descent with different values of eta, and 
#different termination criterion if necessary
eta <- 0.5
Tmax <- 300
w_seed <- c(-0.3,3)
w <- w_seed
w_chain <- c()
Qstop <- 0 
t <- 0
grad_descent <- function(eta,Tmax){
  while ( Qstop == 0 ) {
    # counter
    t <- t +  1
    cat( t ) ; cat( ' ' ) ## counter added for display reasons
    # step 1: update  
    erf_fun <- function(w, z = z_obs, n=n_obs) {
      return( empirical_risk_fun(w, z, n) ) 
    }
    w <- w - eta * grad( erf_fun, w )
    w_chain <- rbind(w_chain, w)
    # step 2: check for termination terminate
    if ( t>= Tmax ) {
      Qstop <- 1
    }
  }
  
  #Plot chain {w_1^(t)} against t
  plot(w_chain[,1], type='l')
  abline(h=w_true[1], col='red')
  #Plot chain {w_2^(t)} against t
  plot(w_chain[,2], type='l')
  abline(h=w_true[2], col='red')
}

grad_descent(0.1,1000)

#Re-run GD with learning rate eta-t = t_0/t for different t_0
#Check how the algorithm behaves by plotting {w_1^(t)} and {w_2^(t)}
#against t
learning_rate <- function(t,t0) {
  return(t0/t)
}
t0<- 10
Tmax <- 300
w_seed <- c(-0.3,3.0)
w <- w_seed
w_chain <- c()
Qstop <- 0 
t <- 0
while ( Qstop == 0 ) {
  # counter
  t <- t +  1
  cat( t ) ; cat( ' ' ) ## counter added for display reasons
  # step 1: update  
  eta <- learning_rate( t, t0 )
  erf_fun <- function(w, z = z_obs, n=n_obs) {
    return( empirical_risk_fun(w, z, n) ) 
  }
  w <- w - eta * grad( erf_fun, w )
  #w <- w - eta * grad_risk_fun( w, z_obs, n_obs )
  w_chain <- rbind(w_chain, w)
  # step 2: check for rtermination terminate
  if ( t>= Tmax ) {
    Qstop <- 1
  }
}

plot(w_chain[,1], type='l')
abline(h=w_true[1], col='red')
plot(w_chain[,2], type='l')
abline(h=w_true[2], col='red')


#Batch stochastic gradient descent
set.seed(2023)
n_obs <- 1000000
w_true <- c(0,1)  
z_obs <- data_generating_model(n = n_obs, w = w_true)
w_true <- as.numeric(glm(z_obs[,2]~ 1+ z_obs[,1],family = "binomial" )$coefficients)


m <- 10 #batch size
eta <- 0.5 #learning rate
Tmax <- 300
w_seed <- c(-0.3,3)
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
  #w <- w - eta * grad_risk_fun( w, zbatch, m )
  w_chain <- rbind(w_chain, w)
  # step 2: check for termination terminate
  if ( t>= Tmax ) {
    Qstop <- 1
  }
}

plot(w_chain[,1], type='l') +
  abline(h=w_true[1], col='red')
plot(w_chain[,2], type='l') +
  abline(h=w_true[2], col='red')

#Re-run with different batch sizes and learning rates
m <- 200
eta <- 0.8
Tmax <- 300
w_seed <- c(-0.3,3)
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
  #w <- w - eta * grad_risk_fun( w, zbatch, m )
  w_chain <- rbind(w_chain, w)
  # step 2: check for rtermination terminate
  if ( t>= Tmax ) {
    Qstop <- 1
  }
}

plot(w_chain[,1], type='l') +
  abline(h=w_true[1], col='red')
plot(w_chain[,2], type='l') +
  abline(h=w_true[2], col='red')

#Bigger batch size, smaller variation of gradient, hence smaller error

