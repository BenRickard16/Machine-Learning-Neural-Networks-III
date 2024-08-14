library(numDeriv)
library(nloptr)

set.seed(2023)

#Dataset S_n generated from data generation probability g()
data_generating_model <- function(n,w) {
  z <- rep( NaN, times=n*2 )
  z <- matrix(z, nrow = n, ncol = 2)
  z[,1] <- runif(n, min = -10, max = 10)
  p <- w[1] + w[2]*z[,1] 
  p <- exp(p) / (1+exp(p))
  z[,2] <- rbinom(n, size = 1, prob = p)
  return(z)
}


#n=500, real values for unkown w = (0,1)
n_obs <- 500
w_true <- c(0,1)  
z_obs <- data_generating_model(n = n_obs, w = w_true) 
w_true <- as.numeric(glm(z_obs[,2]~ 1+ z_obs[,1],family = "binomial" )$coefficients)

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

#Empirical Risk Function
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

#Learning rate function eta_t = t/t_0
learning_rate <-function(t,t0=3) {
  eta <- t0 / t
  return( eta )
}

#Function returns gradient of loss function at values w,z
grad_loss_fun <- function(w,z) {
  x = z[1]
  y = z[2]
  h <- prediction_rule(x,w)
  grd <- c(h-y, (h-y)*x)
  return (grd)
}

#Fct returns gradient of risk function at values w and using
#dataset z size nx2
grad_risk_fun <- function(w,z,n) {
  grd <- 0.0
  for (i in 1:n) {
    grd <- grd + grad_loss_fun(w,z[i,])
  }
  grd <- grd / n
  return (grd)
}

#Stochastic Variance Reduced Gradient
set.seed(2023)
n_obs <- 100000
w_true <- c(0,1)  
z_obs <- data_generating_model(n = n_obs, w = w_true)
w_true <- as.numeric(glm(z_obs[,2]~ 1+ z_obs[,1],family = "binomial" )$coefficients)

#SVRG algorithm eta_t = 0.5, batch size m=1 (online), kappa=100
#controlling number of snapshots, T=500, seed (-0.3,3)
m <- 1
eta <- 0.5
Tmax <- 500
kappa <- 100
Qstop <- 0 
t <- 0
#
#seeds
w_seed <- c(-0.3,3.0)
w <- w_seed
w_chain <- c()
cv_w <- w  
erf_fun <- function(w, z = z_obs, n=n_obs) {
  return( empirical_risk_fun(w, z, n) ) 
}
cv_grad_risk <- numDeriv::grad( erf_fun, cv_w ) #control variate
#
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
  w <- w - eta * (numDeriv::grad( erf_fun, w ) -numDeriv::grad( erf_fun, cv_w ) +cv_grad_risk )
  #w <- w - eta * grad_risk_fun( w, zbatch, m )
  # record
  w_chain <- rbind(w_chain, w)
  # control variate step
  if ( (t %% kappa) == 0) {
    cv_w <- w  
    erf_fun <- function(w, z = z_obs, n=n_obs) {
      return( empirical_risk_fun(w, z, n) ) 
    }
    cv_grad_risk <- numDeriv::grad( erf_fun, cv_w ) #controle variate
  }
  # step 2: check for termination 
  if ( t>= Tmax ) {
    Qstop <- 1
  }
}

par(mfrow=c(1,2))
plot(w_chain[,1], type='l') 
+ abline(h=w_true[1], col='red')

plot(w_chain[,2], type='l') 
+abline(h=w_true[2], col='red')


#Changing kappa
m <- 1
eta <- 0.5
Tmax <- 500
kappa <- 10
Qstop <- 0 
t <- 0
#
#seeds
w_seed <- c(-0.3,3.0)
w <- w_seed
w_chain <- c()
cv_w <- w  
erf_fun <- function(w, z = z_obs, n=n_obs) {
  return( empirical_risk_fun(w, z, n) ) 
}
cv_grad_risk <- numDeriv::grad( erf_fun, cv_w ) #control variate
#
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
  w <- w - eta * (numDeriv::grad( erf_fun, w ) -numDeriv::grad( erf_fun, cv_w ) +cv_grad_risk )
  #w <- w - eta * grad_risk_fun( w, zbatch, m )
  # record
  w_chain <- rbind(w_chain, w)
  # control variate step
  if ( (t %% kappa) == 0) {
    cv_w <- w  
    erf_fun <- function(w, z = z_obs, n=n_obs) {
      return( empirical_risk_fun(w, z, n) ) 
    }
    cv_grad_risk <- numDeriv::grad( erf_fun, cv_w ) #controle variate
  }
  # step 2: check for termination 
  if ( t>= Tmax ) {
    Qstop <- 1
  }
}

plot(w_chain[,1], type='l') 
+ abline(h=w_true[1], col='red')

plot(w_chain[,2], type='l') 
+abline(h=w_true[2], col='red')

#reducing κ, increases the number of snapshots, reduces the
#variance in the gradients, reduces the variance of the trace, 
#and hence aims at making the upper bound of the error smaller