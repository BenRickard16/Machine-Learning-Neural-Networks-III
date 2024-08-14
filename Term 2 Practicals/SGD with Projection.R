library(numDeriv)
library(nloptr)

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

#n=500, real values for unkown w = (0,1) learning data
#stored in z_obs
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

#Gradient of ERF at w =(-0.1, 1.5) using whole dataset
w <- c(-0.1, 1.5)
gr <- grad_risk_fun(w, z=z_obs, n=n_obs)

#Dataset Sn size n=1000000 and wtrue=(0.0,1.0)
#The dataset containing the examples to train the model
#are generated below, and stores in the array z_obs
#Hypothesis class is restricted s.t. |w|_2 <_ 2
set.seed(2023)
n_obs <- 1000000
w_true <- c(0,1)  
z_obs <- data_generating_model(n = n_obs, w = w_true)
w_true <- as.numeric(glm(z_obs[,2]~ 1+ z_obs[,1],family = "binomial" )$coefficients)

#SGD with Projection
boundary <- 2.0 # this is the value |w|_{2}^{2} <= boundary
# auxiliary functions to compute the projection
eval_f0 <- function( w_proj, w_now ){ 
  return( sqrt(sum((w_proj-w_now)^2)) )
}
eval_grad_f0 <- function( w, w_now ){ 
  return( c( 2*(w[1]-w_now[1]), 2*(w[2]-w_now[2]) ) )
}
eval_g0 <- function( w_proj, w_now) {
  return( sum(w_proj^2) -(boundary)^2 )
}
eval_jac_g0 <- function( w, w_now ) {
  return(   c(2*w[1],2*w[2] )  )
}

w <- c(-0.1,0.3)

out <- nloptr(x0=c(0.0,0.0),
              eval_f=eval_f0,
              eval_grad_f=eval_grad_f0,
              eval_g_ineq = eval_g0,
              eval_jac_g_ineq = eval_jac_g0, 
              w_now=w,
              opts = list("algorithm" = "NLOPT_LD_MMA",
                          "xtol_rel"=1.0e-8) 
)
out$solution

#SGD algorithm learning rate eta+0.1, batch size m=1, 
#projection to H={w:|w|<_2} returns chain {w}^t, T=1000,
#seed=(-0.3,0.3)
boundary <- 2.0 # this is the value |w|_{2}^{2} <= boundary
# auxiliary functions to compute the projection
eval_f0 <- function( w_proj, w_now ){ 
  return( sqrt(sum((w_proj-w_now)^2)) )
}
eval_grad_f0 <- function( w, w_now ){ 
  return( c( 2*(w[1]-w_now[1]), 2*(w[2]-w_now[2]) ) )
}
eval_g0 <- function( w_proj, w_now) {
  return( sum(w_proj^2) -(boundary)^2 )
}
eval_jac_g0 <- function( w, w_now ) {
  return(   c(2*w[1],2*w[2] )  )
}

m <- 1
eta <- 0.1
Tmax <- 1000
w_seed <- c(-0.3,3.0)

w <- w_seed
w_chain <- c(w_seed)
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
  } else {
    zbatch <- z_obs[J,]
  }
  #eta <- learning_rate( t )
  erf_fun <- function(w, z = zbatch, n=m) {
    return( empirical_risk_fun(w, z, n) ) 
  }
  w <- w - eta * numDeriv::grad( erf_fun, w )
  #w <- w - eta * grad_risk_fun( w, zbatch, m )
  # step 1.5 projection
  out <- nloptr(x0=c(0.0,0.0),
                eval_f=eval_f0,
                eval_grad_f=eval_grad_f0,
                eval_g_ineq = eval_g0,
                eval_jac_g_ineq = eval_jac_g0, 
                w_now=w,
                opts = list("algorithm" = "NLOPT_LD_MMA",
                            "xtol_rel"=1.0e-8),
  )
  w <- out$solution
  # record
  w_chain <- rbind(w_chain, w)
  # step 2: check for rtermination terminate
  if ( t>= Tmax ) {
    Qstop <- 1
  }
}

par(mfrow=c(1,2))
plot(w_chain[,1], type='l') +
  abline(h=w_true[1], col='red')
plot(w_chain[,2], type='l') +
  abline(h=w_true[2], col='red')
