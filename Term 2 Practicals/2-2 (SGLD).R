install.packages("mvtnorm")
library(numDeriv)
library(mvtnorm)

set.seed(2023)
#Binary classification problem input x real and output y = +_1
#Consider available dataset S_n = {z_i = (y_i, x_i)} from i=1 to n
#Dataset is generated from g() which we pretend not to know
data_generating_model <- function(n,w) {
  d <- 3
  z <- rep( NaN, times=n*d )
  z <- matrix(z, nrow = n, ncol = d)
  z[,1] <- 1.0
  z[,2] <- runif(n, min = -10, max = 10)
  p <- w[1]*z[,1] + w[2]*z[,2] 
  p <- exp(p) / (1+exp(p))
  z[,3] <- rbinom(n, size = 1, prob = p)
  return(z)
}

#Assume n=10^6 and true value of w = (0, 1)
#Dataset containing training examples for the model are generated 
#below, and stored in the array zobs
n_obs <- 10^(6)
w_true <- c(0,1)  
set.seed(2023)
z_obs <- data_generating_model(n = n_obs, w = w_true) 
set.seed(0)
w_true <- as.numeric(glm(z_obs[,3]~ 1 + z_obs[,2],family = "binomial" )$coefficients)

#Function returning prediction rule h given input x and w unknown parameter
prediction_rule <- function(x,w) {
  h <- w[1]*x[1]+w[2]*x[2]
  h <- exp(h) / (1.0 + exp(h) )
  return (h)
}

#We consider d=2, V=100I_2, and  μ=0
#Function for log pdf of the sampling distribution
log_sampling_pdf <- function(z, w) {
  d <- length(w)
  x <- z[1:d] 
  y <- z[d+1]
  log_pdf <- y * log(prediction_rule(x,w)) +(1-y) * log( 1.0-prediction_rule(x,w) )
  #log_pdf <- dbinom(y, size = 1, prob = prediction_rule(x,w), log = TRUE)
  return( log_pdf )
}

#Function for log pdf of the prior distribution of w
log_prior_pdf <- function(w, mu, Sig2 ) {
  log_pdf <- dmvnorm(w, mean = mu, sigma = Sig2, log = TRUE, checkSymmetry = TRUE)
  return( log_pdf )
}

#Consider a learning rate function
learning_rate <- function(t, T_0 = 100, T_1 = 500, C_0 = 0.0001, s_0 = 0.5 ) {
  if ( t <= T_0 ) {
    eta <- C_0
  } else if ( (T_0+1 <= t) && (t <= T_1 ) ) {
    eta <- C_0 / ( (t-T_0) ^ s_0 )
  } else {
    eta <- C_0 / ( (T_1-T_0) ^ s_0 )
  }
  return(eta)
}

#SGLD algorithm batch size m=0.1n, temperature tau=1
#Learning rate constant around C_0=10^-6 for 1/3 of the run, then
#delay ς=0.51, final 1/3 constant to a small value
#T_max=500 and seed (-1,0)
Tmax <- 500
#
w_seed <- c(-1,0)
#
eta <- 10^(-6)
eta_C <- eta
eta_s <- 0.51
eta_T0 <- 0.3*Tmax
eta_T1 <- 0.6*Tmax
#
batch_size <- 1000
#
tau <- 1.0
#
# Set the seed
w <- w_seed
w_chain <- c(w)
# iterate
t <- 1
Qterm <- 0
#
# iterate
#
while ( (Qterm != 1) ) {
  # counter 
  t <- t+1
  cat( t ) ; cat( ' ' ) ## counter added for display reasons
  # learning rate
  eta <- learning_rate(t, eta_T0, eta_T1, eta_C, eta_s)
  # sub-sample
  J <- sample.int( n = n_obs, size = batch_size, replace = FALSE)
  # update
  w_new <- w
  ## likelihood
  grad_est_lik <- rep( 0.0, times=length(w) )
  for (j in J) {
    aux_fun <- function(w, z=z_obs[j,]){
      gr <- log_sampling_pdf(z, w)
      return(gr)
    }
    grad_est_lik <- grad_est_lik + numDeriv::grad(aux_fun, w)
  }
  grad_est_lik <- ( n_obs / batch_size) * grad_est_lik
  w_new <- w_new +eta*grad_est_lik ; 
  ## prior
  aux_fun <- function(w){
    d <- length(w)
    gr <- log_prior_pdf(w, rep(0,d), 100*diag(d))
    return(gr)
  }
  w_new <- w_new +eta*numDeriv::grad(aux_fun, w) ;
  ## noise
  w_new <- w_new +sqrt(2.0)*sqrt(eta)*sqrt(tau)*rnorm(n = length(w), mean = 0, sd = 1)
  # record
  w <- w_new
  # termination criterion
  if  ( t >= Tmax ) {
    Qterm <- 1
  }
  # record the produced chain
  w_chain <- rbind(w_chain,w)
}

plot(w_chain[,1], type='l') +
  abline(h=w_true[1], col='red')
plot(w_chain[,2], type='l') +
  abline(h=w_true[2], col='red')

#Now C_0=10^-2 and apply clipping with threshold C=10
Tmax <- 500
#
w_seed <- c(-1,0)
#
eta <- 10^(-2)
eta_C <- eta
eta_s <- 0.51
eta_T0 <- 0.3*Tmax
eta_T1 <- 0.6*Tmax
#
batch_size <- 1000
#
tau <- 1.0
#
# Set the seed
w <- w_seed
w_chain_clipping <- c(w)
# iterate
t <- 1
Qterm <- 0
#
clipping_threshold <- 10
#
# iterate
#
while ( (Qterm != 1) ) {
  # counter 
  t <- t+1
  cat( t ) ; cat( ' ' ) ## counter added for display reasons
  # learning rate
  eta <- learning_rate(t, eta_T0, eta_T1, eta_C, eta_s)
  # sub-sample
  J <- sample.int( n = n_obs, size = batch_size, replace = FALSE)
  # update
  w_new <- w
  ## likelihood
  grad_est_lik <- rep( 0.0, times=length(w) )
  for (j in J) {
    aux_fun <- function(w, z=z_obs[j,]){
      gr <- log_sampling_pdf(z, w)
      return(gr)
    }
    grad_est_lik <- grad_est_lik + numDeriv::grad(aux_fun, w)
  }
  grad_est_lik <- ( n_obs / batch_size) * grad_est_lik
  # gradient clipping/rescaring
  norm_grad_est_lik <- sqrt(sum(grad_est_lik^2))
  grad_est_lik <- grad_est_lik * min( 1.0, clipping_threshold/norm_grad_est_lik )
  w_new <- w_new +eta*grad_est_lik ; 
  ## prior
  aux_fun <- function(w){
    d <- length(w)
    gr <- log_prior_pdf(w, rep(0,d), 100*diag(d))
    return(gr)
  }
  w_new <- w_new +eta*numDeriv::grad(aux_fun, w) ;
  ## noise
  w_new <- w_new +sqrt(2.0)*sqrt(eta)*sqrt(tau)*rnorm(n = length(w), mean = 0, sd = 1)
  # record 
  w <- w_new
  # termination criterion
  if  ( t >= Tmax ) {
    Qterm <- 1
  }
  # record the produced chain
  w_chain_clipping <- rbind(w_chain_clipping,w)
}
plot(w_chain[,1], type='l') +
  abline(h=w_true[1], col='red')
plot(w_chain[,2], type='l') +
  abline(h=w_true[2], col='red')

#Back to first code, remove burn in values
T_bunrin <- as.integer(0.6*Tmax)
w_chain_output <- w_chain[T_bunrin:Tmax,]

#Plot the histograms plots of output chains {w_1}and {w_2} for
#the estimation of the marginal posterior distributions of the 
#dimensions of  w
par(mfrow=c(1,2))
hist(w_chain_output[,1])
hist(w_chain_output[,2]) 

#Estimate of expectation of w_1 + w_2
w_est = mean(rowSums(w_chain_output))
w_est

#Point estimate for new value x=(1,0.5)
x_new <- c(1,0.5)
T <- dim(w_chain_output)[1]
h_est <- 0.0 
for (t in 1:T) {
  h_est <- h_est + prediction_rule( x_new , w_chain_output[t,] )
}
h_est <- h_est / T
h_est

x_new <- c(1,0.5)
w_est <- colMeans(w_chain_output)
h_est <- prediction_rule( x_new , w_est )
h_est

#Estimate the pdf of the prediction rule at x=(1,0.5)
x_new <- c(1,0.5)
T <- dim(w_chain_output)[1]
h_chain <- c()
for (t in 1:T) {
  h_chain <- c( h_chain , 
                prediction_rule( x_new , w_chain_output[t,] ) 
  )
}
hist(h_chain)


#Non-Convex problem with two modes in the posterior distribution
data_generating_model <- function(n,w) {
  z <- rep( NaN, times=n )
  p1 <- 0.5 
  p2 <- 1.0-p1
  w <- 5
  phi <-  20
  sig2 <- 5
  lab <- as.numeric(runif(n_obs)>p1)
  z <- lab*rnorm(n, mean = w, sd = sqrt(sig2)) + (1-lab)*rnorm(n, mean = phi-w, sd = sqrt(sig2))
  return(z)
}

#Dataset containing the examples to train the model are generated
#below, and stored in the array zobs
n_obs <- 10^(6)
w_true <- 5 
set.seed(2023)
z_obs <- data_generating_model(n = n_obs, w = w_true) 
set.seed(0)
hist(z_obs)

#PDF of sampling distribution in log scale for a single example z
log_sampling_pdf <- function(z, w, p1 = 0.5, phi=20, sig2 = 5) {
  log_sampling_pdf <- p1*dnorm(z, mean = w, sd = sqrt(sig2), log = FALSE)
  log_sampling_pdf <- log_sampling_pdf + (1-p1)*dnorm(z, mean = phi-w, sd = sqrt(sig2), log = FALSE)
  log_sampling_pdf <- log(log_sampling_pdf) ;
  return(log_sampling_pdf)
}

#Log PDF of the prior distribution of w
log_prior_pdf <- function(w, mu = 0.0, sig2 = 100 ) {
  log_pdf <- dnorm(w, mean = mu, sd = sqrt(sig2), log = TRUE)
  return( log_pdf )
}

#Learning rate
learning_rate <- function(t, T_0  , T_1  , C_0  , s_0   ) {
  if ( t <= T_0 ) {
    eta <- C_0
  } else if ( (T_0+1 <= t) && (t <= T_1 ) ) {
    eta <- C_0 / ( (t-T_0) ^ s_0 )
  } else {
    eta <- C_0 / ( (T_1-T_0) ^ s_0 )
  }
  return(eta)
}

#SGLD algorithm to find one of the modes
Tmax <- 500
#
w_seed <- 0.0
#
eta <- 10^(-2)
eta_C <- eta
eta_s <- 0.51
eta_T0 <- 0.3*Tmax
eta_T1 <- 0.6*Tmax
#
batch_size <- 1000
#
tau <- 1.0
#
# Set the seed
w <- w_seed
w_chain_clipping <- c(w)
# iterate
t <- 1
Qterm <- 0
#
clipping_threshold <- 10
#
# iterate
#
while ( (Qterm != 1) ) {
  # counter 
  t <- t+1
  cat( t ) ; cat( ' ' ) ## counter added for display reasons
  # learning rate
  eta <- learning_rate(t, eta_T0, eta_T1, eta_C, eta_s)
  # sub-sample
  J <- sample.int( n = n_obs, size = batch_size, replace = FALSE)
  # update
  w_new <- w
  ## likelihood
  grad_est_lik <- rep( 0.0, times=length(w) )
  for (j in J) {
    aux_fun <- function(w, z=z_obs[j]){
      gr <- log_sampling_pdf(z, w)
      return(gr)
    }
    grad_est_lik <- grad_est_lik + numDeriv::grad(aux_fun, w)
  }
  grad_est_lik <- ( n_obs / batch_size) * grad_est_lik
  # gradient clipping/rescaring
  norm_grad_est_lik <- sqrt(sum(grad_est_lik^2))
  grad_est_lik <- grad_est_lik * min( 1.0, clipping_threshold/norm_grad_est_lik )
  w_new <- w_new +eta*grad_est_lik ; 
  ## prior
  aux_fun <- function(w){
    d <- length(w)
    gr <- log_prior_pdf(w, rep(0,d), 100*diag(d))
    return(gr)
  }
  w_new <- w_new +eta*numDeriv::grad(aux_fun, w) ;
  ## noise
  w_new <- w_new +sqrt(2.0)*sqrt(eta)*sqrt(tau)*rnorm(n = length(w), mean = 0, sd = 1)
  # record
  w <- w_new
  # termination criterion
  if  ( t >= Tmax ) {
    Qterm <- 1
  }
  # record the produced chain
  w_chain_clipping <- rbind(w_chain_clipping,w)
}

plot(w_chain_clipping, type = 'l')
hist(w_chain_clipping)

#Get trapped at first mode and unable to travel across area of low
#density mass to other mode