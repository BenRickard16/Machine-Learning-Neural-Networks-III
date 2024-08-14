#Part III
set.seed(20)
githubURL <-"https://www.maths.dur.ac.uk/users/georgios.karagiannis/temp1/training.data.3.Rda"
library(numDeriv)
load(url(githubURL))
#Prediction rule h
prediction_rule <- function(x,w) {
  h <- w[1]+w[2]*x
  h <- 1 - exp(-exp(h))
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

#AdaGrad algorithm
z_obs <- as.matrix(as.data.frame(training.data.3))
n_obs <- dim(z_obs)[1]

m <- 5
eta <- 0.5
Tmax <- 500
w_seed <- c(-0.3,3.0)
w <- w_seed
w_chain <- c()
Qstop <- 0 
t <- 0
G <- rep(0.0,times=length(w))
eps <- 10^(-6)
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
  g <- numDeriv::grad( erf_fun, w )
  G <- G + g^2
  w <- w - eta * (1.0/sqrt(G+eps)) * g
  #w <- w - eta * grad_risk_fun( w, zbatch, m )
  w_chain <- rbind(w_chain, w)
  # step 2: check for rtermination terminate
  if ( t>= Tmax ) {
    Qstop <- 1
  }
}

#Plotting chains
plot(w_chain[,1], type='l')
plot(w_chain[,2], type='l')

#Extracting optimal w
w1 <- mean(w_chain[,1][450:500])
w2 <- mean(w_chain[,2][450:500])


