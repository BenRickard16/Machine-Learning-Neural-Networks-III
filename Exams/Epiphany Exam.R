myURL <-"https://www.maths.dur.ac.uk/users/georgios.karagiannis/temp1/training.data.2.Rda"

load(url(myURL))
library(numDeriv)

#Prediction rule h
prediction_rule <- function(x,w) {
  h <- w[1]+w[2]*x
  return (h)
}

#Loss function
loss_fun <- function(w,z) {
  x = z[1]
  y = z[2]
  h <- prediction_rule(x,w)
  ell <- (y-h) + 2*log(1 + exp(h-y))
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

#Gradient descent algorithm
z_obs <- as.matrix(as.data.frame(training.data.2))
n_obs <- dim(z_obs)[1]

eta <- 0.5
Tmax <- 100
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

#Plot the converging chains
par(mfrow=c(1,2))
plot(w_chain[,1], type='l') 
plot(w_chain[,2], type='l') 

#Extracting optimal values
w1 <- mean(w_chain[,1][95:100])
w2 <- mean(w_chain[,2][95:100])

#Prediction
w1 + 0.1*w2


#Part III
githubURL <-"https://www.maths.dur.ac.uk/users/georgios.karagiannis/temp1/training.data.3.Rda"

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

m <- 10
eta <- 2
Tmax <- 500
w_seed <- c(0.5,2)
w <- w_seed
w_chain <- c()
Qstop <- 0 
t <- 0
G <- rep(0.0,times=length(w))
eps <- 10^(-2)
while ( Qstop == 0 ) {
  # counter
  t <- t +  1
  cat( t ) ; cat( ' ' ) ## counter added for display reasons
  # step 1: update  
  J <- sample.int(n = n_obs, size = m, replace = TRUE)
  zbatch <- z_obs[J,]
  #eta <- learning_rate( t )
  erf_fun <- function(w, z = zbatch, n=m) {
    return( empirical_risk_fun(w, z, n) ) 
  }
  g <- grad( erf_fun, w )
  G <- G + g^2
  w <- w - eta * (1.0/sqrt(G+eps)) * g
  #w <- w - eta * grad_risk_fun( w, zbatch, m )
  w_chain <- rbind(w_chain, w)
  # step 2: check for rtermination terminate
  if ( t>= Tmax ) {
    Qstop <- 1
  }
}


#Part Iv
githubURL <-"https://www.maths.dur.ac.uk/users/georgios.karagiannis/temp1/training.data.4.Rda" 

load(url(githubURL))
library(e1071)

training.data.4 <-as.data.frame(training.data.4)


tune.out <- tune(svm, factor(y) ~ ., data = training.data.4, 
                 kernel = "radial", 
                 ranges = list(cost = c(0.01,0.1,1,5,6,7,10,15,20),
                   gamma = c(0.01,0.5,5,10)))
summary(tune.out)

tune.out$best.model

svmfit <- svm( factor(y) ~ ., data = training.data.4, scale = FALSE, 
               kernel = "radial", 
               gamma = 1/2,
               cost = 0.1)
plot(svmfit, training.data.4)



#Part V
githubURL <-"https://www.maths.dur.ac.uk/users/georgios.karagiannis/temp1/training.data.5.Rda"
load(url(githubURL))

library(nnet)

githubURL <-"https://www.maths.dur.ac.uk/users/georgios.karagiannis/temp1/validation.data.5.Rda"
load(url(githubURL))
x_valid <- training.data.5[,-1]
y_valid <- training.data.5$V1

Nrealizations <- 100

nnet.out.best <- nnet(V1 ~., size=15, decay=0.001, data=training.data.5)

nnet.out.pred <- predict(nnet.out.best, x_valid)
best.match.best <- mean(  round(nnet.out.pred) == y_valid )

for (r in 1:Nrealizations) {
  set.seed( r )
  nnet.out.new <- nnet(V1 ~., size=15, decay=0.001, data=training.data.5)
  
  nnet.out.pred <- predict(nnet.out.new, x_valid)
  best.match.new <- mean(  round(nnet.out.pred) == y_valid )
  
  if (best.match.best < best.match.new) {
    nnet.out.best <- nnet.out.new
    best.match.best <- best.match.new
  }
}  


best.match.best

nnet.out.best

githubURL <-"https://www.maths.dur.ac.uk/users/georgios.karagiannis/temp1/future.data.5.Rda"

load(url(githubURL))

predict(nnet.out.best, future.data.5)
