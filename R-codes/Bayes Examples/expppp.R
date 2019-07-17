dat   <- read.csv("http://www4.stat.ncsu.edu/~reich/ST590/code/ConcussionsByTeamAndYear.csv")
team  <- dat[,1]
Y2012 <- dat[,2]
Y2013 <- dat[,3]
Y     <- Y2012+Y2013
n     <- length(Y)
N     <- 16*2

model_string1 <- "model{

  # Likelihood
  for(i in 1:n){
    Y[i]      ~ dpois(mu[i])
    mu[i]     <- N*lambda[i]
    lambda[i] ~ dgamma(0.1,0.1)
  }
 }"

# (2) Same rate for all teams
model_string2 <- "model{

  # Likelihood
  for(i in 1:n){
    Y[i] ~ dpois(mu)
  }
  mu     <- N*lambda
  lambda ~ dgamma(0.1,0.1)
 }"

library(rjags)

# DIC requires more than one chain
model1 <- jags.model(textConnection(model_string1), 
                     data = list(Y=Y,n=n,N=N),n.chains=3)

update(model1, 10000, progress.bar="none")

# Use dic.samples instead of coda.samples to get DIC.
dic1  <- dic.samples(model1, 
                     variable.names=c("lambda"), 
                     n.iter=20000, progress.bar="none")


model2 <- jags.model(textConnection(model_string2), 
                     data = list(Y=Y,n=n,N=N),n.chains=3)

update(model2, 10000, progress.bar="none")
dic2  <- dic.samples(model2, 
                     variable.names=c("lambda"), 
                     n.iter=20000, progress.bar="none")

dic1
dic2