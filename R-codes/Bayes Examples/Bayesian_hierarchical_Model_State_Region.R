# load rjags for modeling
library(rjags)

# data
Nregion <- nlevels(state.region)
Nstate <- length(state.abb)
Nqtr <- 20
N <- Nstate * Nqtr # number of obs
y <- runif(N, 4500, 5500) # regresand
x1 <- dplyr::lag(y); x1[is.na(x1)] <- 4500 # lag of y as covaritate
x2 <- rnorm(N, 100, Nqtr) # second covariates
a1 <- rep(seq(1, 4), Nstate * 5) #qtr of the year
s <- as.factor(rep(state.abb, Nqtr)) # state
r <- as.factor(rep(state.region, Nqtr)) # region

# model
modelstring = "
  model {
for ( i in 1:N ) {
y[i] ~ dnorm(y.hat[i], tau.i)
y.hat[i] <- b0[s[i]] + b1[s[i]] * x1[i] + b2[s[i]] * x2[i] + b3[s[i], a1[i]]
}

for ( s in 1:Nstate ) {
b0[s] ~ dnorm(c0[r[s]], b0t)
b1[s] ~ dnorm(c1[r[s]], b1t)
b2[s] ~ dnorm(c2[r[s]], b2t)

for ( j in 1:Nqtr ) {
b3[s, j] ~ dnorm(0, tau.sq.alpha)
}

}

for ( r in 1:Nregion ) {
c0[r] ~ dnorm(d0, c0t)
c1[r] ~ dnorm(d1, c1t)
c2[r] ~ dnorm(d2, c2t)
}

b0t ~ dgamma(0.001, 0.001)
b1t ~ dgamma(0.001, 0.001)
b2t ~ dgamma(0.001, 0.001)
c0t ~ dgamma(0.001, 0.001)
c1t ~ dgamma(0.001, 0.001)
c2t ~ dgamma(0.001, 0.001)
d0 ~ dnorm(0, .0001)
d1 ~ dnorm(0, .0001)
d2 ~ dnorm(0, .0001)
tau.i ~ dgamma(0.001, 0.001)
tau.sq.alpha ~ dgamma(0.001, 0.001)
sigma.sq.alpha <- 1 / tau.sq.alpha

}
"
# write to file
writeLines(modelstring, con="model.txt")

# create jags model object
jags <- jags.model('model.txt',
                   data = list('x1' = x1,
                               'x2' = x2,
                               'a1' = a1,
                               'y' = y,
                               'N' = N,
                               's' = s,
                               'Nstate' = Nstate,
                               'Nqtr' = Nqtr),
                   n.chains = 4,
                   n.adapt = 100)