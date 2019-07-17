# Normal(1,0.1) prior


model3.txt =  "model{

# Likelihood

for(j in 1:Nleaf){

mu[j] <- rho[,j]*lambda
Y[ ,j] ~ dpois(mu[j])

}

### Prior

lambda ~ dnorm(mu2.y3, sqrt(sigma2.y3)) T(0,) 

### Hyper prior

mu2.y3 ~ dnorm(1,0.1)   
sigma2.y3 ~ dnorm(0.1,0.1)

}"