# laplace(1,1) prior


model5.txt =  "model{

# Likelihood

for(j in 1:Nleaf){

mu[j] <- rho[,j]*lambda
Y[ ,j] ~ dpois(mu[j])

}

### Prior

lambda ~ ddexp(mean.y5,scale.y5) T(0,) 

### Hyper prior

mean.y5 ~ dnorm(1,0.1)
scale.y5 ~ dnorm(1,0.1)

}"

