# Gamma(4,3) prior


model4.txt =  "model{

# Likelihood

for(j in 1:Nleaf){

mu[j] <- rho[,j]*lambda
Y[ ,j] ~ dpois(mu[j])

}


### Prior

lambda ~ dgamma(alpha.y4,beta.y4) T(0,) 

### Hyper prior

alpha.y4 ~ dnorm(4,0.1)  
 beta.y4 ~ dnorm(3,0.1)

}"