# 0.9 Mixture prior with Normal(1,0.1) prior


model6.txt =  "model{

# Likelihood

for(j in 1:Nleaf){

mu[j] <- rho[,j]*lambda
Y[ ,j] ~ dpois(mu[j])

}

### Prior

lambda ~ dnorm(spike*1 + (1 - spike)*slab, 0.1) T(0,) 

### Hyper prior

spike ~ dbin(0.9,1)
slab ~ dnorm(mu2.y6, sqrt(sigma2.y6)) 

### Hyper-hyper prior

mu2.y6 ~ dnorm(1,0.1)  
sigma2.y6 ~ dnorm(0.1,0.1) 


}"
