# 0.9 Mixture prior with Normal(1,0.1) prior

model6.txt =  "model{

# Likelihood

for(j in 1:Nleaf){
mu[j]        <- rho[j]*lambda[j]
for(i in 1:Nday){
Y[i,j]      ~ dpois(mu[j])
}}for(j in 1:Nleaf){

### Prior

lambda[j] ~ dnorm(spike[j]*1 + (1 - spike[j])*slab[j], 0.1) T(0,) 

### Hyper prior

spike[j] ~ dbin(0.9,1)
 slab[j] ~ dnorm(mu2.y6[j], sqrt(sigma2.y6[j])) 

### Hyper-hyper prior

   mu2.y6[j] ~ dnorm(1,0.1)  
sigma2.y6 [j]~ dnorm(0.1,0.1) 

}}"

