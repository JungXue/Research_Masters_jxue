# Gamma(4,3) prior

model4.txt =  "model{

# Likelihood

for(j in 1:Nleaf){
mu[j]        <- rho[j]*lambda[j]
for(i in 1:Nday){
Y[i,j]      ~ dpois(mu[j])
}}for(j in 1:Nleaf){

### Prior

lambda[j] ~ dgamma(alpha.y4[j],beta.y4[j]) T(0,) 

### Hyper prior

alpha.y4[j] ~ dnorm(4,0.1)  
 beta.y4[j] ~ dnorm(3,0.1)
}}"
