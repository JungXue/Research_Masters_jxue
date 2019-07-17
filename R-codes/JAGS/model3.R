# Normal(1,0.1) prior

model3.txt =  "model{

# Likelihood

for(j in 1:Nleaf){
mu[j]        <- rho[j]*lambda[j]
for(i in 1:Nday){
Y[i,j]      ~ dpois(mu[j])
}}for(j in 1:Nleaf){

### Prior

lambda[j] ~ dnorm(mu2.y3[j], sqrt(sigma2.y3[j])) T(0,) 

### Hyper prior

mu2.y3[j] ~ dnorm(1,0.1)   
sigma2.y3[j] ~ dnorm(0.1,0.1)
}}"

