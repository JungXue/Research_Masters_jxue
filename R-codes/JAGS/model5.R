# laplace(1,1) prior

model5.txt =  "model{

# Likelihood

for(j in 1:Nleaf){
mu[j]        <- rho[j]*lambda[j]
for(i in 1:Nday){
Y[i,j]      ~ dpois(mu[j])
}}for(j in 1:Nleaf){

### Prior

lambda[j] ~ ddexp(mean.y5[j],scale.y5[j]) T(0,) 

### Hyper prior

 mean.y5[j] ~ dnorm(1,0.1)
scale.y5[j] ~ dnorm(1,0.1)
}}"

