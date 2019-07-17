# Normal(1,0.3) prior

model2h.txt =  "model{

# Likelihood

### Level 0

   mu2.y2.lv0 ~ dnorm(1,0.1) 
sigma2.y2.lv0 ~ dnorm(1,0.1) 
      lambda1 ~ dnorm(mu2.y2.lv0, sigma2.y2.lv0) T(0,)

### level 1

for(l in 1:Nlv1){
   mu2.y2.lv1[l] ~ dnorm(lambda1,0.1) 
sigma2.y2.lv1[l] ~ dnorm(1,0.1) 
      lambda2[l] ~ dnorm(mu2.y2.lv1[l], sigma2.y2.lv1[l]) T(0,) 
}

### Level 2

for(m in 1:Nlv2){
   mu2.y2.lv2[m] ~ dnorm(lambda2[lV1b[m]],0.1) 
sigma2.y2.lv2[m] ~ dnorm(1,0.1) 
      lambda3[m] ~ dnorm(mu2.y2.lv2[m], sigma2.y2.lv2[m]) T(0,) 
}

### Prior*likelihood
lambda      <- c(lambda1,lambda2,lambda3)

for(j in 1:Nleaf){
mu[j]       <- rho[j]*lambda[j]
for(i in 1:Nday){
Y[i,j]      ~ dpois(mu[j])
}}

}"

