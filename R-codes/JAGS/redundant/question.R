spike<-rbinom(N,1,0.9)

mu2.y6 = rnorm(N,1,0.1)   
sigma2.y6 = rnorm(N,1,0.1)
slab <- rnorm(N, mu2.y6, sqrt(sigma2.y6))

y6 <- rnorm(N, spike*1+(1-spike)*slab, .1)
y6 <- y6[-which(y6<0)]
y6.df = data.frame(y6)

any(y6<0)
plot(density(y6))
hist(y6)

# 0.5 Mixture prior with Normal(1,0.1) prior


model6.txt =  "model{

# Likelihood

for(j in 1:Nleaf){
mu[j]        <- rho[j]*lambda
for(i in 1:Nday){
Y[i,j]      ~ dpois(mu[j])
}
}

### Prior

lambda ~ dnorm(spike*1 + (1 - spike)*slab, 0.1)  T(0,) 

### Hyper prior

spike ~ dbin(1,0.9)
slab ~ dnorm(mu2.y6, sqrt(sigma2.y6))

### Hyper-hyper prior

mu2.y6 ~ dnorm(1,0.1)  
sigma2.y6 ~ dnorm(0.1,0.1) 


}"


> model6.jags <- jags.model(textConnection(model6.txt), data = jags.data6 ,n.chains=3,n.adapt=1000)
Compiling model graph
Resolving undeclared variables
Allocating nodes
Deleting model

Error in jags.model(textConnection(model6.txt), data = jags.data6, n.chains = 3,  : 
                      RUNTIME ERROR:
                      Failed check for discrete-valued parameters in distribution dbin
