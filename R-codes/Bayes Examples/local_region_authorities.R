# https://stackoverflow.com/questions/32318385/suggested-examples-of-bayesian-hierarchical-modelling-using-three-levels-in-wi
#
#
#
#

library(rjags)

## DATA SIMULATION AND FORMATTING ##################################

n.regions <- 12
n.locations <- c(4,5,6,3,5,5,5,6,10,7,6,2)
n.reps <- 4

overall.mean <- 10
sigma.regions <- 2
sigma.locations <- 5
sigma.reps <- 1
n.obs <- sum(n.locations)*n.reps

region.means <- rnorm(12, 10, sigma.regions)
location.means <- vector()
for(i in 1:12){
  location.means <- c(location.means, rnorm(n.locations[i], region.means[i], sigma.locations))
}

my.data <- data.frame(matrix(data=NA, nrow=n.obs, ncol=3))
names(my.data) = c("region", "location", "observed")
v <- vector()
for(i in 1:12){
  v <- c(v, rep(i, 4*n.locations[i]))
}
my.data$region <- v
my.data$location <- floor((0:(n.obs-1))/n.reps)+1
for(k in 1:n.obs){
  my.data$observed[k] <- rnorm(1, location.means[my.data$location[k]], sigma.reps)
}
locations.regions <- my.data[4*(1:64), ]

head(locations.regions)
str(locations.regions)
## JAGS CODE #################################################

sink("mymodel.txt")
cat("model{
    
    # Priors
    
    mu ~ dnorm(0, .001)
    ## Overall mean  
    
    sigma.regions ~ dunif(0,20)
    tau.regions <- 1/(sigma.regions*sigma.regions)
    sigma.locations ~ dunif(0,20)
    tau.locations <- 1/(sigma.locations*sigma.locations)
    sigma.reps ~ dunif(0,20)
    tau.reps <- 1/(sigma.reps*sigma.reps)
    
    
    # Model structure
    for(i in 1:n.regions){
    mu.region[i] ~ dnorm(mu, tau.regions)
    }
    for(i in 1:n.locations){
    mu.location[i] ~ dnorm(mu.region[region[i]], tau.locations)
    }
    for(i in 1:n.obs){
    observed[i] ~ dnorm(mu.location[location[i]], tau.reps)
    }
    }
    ", fill=TRUE)
sink()

## RJAGS CODE TO CALL JAGS AND RUN THE MODEL: ##################################

jags.data <- list(observed=my.data$observed, region=locations.regions$region, location=my.data$location, n.obs=n.obs, n.regions=12, n.locations=sum(n.locations))
inits <- function(){list(mu=rnorm(1,0,10), sigma.regions=runif(1,0,10),
                         sigma.locations=runif(1,0,10), sigma.reps=runif(1,0,10))}

params <- c("mu", "mu.region", "mu.location", "sigma.regions", "sigma.locations", "sigma.reps")

nc <- 5
n.adapt <-1000
n.burn <- 2000
n.iter <- 5000
thin <- 10
my.model <- jags.model('mymodel.txt', data = jags.data, inits=inits, n.chains=nc, n.adapt=n.adapt)
update(my.model, n.burn)
my.model_samples <- coda.samples(my.model,params,n.iter=n.iter, thin=thin)
summary(my.model_samples)