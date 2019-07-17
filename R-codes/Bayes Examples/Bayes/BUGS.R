######################################################################

## http://www.r-tutor.com/bayesian-statistics/openbugs


model <- function() { 
  # Prior 
  p ~ dbeta(1, 1) 
  
  # Likelihood 
  y ~ dbin(p, N) 
} 

##transfer the model to OpenBUGS
install.packages("R2OpenBUGS", dependencies = FALSE)
library(R2OpenBUGS) 

model.file <- file.path(tempdir(), "model.txt") 
write.model(model, model.file) 

###parameters

library(MASS) 
tbl <- table(survey$Smoke) 
N <- as.numeric(sum(tbl)); N 

y <- N - as.numeric(tbl["Never"]); y 

##identify data variables in a list called data. 

data <- list("N", "y") 

## identify the variable p to be monitored in a vector called params. 

params <- c("p") 

## select some initial parameters for the simulation. 

inits <- function() { list(p=0.5) } 

## invoke OpenBUGS with the namesake method bugs and save the result in a variable out.

out <- bugs(data, inits, params, model.file, n.iter=10000) 

##check the Rhat component of the output. 

all(out$summary[,"Rhat"] < 1.1) 

## retrieve the posterior mean and standard deviation of p from the output. 

out$mean["p"] 
out$sd["p"] 

## the simulation result 

print(out, digits=5) 

########################################

##MCMC convergence
out <- bugs(data, inits, params, model.file, codaPkg=TRUE, n.iter=10000) 
out.coda <- read.bugs(out) 

library(coda) 

xyplot(out.coda) 

densityplot(out.coda)
###############################################################################################

acfplot(out.coda) 