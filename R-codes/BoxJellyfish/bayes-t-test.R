### Difference from Bayesian T-test ---------------------#

#### Assign values 

control_list      = list()
treatment_list    = list()
pooled_list       = list()
fold_changes_list = list()

for (i in 1:nGene){
  control_list     [[i]] =   genedata$Control       [location[i]:(location[i]+nnRNA[i]-1)]
  treatment_list   [[i]] =   genedata$Treatment     [location[i]:(location[i]+nnRNA[i]-1)]
  pooled_list      [[i]] = c(genedata$Control       [location[i]:(location[i]+nnRNA[i]-1)],
                             genedata$Treatment     [location[i]:(location[i]+nnRNA[i]-1)])
  fold_changes_list[[i]] =   genedata$`Fold changes`[location[i]:(location[i]+nnRNA[i]-1)]
}

names(control_list)      = c(genenames)
names(treatment_list)    = c(genenames)
names(pooled_list)       = c(genenames)
names(fold_changes_list) = c(genenames)

#### table of list

table(summary(control_list))
table(summary(treatment_list))
table(summary(pooled_list))

table(as.numeric(nnRNA == 1))
which(nnRNA == 1)

#### comapare value in list to original data

compare(genedata[1:18,3], c(control_list[[1]],control_list[[2]],control_list[[3]]), 
        round = TRUE, allowAll=TRUE) #TRUE
compare(genedata[1:18,4], c(treatment_list[[1]],treatment_list[[2]],treatment_list[[3]]), 
        round = TRUE, allowAll=TRUE) #TRUE
compare(unlist(genedata[1:18,3:4]), 
        c(pooled_list[[1]],pooled_list[[2]],pooled_list[[3]]), 
        round = TRUE, allowAll=TRUE) #TRUE
compare(genedata[1:18,5], c(fold_changes_list[[1]],fold_changes_list[[2]],fold_changes_list[[3]]), 
        round = TRUE, allowAll=TRUE) #TRUE



hist(    round(unlist(fold_changes_list[c(1:nGene)]),3)[1:nRNA])
hist(log(round(unlist(fold_changes_list[c(1:nGene)]),3)[1:nRNA]))   #so I need to log???


#>>>>>>>>>>>BUG1<<<<<<<<<<<<<<<<<

###testing

length(round(unlist(genedata[,3]),3))
length(round(unlist(control_list[c(1:nGene)]),3))

compare(round(unlist(genedata[,3]),3),
        round(unlist(control_list[c(1:nGene)]),3))

compare(round(unlist(genedata[,3]),3)[1:500],
        round(unlist(control_list[c(1:nGene)]),3)[1:500], allowAll=TRUE)


round(unlist(genedata[,3]),3)
round(unlist(control_list[c(1:nGene)]),3)[1:nRNA]

tail(round(unlist(genedata[,  3]),3))
tail(round(unlist(control_list     [c(1:nGene)]),3)[1:nRNA])

a = round(unlist(genedata[,3]),3)
b = round(unlist(control_list[c(1:nGene)]),3)[1:nRNA]
compare(a, b, allowAll=TRUE)

###



data1 = round(unlist(genedata[,  3]),3)
data2 = round(unlist(genedata[,  4]),3)
data3 = round(unlist(genedata[,3:4]),3)
data4 = round(unlist(genedata[,  5]),3)
list1 = round(unlist(control_list     [c(1:nGene)]),3)[1:nRNA]
list2 = round(unlist(treatment_list   [c(1:nGene)]),3)[1:nRNA]
list3 = round(unlist(pooled_list      [c(1:nGene)]),3)[1:nRNA]
list4 = round(unlist(fold_changes_list[c(1:nGene)]),3)[1:nRNA]

compare(data1, list1, ignoreNames=TRUE)
compare(data2, list2, ignoreNames=TRUE)
compare(data3, list3, ignoreNames=TRUE)
compare(data4, list4, ignoreNames=TRUE)

hist(log(data1))
hist(log(list1))



#############


#----------------------------------Analysis-----------------------------------------------------#


############################################################
#    Difference from Bayesian T-test
############################################################

#https://stats.stackexchange.com/questions/130389/bayesian-equivalent-of-two-sample-t-test 

### Bayesian t-test  # 102 single obs gene, what to do? 


# https://www.r-bloggers.com/bayesian-first-aid-two-sample-t-test/ ### good stuff here
install.packages("BayesianFirstAid")
library( BayesianFirstAid)

### use 1% sample to test bayes speed
pect1 = round(nGene*0.01,0)
sample.1 <- control_list[c(1:pect1)]
sample.2 <- treatment_list[c(1:pect1)]


sample.1 <- control_list[c(1:10)]
sample.2 <- treatment_list[c(1:10)]

pooled<-lapply(names(sample.1),function(x) c(sample.1[[x]],sample.2[[x]]))
names(pooled)<-names(sample.1)

par(mfrow=c(5, 2))
for (i in 1:5){
  hist(sample.1[[i]])
  hist(sample.2[[i]])
}
par(mfrow=c(1, 1))
#########

sample.1 <- control_list[[673]]
sample.2 <- treatment_list[[673]]

sample.1 <- control_list[[1]]
sample.2 <- treatment_list[[1]] 

pooled   <- c(sample.1, sample.2) #we need a pooled data set for estimating parameters in the prior.

par(mfrow=c(1, 2))

hist(sample.1)
hist(sample.2)
par(mfrow=c(1, 1))






### likelihood

likelihood <- function(parameters){
  mu1=parameters[1]; sig1=parameters[2]; mu2=parameters[3]; sig2=parameters[4]
  prod(dnorm(sample.1, mu1, sig1)) * prod(dnorm(sample.2, mu2, sig2))
}

### prior

prior <- function(parameters){
  mu1=parameters[1]; sig1=parameters[2]; mu2=parameters[3]; sig2=parameters[4]
  dnorm(mu1, mean(pooled), 1000*sd(pooled)) * dnorm(mu2, mean(pooled), 1000*sd(pooled)) * dexp(sig1, rate=0.1) * dexp(sig2, 0.1)
}

### posterior

posterior <- function(parameters) {likelihood(parameters) * prior(parameters)}



### starting values
mu1 = 100; sig1 = 10; mu2 = 100; sig2 = 10
parameters <- c(mu1, sig1, mu2, sig2)

### this is the MCMC /w Metropolis method
n.iter <- 10000
results <- matrix(0, nrow=n.iter, ncol=4)
results[1, ] <- parameters
for (iteration in 2:n.iter){
  candidate <- parameters + rnorm(4, sd=0.5)
  a = posterior(candidate)
  b = posterior(parameters)
  ratio <- posterior(candidate)/posterior(parameters)
  
  aff<-c(4,8,12)    
  bff<-c(2,4,6)    
  aff/bff
  
  if (runif(1) < ratio) parameters <- candidate #Metropolis modification
  results[iteration, ] <- parameters
}

#burn-in
results <- results[500:n.iter,]

mu1 <- results[,1]
mu2 <- results[,3]

hist(mu1 - mu2)

library(rjags)
model.str <- 'model {
    for (i in 1:Ntotal) {
y[i] ~ dt(mu[x[i]], tau[x[i]], nu)
}
for (j in 1:2) {
mu[j] ~ dnorm(mu_pooled, tau_pooled)
tau[j] <- 1 / pow(sigma[j], 2)
sigma[j] ~ dunif(sigma_low, sigma_high)
}
nu <- nu_minus_one + 1
nu_minus_one ~ dexp(1 / 29)
}'

# Indicator variable
x <- c(rep(1, length(sample.1)), rep(2, length(sample.2)))

cpd.model <- jags.model(textConnection(model.str),
                        data=list(y=pooled,
                                  x=x,
                                  mu_pooled=mean(pooled),
                                  tau_pooled=1/(1000 * sd(pooled))^2,
                                  sigma_low=sd(pooled) / 1000,
                                  sigma_high=sd(pooled) * 1000,
                                  Ntotal=length(pooled)))
update(cpd.model, 1000)
chain <- coda.samples(model = cpd.model, n.iter = 100000,
                      variable.names = c('mu', 'sigma'))
rchain <- as.matrix(chain)
hist(rchain[, 'mu[1]'] - rchain[, 'mu[2]'])

mean(rchain[, 'mu[1]'] - rchain[, 'mu[2]'] < 0)
mean(rchain[, 'mu[2]'] - rchain[, 'mu[2]'] > 0)






