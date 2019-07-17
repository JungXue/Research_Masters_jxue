####----------------------------------Premeables-----------------------------------------####

# setwd("H:/Desktop/7.1 No time to waste/Stats 798A Research Master/R-codes/JAGS")

library(dplyr)
library(tidyr)
library(coda)
library(rjags)
library(R2jags)
library(lattice)
library(mcmcplots)
library(bayesboot)
library(Rlab)
library(MCMCvis)
library(cAIC4)
library(AICcmodavg)
library(loo)
library(xtable)

# Packages <- c("glue","readxl", "tidyverse","xtable","rjags","ggplot2")
# lapply(Packages, install.packages, character.only = TRUE)

source("11a-rhodata.R")
source("11b-rhodata1day.R")
source("13-readallexcel.R")

set.seed(1234567)

####------------------------------- Import Data --------------------------------------####

setwd("H:/Desktop/7.1 No time to waste/Stats 798A Research Master/R-codes/Simulation")
load("rdata/test/raw1.RData")
load("rdata/test/daily1.RData")
load("rdata/anomaly/daily1.S25.RData")
setwd("H:/Desktop/7.1 No time to waste/Stats 798A Research Master/R-codes/JAGS")


### Note: adte of anomaly = "2015-11-15"

n.cat1 = length(levels(raw1.df$cat1))
n.leaf = length(levels(raw1.df$leaf))
N = nrow(daily1.df)
rawdata <- raw1.df
dailydata <- daily1.df
anomalydate = "2015-11-15"

#rho.df = rhodata(rawdata = raw1.df, 
#                 dailydata = daily1.df, 
#                 method = "mean",
#                 data ="simulation", 
#                 outputdata="rdata/sim1/rho.RData")

load("rdata/sim1/rho.RData")

head(daily1.df)
head(daily1.S25.df)
head(rho.df)

anomalydateloc = which(daily1.df$day == "2015-11-15")

daily1.df[anomalydateloc,]
daily1.S25.df[anomalydateloc,]

####---------------------------- Assign model -------------------------------------####

### Assign model values

Y_all_norm  <- as.matrix(daily1.df[2:length(daily1.df)])
Y_1day_norm <- as.matrix(daily1.df[2:length(daily1.df)])[anomalydateloc,]
Y_1day_abn  <- as.matrix(daily1.S25.df[2:length(daily1.df)])[anomalydateloc,]

Nday_all_norm  <- nrow(Y)
Nday_all_norm  <- nrow(Y)
Nday_all_norm  <- nrow(Y)


Nleaf <- ncol(Y)
rho   <- as.matrix(rho.df[,3:length(rho.df)])
rho   <- rho [1,]

Prior = rho 
likelihood = Y  # We are considering 


### Source models

source("model1.R")
source("model2.R")
source("model3.R")
source("model4.R")
source("model5.R") 
source("model6.R")

### Create Jags models

### Note can not use save load for jags.model

jags.data1 <- list(Y=Y,Nday=Nday,Nleaf=Nleaf,rho=rho) 
jags.data2 <- list(Y=Y,Nday=Nday,Nleaf=Nleaf,rho=rho) 
jags.data3 <- list(Y=Y,Nday=Nday,Nleaf=Nleaf,rho=rho) 
jags.data4 <- list(Y=Y,Nday=Nday,Nleaf=Nleaf,rho=rho) 
jags.data5 <- list(Y=Y,Nday=Nday,Nleaf=Nleaf,rho=rho) 
jags.data6 <- list(Y=Y,Nday=Nday,Nleaf=Nleaf,rho=rho) 

model1.jags <- jags.model(textConnection(model1.txt), data = jags.data1 ,n.chains=3,n.adapt=1000)
model2.jags <- jags.model(textConnection(model2.txt), data = jags.data2 ,n.chains=3,n.adapt=1000)
model3.jags <- jags.model(textConnection(model3.txt), data = jags.data3 ,n.chains=3,n.adapt=1000)
model4.jags <- jags.model(textConnection(model4.txt), data = jags.data4 ,n.chains=3,n.adapt=1000)
model5.jags <- jags.model(textConnection(model5.txt), data = jags.data5 ,n.chains=3,n.adapt=1000)
model6.jags <- jags.model(textConnection(model6.txt), data = jags.data6 ,n.chains=3,n.adapt=1000)

####---------------------------- calculate DIC-------------------------------------####

### dic comaprison  # Note 1000 sample did not give stable dic, try 100000

dic.mod1 <- dic.samples(model1.jags, 10000, "pD")
dic.mod2 <- dic.samples(model2.jags, 10000, "pD")
dic.mod3 <- dic.samples(model3.jags, 10000, "pD")
dic.mod4 <- dic.samples(model4.jags, 10000, "pD")
dic.mod5 <- dic.samples(model5.jags, 10000, "pD")
dic.mod6 <- dic.samples(model6.jags, 10000, "pD")

save(dic.mod1,file="rdata/sim1/dic.mod1.RData")
save(dic.mod2,file="rdata/sim1/dic.mod2.RData")
save(dic.mod3,file="rdata/sim1/dic.mod3.RData")
save(dic.mod4,file="rdata/sim1/dic.mod4.RData")
save(dic.mod5,file="rdata/sim1/dic.mod5.RData")
save(dic.mod6,file="rdata/sim1/dic.mod6.RData")

load("rdata/sim1/dic.mod1.RData")
load("rdata/sim1/dic.mod2.RData")
load("rdata/sim1/dic.mod3.RData")
load("rdata/sim1/dic.mod4.RData")
load("rdata/sim1/dic.mod5.RData")
load("rdata/sim1/dic.mod6.RData")


### Trying AIC, result does not make sense. 
# Cand.mods <- list(model1b.jags)
# Model.names <- "hierarchical model"
# aictab(cand.set = Cand.mods, modnames = Model.names)

### Model 6 is the best


####-------------------------- Simulate MCMC Chain ------------------------------####

update(model1.jags,1000) 
update(model2.jags,1000) 
update(model3.jags,1000) 
update(model4.jags,1000) 
update(model5.jags,1000) 
update(model6.jags,1000) 

### check which samplers are being used

list.samplers(model1.jags) 
list.samplers(model2.jags) 
list.samplers(model3.jags) 
list.samplers(model4.jags) 
list.samplers(model5.jags) 
list.samplers(model6.jags) 


### Assign parameter

params1 = c("mu")
params2 = c("mu")
params3 = c("mu")
params4 = c("mu")
params5 = c("mu")
params6 = c("mu")

### Raftery-Lewis diagnostic estimate burnin and sample

model1.test <- coda.samples(model1.jags, params1, n.iter=500, thin=1)
model2.test <- coda.samples(model2.jags, params2, n.iter=500, thin=1)
model3.test <- coda.samples(model3.jags, params3, n.iter=500, thin=1)
model4.test <- coda.samples(model4.jags, params4, n.iter=500, thin=1)
model5.test <- coda.samples(model5.jags, params5, n.iter=500, thin=1)
model6.test <- coda.samples(model6.jags, params6, n.iter=500, thin=1)

raftery.diag(model1.test)  # rep(3746,6)
raftery.diag(model2.test)
raftery.diag(model3.test)
raftery.diag(model4.test)
raftery.diag(model5.test)
raftery.diag(model6.test)

### run coda.samples 

model1.sim <- coda.samples(model1.jags, params1, n.iter=4000, thin=4)
model2.sim <- coda.samples(model2.jags, params2, n.iter=4000, thin=4)
model3.sim <- coda.samples(model3.jags, params3, n.iter=4000, thin=4)
model4.sim <- coda.samples(model4.jags, params4, n.iter=4000, thin=4)
model5.sim <- coda.samples(model5.jags, params5, n.iter=4000, thin=4)
model6.sim <- coda.samples(model6.jags, params6, n.iter=4000, thin=4)

save(model1.sim,file="rdata/sim1/model1.sim.RData")
save(model2.sim,file="rdata/sim1/model2.sim.RData")
save(model3.sim,file="rdata/sim1/model3.sim.RData")
save(model4.sim,file="rdata/sim1/model4.sim.RData")
save(model5.sim,file="rdata/sim1/model5.sim.RData")
save(model6.sim,file="rdata/sim1/model6.sim.RData")

load("rdata/sim1/model1.sim.RData")
load("rdata/sim1/model2.sim.RData")
load("rdata/sim1/model3.sim.RData")
load("rdata/sim1/model4.sim.RData")
load("rdata/sim1/model5.sim.RData")
load("rdata/sim1/model6.sim.RData")

### convergence
# gelman.diag(model2.sim)
# geweke.diag(model2.sim)
# heidel.diag(model2.sim)

#-------------------------------rename and combine for plots ---------------------------------

n.chain=3

total   = c("total")
brunch1 = c("A","B")
leaves  = c("AA","AB","BA","BB")

MyText<- names(daily.df)[3:ncol(daily.df)]
MyText<- c(total,brunch1,leaves)
MyText

# create a chain for total of all models

modelall.sim = model1.sim

for(i in 1:3){
modelall.sim[[i]][,1] <- model1.sim[[i]][,1]
modelall.sim[[i]][,2] <- model2.sim[[i]][,1]
modelall.sim[[i]][,3] <- model3.sim[[i]][,1]
modelall.sim[[i]][,4] <- model4.sim[[i]][,1]
modelall.sim[[i]][,5] <- model5.sim[[i]][,1]
modelall.sim[[i]][,6] <- model6.sim[[i]][,1]
modelall.sim[[i]]     <- modelall.sim[[i]][,-7]
}
modelnames <- c("model1","model2","model3","model4","model5","model6")
for (i in 1:n.chain) colnames(modelall.sim[[i]]) <- modelnames

modelA.sim = model1.sim

for(i in 1:3){
  modelA.sim[[i]][,1] <- model1.sim[[i]][,2]
  modelA.sim[[i]][,2] <- model2.sim[[i]][,2]
  modelA.sim[[i]][,3] <- model3.sim[[i]][,2]
  modelA.sim[[i]][,4] <- model4.sim[[i]][,2]
  modelA.sim[[i]][,5] <- model5.sim[[i]][,2]
  modelA.sim[[i]][,6] <- model6.sim[[i]][,2]
  modelA.sim[[i]]     <- modelA.sim[[i]][,-7]
}
modelnames <- c("model1","model2","model3","model4","model5","model6")
for (i in 1:n.chain) colnames(modelA.sim[[i]]) <- modelnames

modelAA.sim = model1.sim

for(i in 1:3){
  modelAA.sim[[i]][,1] <- model1.sim[[i]][,4]
  modelAA.sim[[i]][,2] <- model2.sim[[i]][,4]
  modelAA.sim[[i]][,3] <- model3.sim[[i]][,4]
  modelAA.sim[[i]][,4] <- model4.sim[[i]][,4]
  modelAA.sim[[i]][,5] <- model5.sim[[i]][,4]
  modelAA.sim[[i]][,6] <- model6.sim[[i]][,4]
  modelAA.sim[[i]]     <- modelAA.sim[[i]][,-7]
}
modelnames <- c("model1","model2","model3","model4","model5","model6")
for (i in 1:n.chain) colnames(modelAA.sim[[i]]) <- modelnames




####================================================================================####


#------------------------------- MCMC Summary ---------------------------------

# xtable(compare_model,digits = 6, )

### Posterior summary for total of each model

MCMCsummary(modelall.sim, round = 5 , n.eff = T)
MCMCsummary(modelA.sim,   round = 5 , n.eff = T)
MCMCsummary(modelAA.sim,  round = 5 , n.eff = T)

xtable(MCMCsummary(modelall.sim, n.eff = T),digits = 4,type = "latex", file = "plots/sim1/MCMCsumaTotal.tex")
xtable(MCMCsummary(modelA.sim, n.eff = T),digits = 4,type = "latex", file = "plots/sim1/MCMCsumbA.tex")
xtable(MCMCsummary(modelAA.sim, n.eff = T),digits = 4,type = "latex", file = "plots/sim1/MCMCsumcAA.tex")

"rdata/sim1/model1.sim.RData"

### trace plots  

pdf("plots/findmodel/Traceall.PDF")
    xyplot(modelall.sim, main="Trace plot for Total of all models", 
           strip=F, strip.left=strip.custom(factor.levels=modelnames ,bg="skyblue"))
dev.off()

### autocorrelation

pdf("plots/findmodel/Acfall.PDF")
acfplot(modelall.sim, main="Autocorrelation plot for Total of all models", 
        strip=F, strip.left=strip.custom(factor.levels=modelnames ,bg="skyblue"))
dev.off()


### caterpillar plot

pdf("plots/findmodel/Caterall.PDF")
par(mfrow=c(1,1))
caterplot(modelall.sim,labels.loc = "above",labels = modelnames ,style = "plain")
title( main = "Caterpillar plot for Total of all models" , xlab ="Parameter estimate")
dev.off()


### density of posterior distributions
y1 <- modelall.sim[[1]][,1]
y2 <- modelall.sim[[1]][,2]
y3 <- modelall.sim[[1]][,3]
y4 <- modelall.sim[[1]][,4]
y5 <- modelall.sim[[1]][,5]
y6 <- modelall.sim[[1]][,6]
y1.df <- data.frame(y1)
y2.df <- data.frame(y2)
y3.df <- data.frame(y3)
y4.df <- data.frame(y4)
y5.df <- data.frame(y5)
y6.df <- data.frame(y6)


p1 <- ggplot(y1.df, aes(x=y1)) +   
  geom_histogram(binwidth=1,aes(y=..density..),color="dark grey",fill="light blue") + 
  geom_density(aes(y=..density..),alpha=.2, fill="light blue")+scale_x_continuous(limits = c(5, 40))+
  geom_vline(xintercept = rho[1], linetype="dashed", color = "red", size=0.5)
  geom_vline(xintercept = Y[1], linetype="dashed", color = "red", size=0.5)

p2 <- ggplot(y2.df, aes(x=y2)) +   
  geom_histogram(binwidth=0.02,aes(y=..density..),color="dark grey",fill="light blue") + 
  geom_density(aes(y=..density..),alpha=.2, fill="light blue")+scale_x_continuous(limits = c(20.85, 21.25))+
  geom_vline(xintercept = rho[1], linetype="dashed", color = "red", size=0.5)

p3 <- ggplot(y3.df, aes(x=y3)) +   
  geom_histogram(binwidth=0.02,aes(y=..density..),color="dark grey",fill="light blue") + 
  geom_density(aes(y=..density..),alpha=.2, fill="light blue")+scale_x_continuous(limits = c(20.85, 21.25))+
  geom_vline(xintercept = rho[1], linetype="dashed", color = "red", size=0.5)

p4 <- ggplot(y4.df, aes(x=y4)) +   
  geom_histogram(binwidth=0.02,aes(y=..density..),color="dark grey",fill="light blue") + 
  geom_density(aes(y=..density..),alpha=.2, fill="light blue")+scale_x_continuous(limits = c(20.85, 21.25)) +
  geom_vline(xintercept = rho[1], linetype="dashed", color = "red", size=0.5)

p5 <- ggplot(y5.df, aes(x=y5)) +   
  geom_histogram(binwidth=0.02,aes(y=..density..),color="dark grey",fill="light blue") + 
  geom_density(aes(y=..density..),alpha=.2, fill="light blue")+scale_x_continuous(limits = c(20.85, 21.25))+
  geom_vline(xintercept = rho[1], linetype="dashed", color = "red", size=0.5)

p6 <- ggplot(y6.df, aes(x=y6)) +   
  geom_histogram(binwidth=0.02,aes(y=..density..),color="dark grey",fill="light blue") + 
  geom_density(aes(y=..density..),alpha=.2, fill="light blue")+scale_x_continuous(limits = c(20.85, 21.25))+
  geom_vline(xintercept = rho[1], linetype="dashed", color = "red", size=0.5)

grid.arrange(p1 + ggtitle("model 1: Null prior"), 
             p2 + ggtitle("model 2: Normal(1,0.3) prior"), 
             p3 + ggtitle("model 3: Normal(1,0.1) prior"),
             p4 + ggtitle("model 4: Gamma(4,3) prior"),
             p5 + ggtitle("model 5: Laplace(1,1) prior"),
             p6 + ggtitle("model 6: 0.1 Mixture prior"),ncol=2)


pdf("plots/findmodel/Densityall.PDF")
    densityplot(modelall.sim, main="Posterior density for Total of all models", 
                strip=F, strip.left=strip.custom(factor.levels=modelnames ,bg="skyblue"))
dev.off()


### https://www.r-bloggers.com/check-your-prior-posterior-overlap-ppo-mcmc-wrangling-in-r-made-easy-with-mcmcvis/

spike<-rbinom(N,1,0.9)
mu2.y6 = rnorm(N,1,0.1)   
sigma2.y6 = rnorm(N,1,0.1)
slab <- rnorm(N, mu2.y6, sqrt(sigma2.y6))

y6 <- rnorm(N, spike*1+(1-spike)*slab, .1)
y6 <- y6[-which(y6<0)]
y6a <- sample(y6,size=nrow(model6.sim[[1]]),replace = T)
y6b <- sample(y6,size=nrow(model6.sim[[2]]),replace = T)
y6c <- sample(y6,size=nrow(model6.sim[[3]]),replace = T)
PR=list()
PR[[1]] <- outer(y6a,as.numeric(as.vector(rho.df[1,3:9])),FUN = "*")
PR[[2]] <- outer(y6b,as.numeric(as.vector(rho.df[1,3:9])),FUN = "*")
PR[[3]] <- outer(y6c,as.numeric(as.vector(rho.df[1,3:9])),FUN = "*")
MCMCtrace(model6.sim, params = 'total', priors = PR, pdf = FALSE)


### error in caterplot intervals

#---------------------------Latex Tables all models------------------------------




#---------------------------------Tables -------------------------------------------


### 1. DIC table


M1M2 <- summary(diffdic(dic.mod1, dic.mod2))
M1M3 <- summary(diffdic(dic.mod1, dic.mod3))
M1M4 <- summary(diffdic(dic.mod1, dic.mod4))
M1M5 <- summary(diffdic(dic.mod1, dic.mod5))
M1M6 <- summary(diffdic(dic.mod1, dic.mod6))
M2M3 <- summary(diffdic(dic.mod2, dic.mod3))
M2M4 <- summary(diffdic(dic.mod2, dic.mod4))
M2M5 <- summary(diffdic(dic.mod2, dic.mod5))
M2M6 <- summary(diffdic(dic.mod2, dic.mod6))
M3M4 <- summary(diffdic(dic.mod3, dic.mod4))
M3M5 <- summary(diffdic(dic.mod3, dic.mod5))
M3M6 <- summary(diffdic(dic.mod3, dic.mod6))
M4M5 <- summary(diffdic(dic.mod4, dic.mod5))
M4M6 <- summary(diffdic(dic.mod4, dic.mod6))
M5M6 <- summary(diffdic(dic.mod5, dic.mod6))

density(M1M3)

compare_model = rbind(M1M2,M1M3,M1M4,M1M5,M1M6,
                      M2M3,M2M4,M2M5,M2M6,
                      M3M4,M3M5,M3M6,
                      M4M5,M4M6,
                      M5M6)
row.names(compare_model) = c("M1:M2","M1:M3","M1:M4","M1:M5",'M1:M6',
                             "M2:M3","M2:M4","M2:M5","M2:M6",
                             "M3:M4","M3:M5","M3:M6",
                             "M4:M5","M4:M6",
                             "M5:M6")
round(compare_model,6)

save(compare_model,file="rdata/sim1/dictable.RData")
load("rdata/sim1/dictable.RData")

plot(dic.mod1)

# xtable(compare_model,digits = 6, type = "latex", file = "plots/dictable.tex")


### 2. samplers

model1 <-c(" ")
model2 <-c("lambda","mu2.y2","sigma2.y2")
model3 <-c("lambda","mu2.y3","sigma2.y3")
model4 <-c("lambda","alpha.y4","beta.y4")
model5 <-c("lambda","mean.y5","scale.y5")
model6 <-c("mu2.y6","sigma2.y6")

sampler = rbind(model1,model2,model3,model4, model5, model6)

save(sampler,file="rdata/sim1/samplertable.RData")
load("rdata/sim1/samplertable.RData")

# xtable(sampler,digits = 6, type = "latex", file = "plots/samplertable.tex")









### Posterior summary for model 6
MCMCsummary(model6.sim, round = 3 , n.eff = T)



model6.sim[[1]][,1]
plot(density(model6.sim[[1]][,1]))
abline(v= rho[1],col="red")
abline(v= rho[1]*1.001,col="blue")
sum(model6.sim[[1]][,1] > rho[1])/1000

posterior1 = sum(model6.sim[[1]][,1] >= rho[1]*1.000)/1000
posterior2 = sum(model6.sim[[1]][,1] >= rho[1]*1.001)/1000
posterior1
posterior2 
round(posterior,digits = 6)





#################https://cran.r-project.org/web/packages/ggmcmc/vignettes/using_ggmcmc.html








