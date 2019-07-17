####----------------------------------Premeables-----------------------------------------####

setwd("H:/Desktop/7.1 No time to waste/Stats 798A Research Master/R-codes/JAGS")

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
library(ggplot2)
library(gridExtra)

source("11a-rhodata.R")
source("11b-rhodata1day.R")
source("13-readallexcel.R")
source("16-posttheta.R")
source("17-dictable.R")

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
#                 outputdata="rdata/findmodel/rho.RData")

load("rdata/findmodel/rho.RData")

head(daily1.df)
head(daily1.S25.df)
head(rho.df)

anomalydateloc = which(daily1.df$day == "2015-11-15")

daily1.df[anomalydateloc,]
daily1.S25.df[anomalydateloc,]

####---------------------------- Assign model -------------------------------------####

### Assign model values

Y <- as.matrix(daily1.df[anomalydateloc,2:length(daily1.df)])

Nday  <- nrow(Y)
Nleaf <- ncol(Y)
rho   <- as.matrix(rho.df[,3:length(rho.df)])[1,]

### Source models

source("model1.R")   # solo 
source("model2.R")
source("model3.R")
source("model4.R")
source("model5.R") 
source("model6.R")

source("model1h.R")  # hierarchical
source("model2h.R")
source("model3h.R")
source("model4h.R")
source("model5h.R") 
source("model6h.R")

### Create Jags models       Note: can not use save load for jags.model

jags.data1 <- list(Y=Y,Nday=Nday,Nleaf=Nleaf,rho=rho) 
jags.data2 <- list(Y=Y,Nday=Nday,Nleaf=Nleaf,rho=rho) 
jags.data3 <- list(Y=Y,Nday=Nday,Nleaf=Nleaf,rho=rho) 
jags.data4 <- list(Y=Y,Nday=Nday,Nleaf=Nleaf,rho=rho) 
jags.data5 <- list(Y=Y,Nday=Nday,Nleaf=Nleaf,rho=rho) 
jags.data6 <- list(Y=Y,Nday=Nday,Nleaf=Nleaf,rho=rho) 

jags.data1h <- list(Y=Y1,Nday=Nday,Nleaf=Nleaf,rho=rho,Nlv1=Nlv1,Nlv2=Nlv2,lV1b=lV1b) 
jags.data2h <- list(Y=Y2,Nday=Nday,Nleaf=Nleaf,rho=rho,Nlv1=Nlv1,Nlv2=Nlv2,lV1b=lV1b) 
jags.data3h <- list(Y=Y3,Nday=Nday,Nleaf=Nleaf,rho=rho,Nlv1=Nlv1,Nlv2=Nlv2,lV1b=lV1b) 
jags.data4h <- list(Y=Y4,Nday=Nday,Nleaf=Nleaf,rho=rho,Nlv1=Nlv1,Nlv2=Nlv2,lV1b=lV1b) 
jags.data5h <- list(Y=Y5,Nday=Nday,Nleaf=Nleaf,rho=rho,Nlv1=Nlv1,Nlv2=Nlv2,lV1b=lV1b) 
jags.data6h <- list(Y=Y6,Nday=Nday,Nleaf=Nleaf,rho=rho,Nlv1=Nlv1,Nlv2=Nlv2,lV1b=lV1b) 
jags.data7h <- list(Y=Y7,Nday=Nday,Nleaf=Nleaf,rho=rho,Nlv1=Nlv1,Nlv2=Nlv2,lV1b=lV1b) 


model1.jags <- jags.model(textConnection(model1.txt), data = jags.data1 ,n.chains=3,n.adapt=1000)
model2.jags <- jags.model(textConnection(model2.txt), data = jags.data2 ,n.chains=3,n.adapt=1000)
model3.jags <- jags.model(textConnection(model3.txt), data = jags.data3 ,n.chains=3,n.adapt=1000)
model4.jags <- jags.model(textConnection(model4.txt), data = jags.data4 ,n.chains=3,n.adapt=1000)
model5.jags <- jags.model(textConnection(model5.txt), data = jags.data5 ,n.chains=3,n.adapt=1000)
model6.jags <- jags.model(textConnection(model6.txt), data = jags.data6 ,n.chains=3,n.adapt=1000)

####---------------------------- calculate DIC-------------------------------------####

### dic comaprison  # Note 1000 sample did not give stable dic, try 100000

dic.mod1 <- dic.samples(model1.jags, 1000000, "pD")
dic.mod2 <- dic.samples(model2.jags, 1000000, "pD")
dic.mod3 <- dic.samples(model3.jags, 1000000, "pD")
dic.mod4 <- dic.samples(model4.jags, 1000000, "pD")
dic.mod5 <- dic.samples(model5.jags, 1000000, "pD")
dic.mod6 <- dic.samples(model6.jags, 1000000, "pD")

save(dic.mod1,file="rdata/findmodel/dic.2day.mod1.RData")
save(dic.mod2,file="rdata/findmodel/dic.2day.mod2.RData")
save(dic.mod3,file="rdata/findmodel/dic.2day.mod3.RData")
save(dic.mod4,file="rdata/findmodel/dic.2day.mod4.RData")
save(dic.mod5,file="rdata/findmodel/dic.2day.mod5.RData")
save(dic.mod6,file="rdata/findmodel/dic.2day.mod6.RData")

load("rdata/findmodel/dic.2day.mod1.RData")
load("rdata/findmodel/dic.2day.mod2.RData")
load("rdata/findmodel/dic.2day.mod3.RData")
load("rdata/findmodel/dic.2day.mod4.RData")
load("rdata/findmodel/dic.2day.mod5.RData")
load("rdata/findmodel/dic.2day.mod6.RData")

####-------------------------- Simulate MCMC Chain ------------------------------####

update(model1.jags,1000) 
update(model2.jags,1000) 
update(model3.jags,1000) 
update(model4.jags,1000) 
update(model5.jags,1000) 
update(model6.jags,1000) 

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

save(model1.sim,file="rdata/findmodel//model1.2day.sim.RData")
save(model2.sim,file="rdata/findmodel//model2.2day.sim.RData")
save(model3.sim,file="rdata/findmodel//model3.2day.sim.RData")
save(model4.sim,file="rdata/findmodel//model4.2day.sim.RData")
save(model5.sim,file="rdata/findmodel//model5.2day.sim.RData")
save(model6.sim,file="rdata/findmodel//model6.2day.sim.RData")

load("rdata/findmodel//model1.2day.sim.RData")
load("rdata/findmodel//model2.2day.sim.RData")
load("rdata/findmodel//model3.2day.sim.RData")
load("rdata/findmodel//model4.2day.sim.RData")
load("rdata/findmodel//model5.2day.sim.RData")
load("rdata/findmodel//model6.2day.sim.RData")

#-------------------------------renaming and combine for plots ---------------------------------

n.chain = 3

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
setwd("H:/Desktop/7.1 No time to waste/Stats 798A Research Master/R-codes/JAGS")

#---------------------------------DIC table -------------------------------------------

dictablez = dictable()
dictablez
xtable(dictablez ,digits = 6, type = "latex", file = "plots/findmodel/dictable.2day.tex")

save(dictablez ,file="rdata/findmodel/dictable.2day.RData")
load("rdata/findmodel/dictable.2day.RData")

#------------------------------- MCMC Summary ---------------------------------

### Posterior summary for total of each model

postsumtotal = MCMCsummary(modelall.sim, round = 5 , n.eff = T)
postsumA = MCMCsummary(modelA.sim,   round = 5 , n.eff = T)
postsumAA = MCMCsummary(modelAA.sim,  round = 5 , n.eff = T)

xtable(postsumtotal,digits = 4,type = "latex", file = "plots/findmodel/MCMCsumaTotal.2day.tex")
xtable(postsumA ,digits = 4,type = "latex", file = "plots/findmodel/MCMCsumbA.2day.tex")
xtable(postsumAA,digits = 4,type = "latex", file = "plots/findmodel/MCMCsumcAA.2day.tex")

save(postsumtotal,file="rdata/findmodel/postsumtotal.2day.RData")
save(postsumA,file="rdata/findmodel/postsumA.2day.RData")
save(postsumAA,file="rdata/findmodel/postsumAA.2day.RData")

load("rdata/findmodel/dictable.2day.RData")
load("rdata/findmodel/dictable.2day.RData")
load("rdata/findmodel/dictable.2day.RData")

#------------------------------- trace plots  ---------------------------------

pdf("plots/findmodel/Tracetotal.2day.PDF")
xyplot(modelall.sim, main="Trace plot for category Total of all models", 
       strip=F, strip.left=strip.custom(factor.levels=modelnames ,bg="skyblue"))
dev.off()

pdf("plots/findmodel/TraceA.2day.PDF")
xyplot(modelA.sim, main="Trace plot for category A of all models", 
       strip=F, strip.left=strip.custom(factor.levels=modelnames ,bg="skyblue"))
dev.off()

pdf("plots/findmodel/Trace1AA.2day.PDF")
xyplot(modelAA.sim, main="Trace plot for category AA of all models", 
       strip=F, strip.left=strip.custom(factor.levels=modelnames ,bg="skyblue"))
dev.off()

#------------------------------- density plots  ---------------------------------

pdf("plots/findmodel/Densitytotal.2day.PDF")
densityplot(modelall.sim, main="Posterior density for category Total of all models", 
            strip=F, strip.left=strip.custom(factor.levels=modelnames ,bg="skyblue"))
dev.off()

pdf("plots/findmodel/DensityA.2day.PDF")
densityplot(modelA.sim, main="Posterior density for category A of all models", 
            strip=F, strip.left=strip.custom(factor.levels=modelnames ,bg="skyblue"))
dev.off()

pdf("plots/findmodel/DensityAA.2day.PDF")
densityplot(modelAA.sim, main="Posterior density for category AA of all models", 
            strip=F, strip.left=strip.custom(factor.levels=modelnames ,bg="skyblue"))
dev.off()

#------------------------------- autocorrelation  ---------------------------------

pdf("plots/findmodel/Acftotal.2day.PDF")
acfplot(modelall.sim, main="Autocorrelation plot for category Total of all models", 
        strip=F, strip.left=strip.custom(factor.levels=modelnames ,bg="skyblue"))
dev.off()

pdf("plots/findmodel/AcfA.2day.PDF")
acfplot(modelA.sim, main="Autocorrelation plot for category A of all models", 
        strip=F, strip.left=strip.custom(factor.levels=modelnames ,bg="skyblue"))
dev.off()

pdf("plots/findmodel/AcfAA.2day.PDF")
acfplot(modelAA.sim, main="Autocorrelation plot for category AA of all models", 
        strip=F, strip.left=strip.custom(factor.levels=modelnames ,bg="skyblue"))
dev.off()

#------------------------------- caterpillar plot  ---------------------------------

pdf("plots/findmodel/Catertotal.2day.PDF")
par(mfrow=c(1,1))
caterplot(modelall.sim,labels.loc = "above",labels = modelnames ,style = "plain")
title( main = "Caterpillar plot for category Total of all models" , xlab ="Parameter estimate")
dev.off()

pdf("plots/findmodel/CaterA.2day.PDF")
par(mfrow=c(1,1))
caterplot(modelA.sim,labels.loc = "above",labels = modelnames ,style = "plain")
title( main = "Caterpillar plot for category A of all models" , xlab ="Parameter estimate")
dev.off()

pdf("plots/findmodel/CaterAA.2day.PDF")
par(mfrow=c(1,1))
caterplot(modelAA.sim,labels.loc = "above",labels = modelnames ,style = "plain")
title( main = "Caterpillar plot for category AA of all models" , xlab ="Parameter estimate")
dev.off()

#------------------------------- post theta plot  ---------------------------------

theta = posttheta (originalrho = rho, MCMC = model6.sim, k = 1/1.000)
theta

xtable(theta,digits = 4,type = "latex", file = "posttheta.2day.tex")
save(theta,file="rdata/findmodel/posttheta.2day.RData")
load("rdata/findmodel/posttheta.2day.RData")
