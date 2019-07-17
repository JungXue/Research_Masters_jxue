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
#                data ="simulation", 
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

Y <- as.matrix(daily1.S25.df[anomalydateloc,2:length(daily1.df)])

Nday  <- nrow(Y)
Nleaf <- ncol(Y)
rho   <- as.matrix(rho.df[,3:length(rho.df)])[1,]

### Source models

source("model1.R")
source("model2.R")
source("model3.R")
source("model4.R")
source("model5.R") 
source("model6.R")

### Create Jags models       Note: can not use save load for jags.model

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

dic.mod1 <- dic.samples(model1.jags, 1000000, "pD")
dic.mod2 <- dic.samples(model2.jags, 1000000, "pD")
dic.mod3 <- dic.samples(model3.jags, 1000000, "pD")
dic.mod4 <- dic.samples(model4.jags, 1000000, "pD")
dic.mod5 <- dic.samples(model5.jags, 1000000, "pD")
dic.mod6 <- dic.samples(model6.jags, 1000000, "pD")

save(dic.mod1,file="rdata/findmodel/dic.3abn.mod1.RData")
save(dic.mod2,file="rdata/findmodel/dic.3abn.mod2.RData")
save(dic.mod3,file="rdata/findmodel/dic.3abn.mod3.RData")
save(dic.mod4,file="rdata/findmodel/dic.3abn.mod4.RData")
save(dic.mod5,file="rdata/findmodel/dic.3abn.mod5.RData")
save(dic.mod6,file="rdata/findmodel/dic.3abn.mod6.RData")

load("rdata/findmodel/dic.3abn.mod1.RData")
load("rdata/findmodel/dic.3abn.mod2.RData")
load("rdata/findmodel/dic.3abn.mod3.RData")
load("rdata/findmodel/dic.3abn.mod4.RData")
load("rdata/findmodel/dic.3abn.mod5.RData")
load("rdata/findmodel/dic.3abn.mod6.RData")

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

save(model1.sim,file="rdata/findmodel//model1.3abn.sim.RData")
save(model2.sim,file="rdata/findmodel//model2.3abn.sim.RData")
save(model3.sim,file="rdata/findmodel//model3.3abn.sim.RData")
save(model4.sim,file="rdata/findmodel//model4.3abn.sim.RData")
save(model5.sim,file="rdata/findmodel//model5.3abn.sim.RData")
save(model6.sim,file="rdata/findmodel//model6.3abn.sim.RData")

load("rdata/findmodel//model1.3abn.sim.RData")
load("rdata/findmodel//model2.3abn.sim.RData")
load("rdata/findmodel//model3.3abn.sim.RData")
load("rdata/findmodel//model4.3abn.sim.RData")
load("rdata/findmodel//model5.3abn.sim.RData")
load("rdata/findmodel//model6.3abn.sim.RData")

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
xtable(dictablez ,digits = 3, type = "latex", file = "plots/findmodel/dictable.3abn.tex",
       caption = "DIC comapreisons for model1 tp model6", label = "tab:dic1")
?xtable
save(dictablez ,file="rdata/findmodel/dictable.3abn.RData")
load("rdata/findmodel/dictable.3abn.RData")

#------------------------------- MCMC Summary ---------------------------------

### Posterior summary for total of each model

postsumtotal = MCMCsummary(modelall.sim, round = 5 , n.eff = T)
postsumA = MCMCsummary(modelA.sim,   round = 5 , n.eff = T)
postsumAA = MCMCsummary(modelAA.sim,  round = 5 , n.eff = T)

cap1 = "Posterior distributions of different models for Total, with added anomalies, and calculated with independent Bayes model"
cap2 = "Posterior distributions of different models for A , with added anomalies, and calculated with independent Bayes model"
cap3 = "Posterior distributions of different models for AA, with added anomalies, and calculated with independent Bayes model"

cap4 = "Posterior distributions of different modelsfor Total, with added anomalies, and calculated with Hierarchiacl Bayes model"
cap5 = "Posterior distributions of different modelsfor A , with added anomalies, and calculated with Hierarchiacl Bayes model"
cap6 = "Posterior distributions of different modelsfor AA, with added anomalies, and calculated with Hierarchiacl Bayes model"


xtable(postsumtotal,digits = 4,type = "latex", file = "plots/findmodel/MCMCsumaTotal.3abn.tex",
       caption = cap1 ,label = "tab:modelpost1")
xtable(postsumA ,digits = 4,type = "latex", file = "plots/findmodel/MCMCsumbA.3abn.tex",
       caption = cap2 ,label = "tab:modelpost2")
xtable(postsumAA,digits = 4,type = "latex", file = "plots/findmodel/MCMCsumcAA.3abn.tex",
       caption = cap3,label = "tab:modelpost3")

save(postsumtotal,file="rdata/findmodel/postsumtotal.3abn.RData")
save(postsumA,file="rdata/findmodel/postsumA.3abn.RData")
save(postsumAA,file="rdata/findmodel/postsumAA.3abn.RData")

load("rdata/findmodel/dictable.3abn.RData")
load("rdata/findmodel/dictable.3abn.RData")
load("rdata/findmodel/dictable.3abn.RData")

#------------------------------- trace plots  ---------------------------------

pdf("plots/findmodel/Tracetotal3abn.PDF",width=12, height=6)
xyplot(modelall.sim,strip=F, strip.left=strip.custom(factor.levels=modelnames ,bg="skyblue"))
dev.off()

pdf("plots/findmodel/TraceA3abn.PDF",width=12, height=6)
xyplot(modelA.sim, strip=F, strip.left=strip.custom(factor.levels=modelnames ,bg="skyblue"))
dev.off()

pdf("plots/findmodel/TraceAA3abn.PDF",width=12, height=6)
xyplot(modelAA.sim, strip=F, strip.left=strip.custom(factor.levels=modelnames ,bg="skyblue"))
dev.off()

#------------------------------- density plots  ---------------------------------

pdf("plots/findmodel/Densitytotal3abn.PDF",width=12, height=6)
par(mar = c(0, 0, 0, 0),mai=c(0,0,0,0))
densityplot(modelall.sim, strip=F, strip.left=strip.custom(factor.levels=modelnames ,bg="skyblue"))
dev.off()

pdf("plots/findmodel/DensityA3abn.PDF",width=12, height=6)
densityplot(modelA.sim, strip=F, strip.left=strip.custom(factor.levels=modelnames ,bg="skyblue"))
dev.off()

pdf("plots/findmodel/DensityAA3abn.PDF",width=12, height=6)
densityplot(modelAA.sim, strip=F, strip.left=strip.custom(factor.levels=modelnames ,bg="skyblue"))
dev.off()

#------------------------------- autocorrelation  ---------------------------------

pdf("plots/findmodel/Acftotal3abn.PDF",width=12, height=6)
acfplot(modelall.sim, main="Autocorrelation plot for category Total of all models", 
        strip=F, strip.left=strip.custom(factor.levels=modelnames ,bg="skyblue"))
dev.off()

pdf("plots/findmodel/AcfA3abn.PDF",width=12, height=6)
acfplot(modelA.sim, main="Autocorrelation plot for category A of all models", 
        strip=F, strip.left=strip.custom(factor.levels=modelnames ,bg="skyblue"))
dev.off()

pdf("plots/findmodel/AcfAA3abn.PDF",width=12, height=6)
acfplot(modelAA.sim, main="Autocorrelation plot for category AA of all models", 
        strip=F, strip.left=strip.custom(factor.levels=modelnames ,bg="skyblue"))
dev.off()

#------------------------------- caterpillar plot  ---------------------------------

pdf("plots/findmodel/Catertotal.3abn.PDF")
par(mfrow=c(1,1))
caterplot(modelall.sim,labels.loc = "above",labels = modelnames ,style = "plain")
title( main = "Caterpillar plot for category Total of all models" , xlab ="Parameter estimate")
dev.off()

pdf("plots/findmodel/CaterA.3abn.PDF")
par(mfrow=c(1,1))
caterplot(modelA.sim,labels.loc = "above",labels = modelnames ,style = "plain")
title( main = "Caterpillar plot for category A of all models" , xlab ="Parameter estimate")
dev.off()

pdf("plots/findmodel/CaterAA.3abn.PDF")
par(mfrow=c(1,1))
caterplot(modelAA.sim,labels.loc = "above",labels = modelnames ,style = "plain")
title( main = "Caterpillar plot for category AA of all models" , xlab ="Parameter estimate")
dev.off()

#------------------------------- post theta plot  ---------------------------------

source("16-posttheta.R")

theta1 = postthetam (rho = rho,rhoh = rho, 
                     MCMC = modelall.sim, MCMCh = modelall.sim,
                     k = c(0.8,0.85,0.9,0.95,0.99))
theta2 = postthetam (rho = rho,rhoh = rho, 
                     MCMC = modelA.sim, MCMCh = modelA.sim,
                     k = c(0.8,0.85,0.9,0.95,0.99))
theta3 = postthetam (rho = rho,rhoh = rho, 
                     MCMC = modelAA.sim, MCMCh = modelAA.sim,
                     k = c(0.8,0.85,0.9,0.95,0.99))
theta1
theta1
theta3
  
xtable(theta1,digits = 4,type = "latex", file = "posttheta3abn.tex")
xtable(theta2,digits = 4,type = "latex", file = "posttheta3abn.tex")
xtable(theta3,digits = 4,type = "latex", file = "posttheta3abn.tex")