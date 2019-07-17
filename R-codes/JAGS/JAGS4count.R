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

####-------------------------------Import Data--------------------------------------------####

# Import datas

setwd("H:/Desktop/7.1 No time to waste/Stats 798A Research Master/R-codes/Simulation")

load("rdata/count/dailyp12a1.RData")
load("rdata/count/dailyp12a2.RData")
load("rdata/count/dailyp12a4.RData")
load("rdata/count/dailyp12a6.RData")
load("rdata/count/dailyp12a8.RData")
load("rdata/count/dailyp12a10.RData")
load("rdata/count/dailyp12a12.RData")

load("rdata/test/raw1.RData")
load("rdata/test/daily1.RData")
load("rdata/anomaly/daily1.S25.RData")

setwd("H:/Desktop/7.1 No time to waste/Stats 798A Research Master/R-codes/JAGS")

### Note: adte of anomaly = "2015-11-15"

n.cat1 = 2
n.leaf = 14
N = nrow(dailyp12a1.df)
rawdata <- raw1.df
dailydata <- daily1.df
anomalydate = "2015-11-15"

rho.df = rhodata(rawdata = raw1.df, 
                 dailydata = daily1.df, 
                 method = "mean",
                 data ="simulation", 
                 outputdata="rdata/sim2/rho.RData")

load("rdata/sim2/rho.RData")

head(dailyp12a1.df)
head(rho.df)

anomalydateloc = which(dailyp12a1.df$day == "2015-11-15")

dailyp12a1.df[anomalydateloc,]
dailyp12a2.df[anomalydateloc,]
dailyp12a4.df[anomalydateloc,]
dailyp12a6.df[anomalydateloc,]
dailyp12a8.df[anomalydateloc,]
dailyp12a10.df[anomalydateloc,]
dailyp12a12.df[anomalydateloc,]

####---------------------------- Assign model -------------------------------------####
### Assign model values

Y1 <- as.matrix(dailyp12a1.df[anomalydateloc,2:length(dailyp12a1.df)])
Y2 <- as.matrix(dailyp12a2.df[anomalydateloc,2:length(dailyp12a1.df)])
Y3 <- as.matrix(dailyp12a4.df[anomalydateloc,2:length(dailyp12a1.df)])
Y4 <- as.matrix(dailyp12a6.df[anomalydateloc,2:length(dailyp12a1.df)])
Y5 <- as.matrix(dailyp12a8.df[anomalydateloc,2:length(dailyp12a1.df)])
Y6 <- as.matrix(dailyp12a10.df[anomalydateloc,2:length(dailyp12a1.df)])
Y7 <- as.matrix(dailyp12a12.df[anomalydateloc,2:length(dailyp12a1.df)])

Nday  <- nrow(Y1)
Nleaf <- ncol(Y1)
rho   <- as.matrix(rho.df[,3:length(rho.df)])[1,]

### Source models

source("model2.R")
source("model2h.R")

### Create Jags models       Note: can not use save load for jags.model

jags.data1 <- list(Y=Y1,Nday=Nday,Nleaf=Nleaf,rho=rho) 
jags.data2 <- list(Y=Y2,Nday=Nday,Nleaf=Nleaf,rho=rho) 
jags.data3 <- list(Y=Y3,Nday=Nday,Nleaf=Nleaf,rho=rho) 
jags.data4 <- list(Y=Y4,Nday=Nday,Nleaf=Nleaf,rho=rho) 
jags.data5 <- list(Y=Y5,Nday=Nday,Nleaf=Nleaf,rho=rho) 
jags.data6 <- list(Y=Y6,Nday=Nday,Nleaf=Nleaf,rho=rho) 
jags.data7 <- list(Y=Y7,Nday=Nday,Nleaf=Nleaf,rho=rho) 

model1.jags <- jags.model(textConnection(model2.txt), data = jags.data1 ,n.chains=3,n.adapt=1000)
model2.jags <- jags.model(textConnection(model2.txt), data = jags.data2 ,n.chains=3,n.adapt=1000)
model3.jags <- jags.model(textConnection(model2.txt), data = jags.data3 ,n.chains=3,n.adapt=1000)
model4.jags <- jags.model(textConnection(model2.txt), data = jags.data4 ,n.chains=3,n.adapt=1000)
model5.jags <- jags.model(textConnection(model2.txt), data = jags.data5 ,n.chains=3,n.adapt=1000)
model6.jags <- jags.model(textConnection(model2.txt), data = jags.data6 ,n.chains=3,n.adapt=1000)
model7.jags <- jags.model(textConnection(model2.txt), data = jags.data7 ,n.chains=3,n.adapt=1000)

model1h.jags <- jags.model(textConnection(model2h.txt), data = jags.data1 ,n.chains=3,n.adapt=1000)
model2h.jags <- jags.model(textConnection(model2h.txt), data = jags.data2 ,n.chains=3,n.adapt=1000)
model3h.jags <- jags.model(textConnection(model2h.txt), data = jags.data3 ,n.chains=3,n.adapt=1000)
model4h.jags <- jags.model(textConnection(model2h.txt), data = jags.data4 ,n.chains=3,n.adapt=1000)
model5h.jags <- jags.model(textConnection(model2h.txt), data = jags.data5 ,n.chains=3,n.adapt=1000)
model6h.jags <- jags.model(textConnection(model2h.txt), data = jags.data6 ,n.chains=3,n.adapt=1000)
model7h.jags <- jags.model(textConnection(model2h.txt), data = jags.data7 ,n.chains=3,n.adapt=1000)

####-------------------------- Simulate MCMC Chain ------------------------------####
update(model1.jags,1000) 
update(model2.jags,1000) 
update(model3.jags,1000) 
update(model4.jags,1000) 
update(model5.jags,1000) 
update(model6.jags,1000) 
update(model7.jags,1000) 

update(model1h.jags,1000) 
update(model2h.jags,1000) 
update(model3h.jags,1000) 
update(model4h.jags,1000) 
update(model5h.jags,1000) 
update(model6h.jags,1000) 
update(model7h.jags,1000) 

### Assign parameter

params = c("mu")

### run coda.samples 

model1.MCMC <- coda.samples(model1.jags, params, n.iter=4000, thin=4)
model2.MCMC <- coda.samples(model2.jags, params, n.iter=4000, thin=4)
model3.MCMC <- coda.samples(model3.jags, params, n.iter=4000, thin=4)
model4.MCMC <- coda.samples(model4.jags, params, n.iter=4000, thin=4)
model5.MCMC <- coda.samples(model5.jags, params, n.iter=4000, thin=4)
model6.MCMC <- coda.samples(model6.jags, params, n.iter=4000, thin=4)
model7.MCMC <- coda.samples(model7.jags, params, n.iter=4000, thin=4)

model1h.MCMC <- coda.samples(model1h.jags, params, n.iter=4000, thin=4)
model2h.MCMC <- coda.samples(model2h.jags, params, n.iter=4000, thin=4)
model3h.MCMC <- coda.samples(model3h.jags, params, n.iter=4000, thin=4)
model4h.MCMC <- coda.samples(model4h.jags, params, n.iter=4000, thin=4)
model5h.MCMC <- coda.samples(model5h.jags, params, n.iter=4000, thin=4)
model6h.MCMC <- coda.samples(model6h.jags, params, n.iter=4000, thin=4)
model7h.MCMC <- coda.samples(model7h.jags, params, n.iter=4000, thin=4)

save(model1.MCMC,file="rdata/sim2/model1.MCMC.RData")
save(model2.MCMC,file="rdata/sim2/model2.MCMC.RData")
save(model3.MCMC,file="rdata/sim2/model3.MCMC.RData")
save(model4.MCMC,file="rdata/sim2/model4.MCMC.RData")
save(model5.MCMC,file="rdata/sim2/model5.MCMC.RData")
save(model6.MCMC,file="rdata/sim2/model6.MCMC.RData")
save(model7.MCMC,file="rdata/sim2/model7.MCMC.RData")

save(model1h.MCMC,file="rdata/sim2/model1h.MCMC.RData")
save(model2h.MCMC,file="rdata/sim2/model2h.MCMC.RData")
save(model3h.MCMC,file="rdata/sim2/model3h.MCMC.RData")
save(model4h.MCMC,file="rdata/sim2/model4h.MCMC.RData")
save(model5h.MCMC,file="rdata/sim2/model5h.MCMC.RData")
save(model6h.MCMC,file="rdata/sim2/model6h.MCMC.RData")
save(model7h.MCMC,file="rdata/sim2/model7h.MCMC.RData")

load("rdata/sim2/model1.MCMC.RData")
load("rdata/sim2/model2.MCMC.RData")
load("rdata/sim2/model3.MCMC.RData")
load("rdata/sim2/model4.MCMC.RData")
load("rdata/sim2/model5.MCMC.RData")
load("rdata/sim2/model6.MCMC.RData")
load("rdata/sim2/model7.MCMC.RData")

load("rdata/sim2/model1h.MCMC.RData")
load("rdata/sim2/model2h.MCMC.RData")
load("rdata/sim2/model3h.MCMC.RData")
load("rdata/sim2/model4h.MCMC.RData")
load("rdata/sim2/model5h.MCMC.RData")
load("rdata/sim2/model6h.MCMC.RData")
load("rdata/sim2/model7h.MCMC.RData")

#-------------------------------renaming and combine for plots ---------------------------------
n.chain = 3

total   = c("total")
brunch1 = c("A","B")
leaves  = c("AA","AB","BA","BB")

MyText<- c(total,brunch1,leaves)
MyText

modelnames <- c("Anomaly0","Anomaly10","Anomaly25","Anomaly50","Anomaly100","Anomaly250","Anomaly500")

# create a chain for total of all models

modelall.MCMC = model1.MCMC

for(i in 1:3){
  modelall.MCMC[[i]][,1] <- model1.MCMC[[i]][,1]
  modelall.MCMC[[i]][,2] <- model2.MCMC[[i]][,1]
  modelall.MCMC[[i]][,3] <- model3.MCMC[[i]][,1]
  modelall.MCMC[[i]][,4] <- model4.MCMC[[i]][,1]
  modelall.MCMC[[i]][,5] <- model5.MCMC[[i]][,1]
  modelall.MCMC[[i]][,6] <- model6.MCMC[[i]][,1]
  modelall.MCMC[[i]][,7] <- model7.MCMC[[i]][,1]
}

for (i in 1:n.chain) colnames(modelall.MCMC[[i]]) <- modelnames

modelA.MCMC = model1.MCMC

for(i in 1:3){
  modelA.MCMC[[i]][,1] <- model1.MCMC[[i]][,2]
  modelA.MCMC[[i]][,2] <- model2.MCMC[[i]][,2]
  modelA.MCMC[[i]][,3] <- model3.MCMC[[i]][,2]
  modelA.MCMC[[i]][,4] <- model4.MCMC[[i]][,2]
  modelA.MCMC[[i]][,5] <- model5.MCMC[[i]][,2]
  modelA.MCMC[[i]][,6] <- model6.MCMC[[i]][,2]
  modelA.MCMC[[i]][,7] <- model7.MCMC[[i]][,2]
}

for (i in 1:n.chain) colnames(modelA.MCMC[[i]]) <- modelnames

modelAA.MCMC = model1.MCMC

for(i in 1:3){
  modelAA.MCMC[[i]][,1] <- model1.MCMC[[i]][,4]
  modelAA.MCMC[[i]][,2] <- model2.MCMC[[i]][,4]
  modelAA.MCMC[[i]][,3] <- model3.MCMC[[i]][,4]
  modelAA.MCMC[[i]][,4] <- model4.MCMC[[i]][,4]
  modelAA.MCMC[[i]][,5] <- model5.MCMC[[i]][,4]
  modelAA.MCMC[[i]][,6] <- model6.MCMC[[i]][,4]
  modelAA.MCMC[[i]][,7] <- model7.MCMC[[i]][,4]
}

for (i in 1:n.chain) colnames(modelAA.MCMC[[i]]) <- modelnames

modelallh.MCMC = model1.MCMC

for(i in 1:3){
  modelallh.MCMC[[i]][,1] <- model1h.MCMC[[i]][,1]
  modelallh.MCMC[[i]][,2] <- model2h.MCMC[[i]][,1]
  modelallh.MCMC[[i]][,3] <- model3h.MCMC[[i]][,1]
  modelallh.MCMC[[i]][,4] <- model4h.MCMC[[i]][,1]
  modelallh.MCMC[[i]][,5] <- model5h.MCMC[[i]][,1]
  modelallh.MCMC[[i]][,6] <- model6h.MCMC[[i]][,1]
  modelallh.MCMC[[i]][,7] <- model7h.MCMC[[i]][,1]
}

for (i in 1:n.chain) colnames(modelallh.MCMC[[i]]) <- modelnames

modelAh.MCMC = model1.MCMC

for(i in 1:3){
  modelAh.MCMC[[i]][,1] <- model1h.MCMC[[i]][,2]
  modelAh.MCMC[[i]][,2] <- model2h.MCMC[[i]][,2]
  modelAh.MCMC[[i]][,3] <- model3h.MCMC[[i]][,2]
  modelAh.MCMC[[i]][,4] <- model4h.MCMC[[i]][,2]
  modelAh.MCMC[[i]][,5] <- model5h.MCMC[[i]][,2]
  modelAh.MCMC[[i]][,6] <- model6h.MCMC[[i]][,2]
  modelAh.MCMC[[i]][,7] <- model7h.MCMC[[i]][,2]
  
}

for (i in 1:n.chain) colnames(modelAh.MCMC[[i]]) <- modelnames

modelAAh.MCMC = model1.MCMC

for(i in 1:3){
  modelAAh.MCMC[[i]][,1] <- model1h.MCMC[[i]][,4]
  modelAAh.MCMC[[i]][,2] <- model2h.MCMC[[i]][,4]
  modelAAh.MCMC[[i]][,3] <- model3h.MCMC[[i]][,4]
  modelAAh.MCMC[[i]][,4] <- model4h.MCMC[[i]][,4]
  modelAAh.MCMC[[i]][,5] <- model5h.MCMC[[i]][,4]
  modelAAh.MCMC[[i]][,6] <- model6h.MCMC[[i]][,4]
  modelAAh.MCMC[[i]][,7] <- model7h.MCMC[[i]][,4]
  
}

for (i in 1:n.chain) colnames(modelAAh.MCMC[[i]]) <- modelnames

####================================================================================####

save(modelall.MCMC,file="rdata/sim2/modelall.MCMC.RData")
save(modelA.MCMC,file="rdata/sim2/modelA.MCMC.RData")
save(modelAA.MCMC,file="rdata/sim2/modelAA.MCMC.RData")

save(modelallh.MCMC,file="rdata/sim2/modelallh.MCMC.RData")
save(modelAh.MCMC,file="rdata/sim2/modelAh.MCMC.RData")
save(modelAAh.MCMC,file="rdata/sim2/modelAAh.MCMC.RData")

load("rdata/sim2/modelall.MCMC.RData")
load("rdata/sim2/modelA.MCMC.RData")
load("rdata/sim2/modelAA.MCMC.RData")

load("rdata/sim2/modelallh.MCMC.RData")
load("rdata/sim2/modelAh.MCMC.RData")
load("rdata/sim2/modelAAh.MCMC.RData")

#------------------------------- MCMC Summary ---------------------------------

### Posterior summary for total of each model

postsumtotal = MCMCsummary(modelall.MCMC, round = 5 , n.eff = T)
postsumA = MCMCsummary(modelA.MCMC,   round = 5 , n.eff = T)
postsumAA = MCMCsummary(modelAA.MCMC,  round = 5 , n.eff = T)

postsumtotalh = MCMCsummary(modelallh.MCMC, round = 5 , n.eff = T)
postsumAh = MCMCsummary(modelAh.MCMC,   round = 5 , n.eff = T)
postsumAAh = MCMCsummary(modelAAh.MCMC,  round = 5 , n.eff = T)

xtable(postsumtotal,digits = 4,type = "latex", file = "plots/sim2/MCMCsumatotal.tex")
xtable(postsumA ,digits = 4,type = "latex", file = "plots/sim2/MCMCsumbA.tex")
xtable(postsumAA,digits = 4,type = "latex", file = "plots/sim2/MCMCsumcAA.tex")
xtable(postsumtotalh,digits = 4,type = "latex", file = "plots/sim2/MCMCsumatotalh.tex")
xtable(postsumAh ,digits = 4,type = "latex", file = "plots/sim2/MCMCsumbAh.tex")
xtable(postsumAAh,digits = 4,type = "latex", file = "plots/sim2/MCMCsumcAAh.tex")

save(postsumtotal,file="rdata/sim2/postsumtotal.RData")
save(postsumA,file="rdata/sim2/postsumA.RData")
save(postsumAA,file="rdata/sim2/postsumAA.RData")
save(postsumtotalh,file="rdata/sim2/postsumtotalh.RData")
save(postsumAh,file="rdata/sim2/postsumAh.RData")
save(postsumAAh,file="rdata/sim2/postsumAAh.RData")

load("rdata/sim2/postsumtotal.RData")
load("rdata/sim2/postsumA.RData")
load("rdata/sim2/postsumAA.RData")
load("rdata/sim2/postsumtotalh.RData")
load("rdata/sim2/postsumAh.RData")
load("rdata/sim2/postsumAAh.RData")

#------------------------------- density plots  ---------------------------------

pdf("plots/sim2/Densitytotal.PDF")
densityplot(modelall.MCMC, main="Posterior density for category Total of all models", 
            strip=F, strip.left=strip.custom(factor.levels=modelnames ,bg="skyblue"))
dev.off()

pdf("plots/sim2/DensityA.PDF")
densityplot(modelA.MCMC, main="Posterior density for category A of all models", 
            strip=F, strip.left=strip.custom(factor.levels=modelnames ,bg="skyblue"))
dev.off()

pdf("plots/sim2/DensityAA.PDF")
densityplot(modelAA.MCMC, main="Posterior density for category AA of all models", 
            strip=F, strip.left=strip.custom(factor.levels=modelnames ,bg="skyblue"))
dev.off()

pdf("plots/sim2/Densitytotalh.PDF")
densityplot(modelallh.MCMC, main="Posterior density for category Total of all models", 
            strip=F, strip.left=strip.custom(factor.levels=modelnames ,bg="skyblue"))
dev.off()

pdf("plots/sim2/DensityAh.PDF")
densityplot(modelAh.MCMC, main="Posterior density for category A of all models", 
            strip=F, strip.left=strip.custom(factor.levels=modelnames ,bg="skyblue"))
dev.off()

pdf("plots/sim2/DensityAAh.PDF")
densityplot(modelAAh.MCMC, main="Posterior density for category AA of all models", 
            strip=F, strip.left=strip.custom(factor.levels=modelnames ,bg="skyblue"))
dev.off()

#------------------------------- caterpillar plot  ---------------------------------

pdf("plots/sim2/Catertotal.PDF")
par(mfrow=c(1,1))
caterplot(modelall.MCMC,labels.loc = "above",labels = rev(modelnames) ,style = "plain")
title( main = "Caterpillar plot for category Total of all models" , xlab ="Parameter estimate")
dev.off()

pdf("plots/sim2/CaterA.PDF")
par(mfrow=c(1,1))
caterplot(modelA.MCMC,labels.loc = "above",labels = rev(modelnames) ,style = "plain")
title( main = "Caterpillar plot for category A of all models" , xlab ="Parameter estimate")
dev.off()

pdf("plots/sim2/CaterAA.PDF")
par(mfrow=c(1,1))
caterplot(modelAA.MCMC,labels.loc = "above",labels = rev(modelnames) ,style = "plain")
title( main = "Caterpillar plot for category AA of all models" , xlab ="Parameter estimate")
dev.off()

pdf("plots/sim2/Catertotalh.PDF")
par(mfrow=c(1,1))
caterplot(modelallh.MCMC,labels.loc = "above",labels = modelnames ,style = "plain")
title( main = "Caterpillar plot for category Total of all models" , xlab ="Parameter estimate")
dev.off()

pdf("plots/sim2/CaterAh.PDF")
par(mfrow=c(1,1))
caterplot(modelAh.MCMC,labels.loc = "above",labels = modelnames ,style = "plain")
title( main = "Caterpillar plot for category A of all models" , xlab ="Parameter estimate")
dev.off()

pdf("plots/sim2/CaterAAh.PDF")
par(mfrow=c(1,1))
caterplot(modelAAh.MCMC,labels.loc = "above",labels = modelnames ,style = "plain")
title( main = "Caterpillar plot for category AA of all models" , xlab ="Parameter estimate")
dev.off()


#------------------------------- post theta plot  ---------------------------------

theta1 = posttheta2 (originalrho = rho, MCMC = modelall.MCMC, k = 1/1.25)
theta1

theta2 = posttheta2 (originalrho = rho, MCMC = modelA.MCMC, k = 1/1.25)
theta2

theta3 = posttheta2 (originalrho = rho, MCMC = modelAA.MCMC, k = 1/1.25)
theta3

