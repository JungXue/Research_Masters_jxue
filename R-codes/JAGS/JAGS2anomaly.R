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

load("rdata/anomaly/daily1.RData")
load("rdata/anomaly/daily1.S10.RData")
load("rdata/anomaly/daily1.S25.RData")
load("rdata/anomaly/daily1.S50.RData")
load("rdata/anomaly/daily1.S100.RData")
load("rdata/anomaly/daily1.S250.RData")
load("rdata/anomaly/daily1.S500.RData")


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
daily.df <- daily1.df

rho.df = rhodata(rawdata = raw1.df, 
                 dailydata = daily1.df, 
                 method = "mean",
               data ="simulation", 
                outputdata="rdata/sim1/rho.RData")

load("rdata/sim1/rho.RData")

head(daily1.df)
head(daily1.S25.df)
head(rho.df)

anomalydateloc = which(daily1.df$day == "2015-11-15")

daily1.df[anomalydateloc,]
daily1.S10.df[anomalydateloc,]
daily1.S25.df[anomalydateloc,]
daily1.S50.df[anomalydateloc,]
daily1.S100.df[anomalydateloc,]
daily1.S250.df[anomalydateloc,]
daily1.S500.df[anomalydateloc,]


####---------------------------- Assign model -------------------------------------####
### Assign model values

Y1 <- as.matrix(daily1.df[anomalydateloc,2:length(daily1.df)])
Y2 <- as.matrix(daily1.S10.df[anomalydateloc,2:length(daily1.df)])
Y3 <- as.matrix(daily1.S25.df[anomalydateloc,2:length(daily1.df)])
Y4 <- as.matrix(daily1.S50.df[anomalydateloc,2:length(daily1.df)])
Y5 <- as.matrix(daily1.S100.df[anomalydateloc,2:length(daily1.df)])
Y6 <- as.matrix(daily1.S250.df[anomalydateloc,2:length(daily1.df)])
Y7 <- as.matrix(daily1.S500.df[anomalydateloc,2:length(daily1.df)])

Nday  <- nrow(Y1)
Nleaf <- ncol(Y1)
rho   <- as.matrix(rho.df[,2:length(rho.df)])[1,]

Nlv1 = 2
Nlv2 = 4
lV1b = rep(1:2,c(2,2))


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

jags.data1h <- list(Y=Y1,Nday=Nday,Nleaf=Nleaf,rho=rho,Nlv1=Nlv1,Nlv2=Nlv2,lV1b=lV1b) 
jags.data2h <- list(Y=Y2,Nday=Nday,Nleaf=Nleaf,rho=rho,Nlv1=Nlv1,Nlv2=Nlv2,lV1b=lV1b) 
jags.data3h <- list(Y=Y3,Nday=Nday,Nleaf=Nleaf,rho=rho,Nlv1=Nlv1,Nlv2=Nlv2,lV1b=lV1b) 
jags.data4h <- list(Y=Y4,Nday=Nday,Nleaf=Nleaf,rho=rho,Nlv1=Nlv1,Nlv2=Nlv2,lV1b=lV1b) 
jags.data5h <- list(Y=Y5,Nday=Nday,Nleaf=Nleaf,rho=rho,Nlv1=Nlv1,Nlv2=Nlv2,lV1b=lV1b) 
jags.data6h <- list(Y=Y6,Nday=Nday,Nleaf=Nleaf,rho=rho,Nlv1=Nlv1,Nlv2=Nlv2,lV1b=lV1b) 
jags.data7h <- list(Y=Y7,Nday=Nday,Nleaf=Nleaf,rho=rho,Nlv1=Nlv1,Nlv2=Nlv2,lV1b=lV1b) 

model1.jags <- jags.model(textConnection(model2.txt), data = jags.data1 ,n.chains=3,n.adapt=1000)
model2.jags <- jags.model(textConnection(model2.txt), data = jags.data2 ,n.chains=3,n.adapt=1000)
model3.jags <- jags.model(textConnection(model2.txt), data = jags.data3 ,n.chains=3,n.adapt=1000)
model4.jags <- jags.model(textConnection(model2.txt), data = jags.data4 ,n.chains=3,n.adapt=1000)
model5.jags <- jags.model(textConnection(model2.txt), data = jags.data5 ,n.chains=3,n.adapt=1000)
model6.jags <- jags.model(textConnection(model2.txt), data = jags.data6 ,n.chains=3,n.adapt=1000)
model7.jags <- jags.model(textConnection(model2.txt), data = jags.data7 ,n.chains=3,n.adapt=1000)

model1h.jags <- jags.model(textConnection(model2h.txt), data = jags.data1h ,n.chains=3,n.adapt=1000)
model2h.jags <- jags.model(textConnection(model2h.txt), data = jags.data2h ,n.chains=3,n.adapt=1000)
model3h.jags <- jags.model(textConnection(model2h.txt), data = jags.data3h ,n.chains=3,n.adapt=1000)
model4h.jags <- jags.model(textConnection(model2h.txt), data = jags.data4h ,n.chains=3,n.adapt=1000)
model5h.jags <- jags.model(textConnection(model2h.txt), data = jags.data5h ,n.chains=3,n.adapt=1000)
model6h.jags <- jags.model(textConnection(model2h.txt), data = jags.data6h ,n.chains=3,n.adapt=1000)
model7h.jags <- jags.model(textConnection(model2h.txt), data = jags.data7h ,n.chains=3,n.adapt=1000)

####---------------------------- calculate DIC-------------------------------------####

### dic comaprison  # Note 1000 sample did not give stable dic, try 100000

dic.mod1 <- dic.samples(model1.jags, 1000, "pD")
dic.mod2 <- dic.samples(model2.jags, 1000, "pD")
dic.mod3 <- dic.samples(model3.jags, 1000, "pD")
dic.mod4 <- dic.samples(model4.jags, 1000, "pD")
dic.mod5 <- dic.samples(model5.jags, 1000, "pD")
dic.mod6 <- dic.samples(model6.jags, 1000, "pD")
dic.mod7 <- dic.samples(model7.jags, 1000, "pD")

dic.mod1h <- dic.samples(model1h.jags, 1000, "pD")
dic.mod2h <- dic.samples(model2h.jags, 1000, "pD")
dic.mod3h <- dic.samples(model3h.jags, 1000, "pD")
dic.mod4h <- dic.samples(model4h.jags, 1000, "pD")
dic.mod5h <- dic.samples(model5h.jags, 1000, "pD")
dic.mod6h <- dic.samples(model6h.jags, 1000, "pD")
dic.mod7h <- dic.samples(model7h.jags, 1000, "pD")

save(dic.mod1,file="rdata/sim1/dic.3abn.mod1.RData")
save(dic.mod2,file="rdata/sim1/dic.3abn.mod2.RData")
save(dic.mod3,file="rdata/sim1/dic.3abn.mod3.RData")
save(dic.mod4,file="rdata/sim1/dic.3abn.mod4.RData")
save(dic.mod5,file="rdata/sim1/dic.3abn.mod5.RData")
save(dic.mod6,file="rdata/sim1/dic.3abn.mod6.RData")
save(dic.mod7,file="rdata/sim1/dic.3abn.mod7.RData")

save(dic.mod1h,file="rdata/sim1/dic.3abn.mod1h.RData")
save(dic.mod2h,file="rdata/sim1/dic.3abn.mod2h.RData")
save(dic.mod3h,file="rdata/sim1/dic.3abn.mod3h.RData")
save(dic.mod4h,file="rdata/sim1/dic.3abn.mod4h.RData")
save(dic.mod5h,file="rdata/sim1/dic.3abn.mod5h.RData")
save(dic.mod6h,file="rdata/sim1/dic.3abn.mod6h.RData")
save(dic.mod7h,file="rdata/sim1/dic.3abn.mod7h.RData")

load("rdata/sim1/dic.3abn.mod1.RData")
load("rdata/sim1/dic.3abn.mod2.RData")
load("rdata/sim1/dic.3abn.mod3.RData")
load("rdata/sim1/dic.3abn.mod4.RData")
load("rdata/sim1/dic.3abn.mod5.RData")
load("rdata/sim1/dic.3abn.mod6.RData")
load("rdata/sim1/dic.3abn.mod7.RData")

load("rdata/sim1/dic.3abn.mod1h.RData")
load("rdata/sim1/dic.3abn.mod2h.RData")
load("rdata/sim1/dic.3abn.mod3h.RData")
load("rdata/sim1/dic.3abn.mod4h.RData")
load("rdata/sim1/dic.3abn.mod5h.RData")
load("rdata/sim1/dic.3abn.mod6h.RData")
load("rdata/sim1/dic.3abn.mod7h.RData")

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

save(model1.MCMC,file="rdata/sim1/model1.MCMC.RData")
save(model2.MCMC,file="rdata/sim1/model2.MCMC.RData")
save(model3.MCMC,file="rdata/sim1/model3.MCMC.RData")
save(model4.MCMC,file="rdata/sim1/model4.MCMC.RData")
save(model5.MCMC,file="rdata/sim1/model5.MCMC.RData")
save(model6.MCMC,file="rdata/sim1/model6.MCMC.RData")
save(model7.MCMC,file="rdata/sim1/model7.MCMC.RData")

save(model1h.MCMC,file="rdata/sim1/model1h.MCMC.RData")
save(model2h.MCMC,file="rdata/sim1/model2h.MCMC.RData")
save(model3h.MCMC,file="rdata/sim1/model3h.MCMC.RData")
save(model4h.MCMC,file="rdata/sim1/model4h.MCMC.RData")
save(model5h.MCMC,file="rdata/sim1/model5h.MCMC.RData")
save(model6h.MCMC,file="rdata/sim1/model6h.MCMC.RData")
save(model7h.MCMC,file="rdata/sim1/model7h.MCMC.RData")

load("rdata/sim1/model1.MCMC.RData")
load("rdata/sim1/model2.MCMC.RData")
load("rdata/sim1/model3.MCMC.RData")
load("rdata/sim1/model4.MCMC.RData")
load("rdata/sim1/model5.MCMC.RData")
load("rdata/sim1/model6.MCMC.RData")
load("rdata/sim1/model7.MCMC.RData")

load("rdata/sim1/model1h.MCMC.RData")
load("rdata/sim1/model2h.MCMC.RData")
load("rdata/sim1/model3h.MCMC.RData")
load("rdata/sim1/model4h.MCMC.RData")
load("rdata/sim1/model5h.MCMC.RData")
load("rdata/sim1/model6h.MCMC.RData")
load("rdata/sim1/model7h.MCMC.RData")

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

save(modelall.MCMC,file="rdata/sim1/modelall.MCMC.RData")
save(modelA.MCMC,file="rdata/sim1/modelA.MCMC.RData")
save(modelAA.MCMC,file="rdata/sim1/modelAA.MCMC.RData")

save(modelallh.MCMC,file="rdata/sim1/modelallh.MCMC.RData")
save(modelAh.MCMC,file="rdata/sim1/modelAh.MCMC.RData")
save(modelAAh.MCMC,file="rdata/sim1/modelAAh.MCMC.RData")

load("rdata/sim1/modelall.MCMC.RData")
load("rdata/sim1/modelA.MCMC.RData")
load("rdata/sim1/modelAA.MCMC.RData")

load("rdata/sim1/modelallh.MCMC.RData")
load("rdata/sim1/modelAh.MCMC.RData")
load("rdata/sim1/modelAAh.MCMC.RData")

#----------------------------------DIC -----------------------------------------

M1 = summary(diffdic(dic.mod1, dic.mod1h))
M2 = summary(diffdic(dic.mod2, dic.mod2h))
M3 = summary(diffdic(dic.mod3, dic.mod3h))
M4 = summary(diffdic(dic.mod4, dic.mod4h))
M5 = summary(diffdic(dic.mod5, dic.mod5h))
M6 = summary(diffdic(dic.mod6, dic.mod6h))
M7 = summary(diffdic(dic.mod7, dic.mod7h))

compare_model = rbind(M1,M2,M3,M4,M5,M6,M7)

row.names(compare_model) = c("0","10","25","50","100","250","500")

compare_model

xtable(compare_model ,digits = 3, type = "latex", file = "plots/findmodel/dictable.3abn.tex",
       caption = "DIC comapreisons for different anomalies sizes", label = "tab:dicanomaly")


dictablesim1()

#------------------------------- MCMC Summary ---------------------------------

### Posterior summary for total of each model

postsumtotal = MCMCsummary(modelall.MCMC, round = 5 , n.eff = T)
postsumA = MCMCsummary(modelA.MCMC,   round = 5 , n.eff = T)
postsumAA = MCMCsummary(modelAA.MCMC,  round = 5 , n.eff = T)

postsumtotalh = MCMCsummary(modelallh.MCMC, round = 5 , n.eff = T)
postsumAh = MCMCsummary(modelAh.MCMC,   round = 5 , n.eff = T)
postsumAAh = MCMCsummary(modelAAh.MCMC,  round = 5 , n.eff = T)

cap1 = "Posterior distributions of different models for Total, with different increments of added anomalies, and calculated with independent Bayes model"
cap2 = "Posterior distributions of different models for A , with different increments of added anomalies, and calculated with independent Bayes model"
cap3 = "Posterior distributions of different models for AA, with different increments of added anomalies, and calculated with independent Bayes model"

cap4 = "Posterior distributions of different modelsfor Total, with different increments of added anomalies, and calculated with Hierarchiacl Bayes model"
cap5 = "Posterior distributions of different modelsfor A , with different increments of added anomalies, and calculated with Hierarchiacl Bayes model"
cap6 = "Posterior distributions of different modelsfor AA, with different increments of added anomalies, and calculated with Hierarchiacl Bayes model"


xtable(postsumtotal,digits = 4,type = "latex", file = "plots/sim1/MCMCsumatotal.tex",caption = cap1,label = "pstanototal")
xtable(postsumA ,digits = 4,type = "latex", file = "plots/sim1/MCMCsumbA.tex",caption = cap2,label = "pstanoA")
xtable(postsumAA,digits = 4,type = "latex", file = "plots/sim1/MCMCsumcAA.tex",caption = cap3,label = "pstanoAA")
xtable(postsumtotalh,digits = 4,type = "latex", file = "plots/sim1/MCMCsumatotalh.tex",caption = cap4,label = "pstanototal")
xtable(postsumAh ,digits = 4,type = "latex", file = "plots/sim1/MCMCsumbAh.tex",caption = cap5,label = "pstanoA")
xtable(postsumAAh,digits = 4,type = "latex", file = "plots/sim1/MCMCsumcAAh.tex",caption = cap6,label = "pstanoAA")

save(postsumtotal,file="rdata/sim1/postsumtotal.RData")
save(postsumA,file="rdata/sim1/postsumA.RData")
save(postsumAA,file="rdata/sim1/postsumAA.RData")
save(postsumtotalh,file="rdata/sim1/postsumtotalh.RData")
save(postsumAh,file="rdata/sim1/postsumAh.RData")
save(postsumAAh,file="rdata/sim1/postsumAAh.RData")

load("rdata/sim1/postsumtotal.RData")
load("rdata/sim1/postsumA.RData")
load("rdata/sim1/postsumAA.RData")
load("rdata/sim1/postsumtotalh.RData")
load("rdata/sim1/postsumAh.RData")
load("rdata/sim1/postsumAAh.RData")

#------------------------------- density plots  ---------------------------------

pdf("plots/sim1/Densitytotal.PDF",width=12, height=8)
densityplot(modelall.MCMC, strip=F, strip.left=strip.custom(factor.levels=modelnames ,bg="skyblue"))
dev.off()

pdf("plots/sim1/DensityA.PDF",width=12, height=8)
densityplot(modelA.MCMC, strip=F, strip.left=strip.custom(factor.levels=modelnames ,bg="skyblue"))
dev.off()

pdf("plots/sim1/DensityAA.PDF",width=12, height=8)
densityplot(modelAA.MCMC, strip=F, strip.left=strip.custom(factor.levels=modelnames ,bg="skyblue"))
dev.off()

pdf("plots/sim1/Densitytotalh.PDF",width=12, height=8)
densityplot(modelallh.MCMC,strip=F, strip.left=strip.custom(factor.levels=modelnames ,bg="skyblue"))
dev.off()

pdf("plots/sim1/DensityAh.PDF",width=12, height=8)
densityplot(modelAh.MCMC,strip=F, strip.left=strip.custom(factor.levels=modelnames ,bg="skyblue"))
dev.off()

pdf("plots/sim1/DensityAAh.PDF",width=12, height=8)
densityplot(modelAAh.MCMC,strip=F, strip.left=strip.custom(factor.levels=modelnames ,bg="skyblue"))
dev.off()

#------------------------------- caterpillar plot  ---------------------------------

pdf("plots/sim1/Catertotal.PDF")
par(mfrow=c(1,1))
caterplot(modelall.MCMC,labels.loc = "above",labels = rev(modelnames) ,style = "plain")
title( main = "Caterpillar plot for category Total of all models" , xlab ="Parameter estimate")
dev.off()

pdf("plots/sim1/CaterA.PDF")
par(mfrow=c(1,1))
caterplot(modelA.MCMC,labels.loc = "above",labels = rev(modelnames) ,style = "plain")
title( main = "Caterpillar plot for category A of all models" , xlab ="Parameter estimate")
dev.off()

pdf("plots/sim1/CaterAA.PDF")
par(mfrow=c(1,1))
caterplot(modelAA.MCMC,labels.loc = "above",labels = rev(modelnames) ,style = "plain")
title( main = "Caterpillar plot for category AA of all models" , xlab ="Parameter estimate")
dev.off()

pdf("plots/sim1/Catertotalh.PDF")
par(mfrow=c(1,1))
caterplot(modelallh.MCMC,labels.loc = "above",labels = modelnames ,style = "plain")
title( main = "Caterpillar plot for category Total of all models" , xlab ="Parameter estimate")
dev.off()

pdf("plots/sim1/CaterAh.PDF")
par(mfrow=c(1,1))
caterplot(modelAh.MCMC,labels.loc = "above",labels = modelnames ,style = "plain")
title( main = "Caterpillar plot for category A of all models" , xlab ="Parameter estimate")
dev.off()

pdf("plots/sim1/CaterAAh.PDF")
par(mfrow=c(1,1))
caterplot(modelAAh.MCMC,labels.loc = "above",labels = modelnames ,style = "plain")
title( main = "Caterpillar plot for category AA of all models" , xlab ="Parameter estimate")
dev.off()

#------------------------------- trace plots  ---------------------------------

pdf("plots/sim1/Tracetotalabn.PDF",width=12, height=6)
xyplot(modelall.MCMC,strip=F, strip.left=strip.custom(factor.levels=modelnames ,bg="skyblue"))
dev.off()

pdf("plots/sim1/TraceAabn.PDF",width=12, height=6)
xyplot(modelA.MCMC, strip=F, strip.left=strip.custom(factor.levels=modelnames ,bg="skyblue"))
dev.off()

pdf("plots/sim1/TraceAAabn.PDF",width=12, height=6)
xyplot(modelAA.MCMC, strip=F, strip.left=strip.custom(factor.levels=modelnames ,bg="skyblue"))
dev.off()



#------------------------------- post theta plot  ---------------------------------

setwd("H:/Desktop/7.1 No time to waste/Stats 798A Research Master/R-codes/JAGS")
source("16-posttheta.R")
library(RColorBrewer)
postthetampic (rho = rho,
               rhoh = rho ,
               MCMC = modelall.MCMC,
               MCMCh = modelallh.MCMC,
               k = seq(1,0.05,by=-0.10),
               output ="plots/sim1/heattotal.PNG")


postthetampic (rho = rho,
               rhoh = rho ,
               MCMC = modelA.MCMC,
               MCMCh = modelAh.MCMC,
               k = seq(1,0.05,by=-0.10),
               output ="plots/sim1/heatA.PNG")

postthetampic (rho = rho,
               rhoh = rho ,
               MCMC = modelAA.MCMC,
               MCMCh = modelAAh.MCMC,
               k = seq(1,0.05,by=-0.10),
               output ="plots/sim1/heatAA.PNG")
#---------------------------------------
postthetampic (rho = rho,
               rhoh = rho ,
               MCMC = modelall.MCMC,
               MCMCh = modelallh.MCMC,
               k = seq(1,0.6,by=-0.02),
               output ="plots/sim1/heattotal2.PNG")


postthetampic (rho = rho,
               rhoh = rho ,
               MCMC = modelA.MCMC,
               MCMCh = modelAh.MCMC,
               k = seq(1,0.6,by=-0.02),
               output ="plots/sim1/heatA2.PNG")

postthetampic (rho = rho,
               rhoh = rho ,
               MCMC = modelAA.MCMC,
               MCMCh = modelAAh.MCMC,
               k = seq(1,0.6,by=-0.02),
               output ="plots/sim1/heatAA2.PNG")








