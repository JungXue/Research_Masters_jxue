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

set.seed(1234567)

####------------------------------- Import Data --------------------------------------####

setwd("H:/Desktop/7.1 No time to waste/Stats 798A Research Master/R-codes/MIMIC")


load("data/mimic3.daily.df.RData")
load("data/mimic3.monthly.df.RData")
load("data/mimic3.yearly.df.RData")
load("data/mimic3.quarterly.df.RData")

load("data/mimic3.daily.ts.RData")
load("data/mimic3.monthly.ts.RData")
load("data/mimic3.yearly.ts.RData")
load("data/mimic3.quarterly.ts.RData")
        
load("data/mimic3.daily.hts.RData")
load("data/mimic3.monthly.hts.RData")
load("data/mimic3.yearly.hts.RData")
load("data/mimic3.quarterly.hts.RData")  

# use "2190-10-15" as endpoint

###---------- hts optimum reconciliation estimation-------------------

load("data/forecast.list.RData")
load('data/rho.RData')

####---------------------------- Assign model -------------------------------------####

# 3307 410 Acute myocardial infarction    everyday
#  373 415 Acute pulmonary heart disease  once/month
#   12 452 Portal vein thrombosis         once/year

Anomaly1 = mimic3.yearly.df[91,387]  # 410 Acute myocardial infarction
Anomaly2 = mimic3.yearly.df[91,391]  # 415, Acute pulmonary heart disease
Anomaly3 = mimic3.yearly.df[91,422]  # 452 Portal vein thrombosis

mimic3.yearly.abn1.df = mimic3.yearly.df
mimic3.yearly.abn2.df = mimic3.yearly.df
mimic3.yearly.abn3.df = mimic3.yearly.df

mimic3.yearly.abn1.df[91,c(2,9,73,387)] <- mimic3.yearly.df[91,c(2,9,73,387)] + Anomaly1
mimic3.yearly.abn2.df[91,c(2,9,74,391)] <- mimic3.yearly.df[91,c(2,9,74,391)] + Anomaly2
mimic3.yearly.abn3.df[91,c(2,9,78,422)] <- mimic3.yearly.df[91,c(2,9,78,422)] + Anomaly3 + 1

Y  = mimic3.yearly.df[91,-1]
Y1 = mimic3.yearly.abn1.df[91,-1]
Y2 = mimic3.yearly.abn2.df[91,-1]
Y3 = mimic3.yearly.abn3.df[91,-1]

Y [c(2,9,73,74,78,387,391,422)-1]
Y1[c(2,9,73,74,78,387,391,422)-1]
Y2[c(2,9,73,74,78,387,391,422)-1]
Y3[c(2,9,73,74,78,387,391,422)-1]

sum(Y)  == sum(Y)
sum(Y1) == sum(Y) + 4*Anomaly1
sum(Y2) == sum(Y) + 4*Anomaly2
sum(Y3) == sum(Y) + 4*(Anomaly3+1)

Nday  <- nrow(Y)
Nleaf <- ncol(Y)

rhoyear   <- rho$yearly_bu
rhoyearh  <- rho$yearly_comb
  
Nlv1 <- 17
Nlv2 <- 132
Nlv3 <- 631
lV1b <- mimic3.daily.hts$nodes$`Level 2`
lV2b <- mimic3.daily.hts$nodes$`Level 3`


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

jags.data1h <- list(Y=Y1,Nday=Nday,Nleaf=Nleaf,rho=rho,Nlv1=Nlv1,Nlv2=Nlv2,Nlv3=Nlv3,lV1b=lV1b,lV2b=lV2b) 
jags.data2h <- list(Y=Y2,Nday=Nday,Nleaf=Nleaf,rho=rho,Nlv1=Nlv1,Nlv2=Nlv2,Nlv3=Nlv3,lV1b=lV1b,lV2b=lV2b) 
jags.data3h <- list(Y=Y3,Nday=Nday,Nleaf=Nleaf,rho=rho,Nlv1=Nlv1,Nlv2=Nlv2,Nlv3=Nlv3,lV1b=lV1b,lV2b=lV2b) 
jags.data4h <- list(Y=Y4,Nday=Nday,Nleaf=Nleaf,rho=rho,Nlv1=Nlv1,Nlv2=Nlv2,Nlv3=Nlv3,lV1b=lV1b,lV2b=lV2b) 
jags.data5h <- list(Y=Y5,Nday=Nday,Nleaf=Nleaf,rho=rho,Nlv1=Nlv1,Nlv2=Nlv2,Nlv3=Nlv3,lV1b=lV1b,lV2b=lV2b) 
jags.data6h <- list(Y=Y6,Nday=Nday,Nleaf=Nleaf,rho=rho,Nlv1=Nlv1,Nlv2=Nlv2,Nlv3=Nlv3,lV1b=lV1b,lV2b=lV2b) 
jags.data7h <- list(Y=Y7,Nday=Nday,Nleaf=Nleaf,rho=rho,Nlv1=Nlv1,Nlv2=Nlv2,Nlv3=Nlv3,lV1b=lV1b,lV2b=lV2b) 

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

####-------------------------- Simulate MCMC Chain ------------------------------####