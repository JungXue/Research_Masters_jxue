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

load("rdata/proportion/rawp1.RData")
load("rdata/proportion/rawp2.RData")
load("rdata/proportion/rawp3.RData")
load("rdata/proportion/rawp4.RData")
load("rdata/proportion/rawp5.RData")
load("rdata/proportion/rawp6.RData")
load("rdata/proportion/rawp7.RData")
load("rdata/proportion/rawp8.RData")
load("rdata/proportion/rawp9.RData")
load("rdata/proportion/rawp10.RData")
load("rdata/proportion/rawp11.RData")
load("rdata/proportion/rawp12.RData")

load("rdata/proportion/dailyp1a1.RData")
load("rdata/proportion/dailyp2a1.RData")
load("rdata/proportion/dailyp3a1.RData")
load("rdata/proportion/dailyp4a1.RData")
load("rdata/proportion/dailyp5a1.RData")
load("rdata/proportion/dailyp6a1.RData")
load("rdata/proportion/dailyp7a1.RData")
load("rdata/proportion/dailyp8a1.RData")
load("rdata/proportion/dailyp9a1.RData")
load("rdata/proportion/dailyp10a1.RData")
load("rdata/proportion/dailyp11a1.RData")
load("rdata/proportion/dailyp12a1.RData")

setwd("H:/Desktop/7.1 No time to waste/Stats 798A Research Master/R-codes/JAGS")

### Note: adte of anomaly = "2015-11-15"

n.cat1 = 2
N = nrow(dailyp1a1.df)
anomalydate = "2015-11-15"

raw.df <- rawp1.df
daily.df <- dailyp1a1.df
n.leaf = 3

rho1.df = rhodata(rawdata = raw.df, 
                 dailydata = daily.df, 
                 method = "mean",
                 data ="simulation", 
                 outputdata="rdata/sim2/rho1.RData")

raw.df <- rawp2.df
daily.df <- dailyp2a1.df
n.leaf = 4

rho2.df = rhodata(rawdata = raw.df, 
                 dailydata = daily.df, 
                 method = "mean",
                 data ="simulation", 
                 outputdata="rdata/sim2/rho2.RData")

raw.df <- rawp3.df
daily.df <- dailyp3a1.df
n.leaf = 5
rho3.df = rhodata(rawdata = raw.df, 
                 dailydata = daily.df, 
                 method = "mean",
                 data ="simulation", 
                 outputdata="rdata/sim2/rho3.RData")

raw.df <- rawp4.df
daily.df <- dailyp4a1.df
n.leaf = 6
rho4.df = rhodata(rawdata = raw.df, 
                 dailydata = daily.df, 
                 method = "mean",
                 data ="simulation", 
                 outputdata="rdata/sim2/rho4.RData")

raw.df <- rawp5.df
daily.df <- dailyp5a1.df
n.leaf = 7
rho5.df = rhodata(rawdata = raw.df, 
                 dailydata = daily.df, 
                 method = "mean",
                 data ="simulation", 
                 outputdata="rdata/sim2/rho5.RData")

raw.df <- rawp6.df
daily.df <- dailyp6a1.df
n.leaf = 8
rho6.df = rhodata(rawdata = raw.df, 
                 dailydata = daily.df, 
                 method = "mean",
                 data ="simulation", 
                 outputdata="rdata/sim2/rho6.RData")

raw.df <- rawp7.df
daily.df <- dailyp7a1.df
n.leaf = 9
rho7.df = rhodata(rawdata = raw.df, 
                 dailydata = daily.df, 
                 method = "mean",
                 data ="simulation", 
                 outputdata="rdata/sim2/rho7.RData")

raw.df <- rawp8.df
daily.df <- dailyp8a1.df
n.leaf = 10
rho8.df = rhodata(rawdata = raw.df, 
                 dailydata = daily.df, 
                 method = "mean",
                 data ="simulation", 
                 outputdata="rdata/sim2/rho8.RData")

raw.df <- rawp9.df
daily.df <- dailyp9a1.df
n.leaf = 11
rho9.df = rhodata(rawdata = raw.df, 
                 dailydata = daily.df, 
                 method = "mean",
                 data ="simulation", 
                 outputdata="rdata/sim2/rho9.RData")

raw.df <- rawp10.df
daily.df <- dailyp10a1.df
n.leaf = 12
rho10.df = rhodata(rawdata = raw.df, 
                 dailydata = daily.df, 
                 method = "mean",
                 data ="simulation", 
                 outputdata="rdata/sim2/rho10.RData")

raw.df <- rawp11.df
daily.df <- dailyp11a1.df
n.leaf = 13
rho11.df = rhodata(rawdata = raw.df, 
                   dailydata = daily.df, 
                   method = "mean",
                   data ="simulation", 
                   outputdata="rdata/sim2/rho11.RData")


raw.df <- rawp12.df
daily.df <- dailyp12a1.df
n.leaf = 14

rho12.df = rhodata(rawdata = raw.df, 
                   dailydata = daily.df, 
                   method = "mean",
                   data ="simulation", 
                   outputdata="rdata/sim2/rho12.RData")

setwd("H:/Desktop/7.1 No time to waste/Stats 798A Research Master/R-codes/JAGS")
load("rdata/sim2/rho1.RData")
load("rdata/sim2/rho2.RData")
load("rdata/sim2/rho3.RData")
load("rdata/sim2/rho4.RData")
load("rdata/sim2/rho5.RData")
load("rdata/sim2/rho6.RData")
load("rdata/sim2/rho7.RData")
load("rdata/sim2/rho8.RData")
load("rdata/sim2/rho9.RData")
load("rdata/sim2/rho10.RData")
load("rdata/sim2/rho11.RData")
load("rdata/sim2/rho12.RData")






rho1.df[1,]
rho2.df[1,]
rho3.df[1,]
rho4.df[1,]
rho5.df[1,]
rho6.df[1,]
rho7.df[1,]
rho8.df[1,]
rho9.df[1,]
rho10.df[1,]
rho11.df[1,]
rho12.df[1,]

anomalydateloc = which(dailyp12a1.df$day == "2015-11-15")

dailyp1a1.df[anomalydateloc,]
dailyp2a1.df[anomalydateloc,]
dailyp3a1.df[anomalydateloc,]
dailyp4a1.df[anomalydateloc,]
dailyp5a1.df[anomalydateloc,]
dailyp6a1.df[anomalydateloc,]
dailyp7a1.df[anomalydateloc,]
dailyp8a1.df[anomalydateloc,]
dailyp9a1.df[anomalydateloc,]
dailyp10a1.df[anomalydateloc,]
dailyp11a1.df[anomalydateloc,]
dailyp12a1.df[anomalydateloc,]


####---------------------------- Assign model -------------------------------------####
### Assign model values
rho1.df[3:4] = 10.53896
rho2.df[3:4] = 10.53896
rho3.df[3:4] = 10.53896
rho4.df[3:4] = 10.53896
rho5.df[3:4] = 10.53896
rho6.df[3:4] = 10.53896
rho7.df[3:4] = 10.53896
rho8.df[3:4] = 10.53896
rho9.df[3:4] = 10.53896
rho10.df[3:4] = 10.53896
rho11.df[3:4] = 10.53896
rho12.df[3:4] = 10.53896

rho1.df[2] = 21.06192
rho2.df[2] = 21.06192
rho3.df[2] = 21.06192
rho4.df[2] = 21.06192
rho5.df[2] = 21.06192
rho6.df[2] = 21.06192
rho7.df[2] = 21.06192
rho8.df[2] = 21.06192
rho9.df[2] = 21.06192
rho10.df[2] = 21.06192
rho12.df[2] = 21.06192


rho1 = rho1.df[1,-1]
rho2 = rho2.df[1,-1]
rho3 = rho3.df[1,-1]
rho4 = rho4.df[1,-1]
rho5 = rho5.df[1,-1]
rho6 = rho6.df[1,-1]
rho7 = rho7.df[1,-1]
rho8 = rho8.df[1,-1]
rho9 = rho9.df[1,-1]
rho10 = rho10.df[10,-1]
rho11 = rho11.df[11,-1]
rho12 = rho12.df[12,-1]


Y1 <- as.matrix(dailyp1a1.df[anomalydateloc,2:length(dailyp1a1.df)])
Y2 <- as.matrix(dailyp2a1.df[anomalydateloc,2:length(dailyp2a1.df)])
Y3 <- as.matrix(dailyp3a1.df[anomalydateloc,2:length(dailyp3a1.df)])
Y4 <- as.matrix(dailyp4a1.df[anomalydateloc,2:length(dailyp4a1.df)])
Y5 <- as.matrix(dailyp5a1.df[anomalydateloc,2:length(dailyp5a1.df)])
Y6 <- as.matrix(dailyp6a1.df[anomalydateloc,2:length(dailyp6a1.df)])
Y7 <- as.matrix(dailyp7a1.df[anomalydateloc,2:length(dailyp7a1.df)])
Y8 <- as.matrix(dailyp8a1.df[anomalydateloc,2:length(dailyp8a1.df)])
Y9 <- as.matrix(dailyp9a1.df[anomalydateloc,2:length(dailyp9a1.df)])
Y10 <- as.matrix(dailyp10a1.df[anomalydateloc,2:length(dailyp10a1.df)])
Y11 <- as.matrix(dailyp11a1.df[anomalydateloc,2:length(dailyp11a1.df)])
Y12 <- as.matrix(dailyp12a1.df[anomalydateloc,2:length(dailyp12a1.df)])

Nday  <- nrow(Y1)

Nleaf1 <- ncol(Y1)
Nleaf2 <- ncol(Y2)
Nleaf3 <- ncol(Y3)
Nleaf4 <- ncol(Y4)
Nleaf5 <- ncol(Y5)
Nleaf6 <- ncol(Y6)
Nleaf7 <- ncol(Y7)
Nleaf8 <- ncol(Y8)
Nleaf9 <- ncol(Y9)
Nleaf10 <- ncol(Y10)
Nleaf11 <- ncol(Y11)
Nleaf12 <- ncol(Y12)


Nlv21 = 3
Nlv22 = 4
Nlv23 = 5
Nlv24 = 6
Nlv25 = 7
Nlv26 = 8
Nlv27 = 9
Nlv28 = 10
Nlv29 = 11
Nlv210 = 12
Nlv211 = 13
Nlv212 = 14

lV1b1 = rep(1:2,c(1,2))
lV1b2 = rep(1:2,c(2,2))
lV1b3 = rep(1:2,c(3,2))
lV1b4 = rep(1:2,c(4,2))
lV1b5 = rep(1:2,c(5,2))
lV1b6 = rep(1:2,c(6,2))
lV1b7 = rep(1:2,c(7,2))
lV1b8 = rep(1:2,c(8,2))
lV1b9 = rep(1:2,c(9,2))
lV1b10 = rep(1:2,c(10,2))
lV1b11 = rep(1:2,c(11,2))
lV1b12 = rep(1:2,c(12,2))


### Source models
setwd("H:/Desktop/7.1 No time to waste/Stats 798A Research Master/R-codes/JAGS")
source("model2.R")
source("model2h.R")

### Create Jags models       Note: can not use save load for jags.model

jags.data1 <- list(Y=Y1,Nday=Nday,Nleaf=Nleaf1,rho=rho1) 
jags.data2 <- list(Y=Y2,Nday=Nday,Nleaf=Nleaf2,rho=rho2) 
jags.data3 <- list(Y=Y3,Nday=Nday,Nleaf=Nleaf3,rho=rho3) 
jags.data4 <- list(Y=Y4,Nday=Nday,Nleaf=Nleaf4,rho=rho4) 
jags.data5 <- list(Y=Y5,Nday=Nday,Nleaf=Nleaf5,rho=rho5) 
jags.data6 <- list(Y=Y6,Nday=Nday,Nleaf=Nleaf6,rho=rho6) 
jags.data7 <- list(Y=Y7,Nday=Nday,Nleaf=Nleaf7,rho=rho7) 
jags.data8 <- list(Y=Y8,Nday=Nday,Nleaf=Nleaf8,rho=rho8) 
jags.data9 <- list(Y=Y9,Nday=Nday,Nleaf=Nleaf9,rho=rho9) 
jags.data10 <- list(Y=Y10,Nday=Nday,Nleaf=Nleaf10,rho=rho10) 
jags.data11 <- list(Y=Y11,Nday=Nday,Nleaf=Nleaf11,rho=rho11) 
jags.data12 <- list(Y=Y12,Nday=Nday,Nleaf=Nleaf12,rho=rho12) 

jags.data1h <- list(Y=Y1,Nday=Nday,Nleaf=Nleaf1,rho=rho1,Nlv1=Nlv1,Nlv2=Nlv21,lV1b=lV1b1) 
jags.data2h <- list(Y=Y2,Nday=Nday,Nleaf=Nleaf2,rho=rho2,Nlv1=Nlv1,Nlv2=Nlv22,lV1b=lV1b2) 
jags.data3h <- list(Y=Y3,Nday=Nday,Nleaf=Nleaf3,rho=rho3,Nlv1=Nlv1,Nlv2=Nlv23,lV1b=lV1b3) 
jags.data4h <- list(Y=Y4,Nday=Nday,Nleaf=Nleaf4,rho=rho4,Nlv1=Nlv1,Nlv2=Nlv24,lV1b=lV1b4) 
jags.data5h <- list(Y=Y5,Nday=Nday,Nleaf=Nleaf5,rho=rho5,Nlv1=Nlv1,Nlv2=Nlv25,lV1b=lV1b5) 
jags.data6h <- list(Y=Y6,Nday=Nday,Nleaf=Nleaf6,rho=rho6,Nlv1=Nlv1,Nlv2=Nlv26,lV1b=lV1b6) 
jags.data7h <- list(Y=Y7,Nday=Nday,Nleaf=Nleaf7,rho=rho7,Nlv1=Nlv1,Nlv2=Nlv27,lV1b=lV1b7) 
jags.data8h <- list(Y=Y8,Nday=Nday,Nleaf=Nleaf8,rho=rho8,Nlv1=Nlv1,Nlv2=Nlv28,lV1b=lV1b8) 
jags.data9h <- list(Y=Y9,Nday=Nday,Nleaf=Nleaf9,rho=rho9,Nlv1=Nlv1,Nlv2=Nlv29,lV1b=lV1b9) 
jags.data10h <- list(Y=Y10,Nday=Nday,Nleaf=Nleaf10,rho=rho10,Nlv1=Nlv1,Nlv2=Nlv210,lV1b=lV1b10) 
jags.data11h <- list(Y=Y11,Nday=Nday,Nleaf=Nleaf11,rho=rho11,Nlv1=Nlv1,Nlv2=Nlv211,lV1b=lV1b11) 
jags.data12h <- list(Y=Y12,Nday=Nday,Nleaf=Nleaf12,rho=rho12,Nlv1=Nlv1,Nlv2=Nlv212,lV1b=lV1b12) 


model1.jags <- jags.model(textConnection(model2.txt), data = jags.data1 ,n.chains=3,n.adapt=1000)
model2.jags <- jags.model(textConnection(model2.txt), data = jags.data2 ,n.chains=3,n.adapt=1000)
model3.jags <- jags.model(textConnection(model2.txt), data = jags.data3 ,n.chains=3,n.adapt=1000)
model4.jags <- jags.model(textConnection(model2.txt), data = jags.data4 ,n.chains=3,n.adapt=1000)
model5.jags <- jags.model(textConnection(model2.txt), data = jags.data5 ,n.chains=3,n.adapt=1000)
model6.jags <- jags.model(textConnection(model2.txt), data = jags.data6 ,n.chains=3,n.adapt=1000)
model7.jags <- jags.model(textConnection(model2.txt), data = jags.data7 ,n.chains=3,n.adapt=1000)
model8.jags <- jags.model(textConnection(model2.txt), data = jags.data8 ,n.chains=3,n.adapt=1000)
model9.jags <- jags.model(textConnection(model2.txt), data = jags.data9 ,n.chains=3,n.adapt=1000)
model10.jags <- jags.model(textConnection(model2.txt), data = jags.data10 ,n.chains=3,n.adapt=1000)
model11.jags <- jags.model(textConnection(model2.txt), data = jags.data11 ,n.chains=3,n.adapt=1000)
model12.jags <- jags.model(textConnection(model2.txt), data = jags.data12 ,n.chains=3,n.adapt=1000)

model1h.jags <- jags.model(textConnection(model2h.txt), data = jags.data1h ,n.chains=3,n.adapt=1000)
model2h.jags <- jags.model(textConnection(model2h.txt), data = jags.data2h ,n.chains=3,n.adapt=1000)
model3h.jags <- jags.model(textConnection(model2h.txt), data = jags.data3h ,n.chains=3,n.adapt=1000)
model4h.jags <- jags.model(textConnection(model2h.txt), data = jags.data4h ,n.chains=3,n.adapt=1000)
model5h.jags <- jags.model(textConnection(model2h.txt), data = jags.data5h ,n.chains=3,n.adapt=1000)
model6h.jags <- jags.model(textConnection(model2h.txt), data = jags.data6h ,n.chains=3,n.adapt=1000)
model7h.jags <- jags.model(textConnection(model2h.txt), data = jags.data7h ,n.chains=3,n.adapt=1000)
model8h.jags <- jags.model(textConnection(model2h.txt), data = jags.data8h ,n.chains=3,n.adapt=1000)
model9h.jags <- jags.model(textConnection(model2h.txt), data = jags.data9h ,n.chains=3,n.adapt=1000)
model10h.jags <- jags.model(textConnection(model2h.txt), data = jags.data10h ,n.chains=3,n.adapt=1000)
model11h.jags <- jags.model(textConnection(model2h.txt), data = jags.data11h ,n.chains=3,n.adapt=1000)
model12h.jags <- jags.model(textConnection(model2h.txt), data = jags.data12h ,n.chains=3,n.adapt=1000)

####-------------------------- Simulate MCMC Chain ------------------------------####
update(model1.jags,1000) 
update(model2.jags,1000) 
update(model3.jags,1000) 
update(model4.jags,1000) 
update(model5.jags,1000) 
update(model6.jags,1000) 
update(model7.jags,1000) 
update(model8.jags,1000) 
update(model9.jags,1000) 
update(model10.jags,1000) 
update(model11.jags,1000) 
update(model12.jags,1000)

update(model1h.jags,1000) 
update(model2h.jags,1000) 
update(model3h.jags,1000) 
update(model4h.jags,1000) 
update(model5h.jags,1000) 
update(model6h.jags,1000) 
update(model7h.jags,1000) 
update(model8h.jags,1000) 
update(model9h.jags,1000) 
update(model10h.jags,1000) 
update(model11h.jags,1000) 
update(model12h.jags,1000) 

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
model8.MCMC <- coda.samples(model8.jags, params, n.iter=4000, thin=4)
model9.MCMC <- coda.samples(model9.jags, params, n.iter=4000, thin=4)
model10.MCMC <- coda.samples(model10.jags, params, n.iter=4000, thin=4)
model11.MCMC <- coda.samples(model11.jags, params, n.iter=4000, thin=4)
model12.MCMC <- coda.samples(model12.jags, params, n.iter=4000, thin=4)

model1h.MCMC <- coda.samples(model1h.jags, params, n.iter=4000, thin=4)
model2h.MCMC <- coda.samples(model2h.jags, params, n.iter=4000, thin=4)
model3h.MCMC <- coda.samples(model3h.jags, params, n.iter=4000, thin=4)
model4h.MCMC <- coda.samples(model4h.jags, params, n.iter=4000, thin=4)
model5h.MCMC <- coda.samples(model5h.jags, params, n.iter=4000, thin=4)
model6h.MCMC <- coda.samples(model6h.jags, params, n.iter=4000, thin=4)
model7h.MCMC <- coda.samples(model7h.jags, params, n.iter=4000, thin=4)
model8h.MCMC <- coda.samples(model8h.jags, params, n.iter=4000, thin=4)
model9h.MCMC <- coda.samples(model9h.jags, params, n.iter=4000, thin=4)
model10h.MCMC <- coda.samples(model10h.jags, params, n.iter=4000, thin=4)
model11h.MCMC <- coda.samples(model11h.jags, params, n.iter=4000, thin=4)
model12h.MCMC <- coda.samples(model12h.jags, params, n.iter=4000, thin=4)

save(model1.MCMC,file="rdata/sim2/model1.MCMC.RData")
save(model2.MCMC,file="rdata/sim2/model2.MCMC.RData")
save(model3.MCMC,file="rdata/sim2/model3.MCMC.RData")
save(model4.MCMC,file="rdata/sim2/model4.MCMC.RData")
save(model5.MCMC,file="rdata/sim2/model5.MCMC.RData")
save(model6.MCMC,file="rdata/sim2/model6.MCMC.RData")
save(model7.MCMC,file="rdata/sim2/model7.MCMC.RData")
save(model8.MCMC,file="rdata/sim2/model8.MCMC.RData")
save(model9.MCMC,file="rdata/sim2/model9.MCMC.RData")
save(model10.MCMC,file="rdata/sim2/model10.MCMC.RData")
save(model11.MCMC,file="rdata/sim2/model11.MCMC.RData")
save(model12.MCMC,file="rdata/sim2/model12.MCMC.RData")

save(model1h.MCMC,file="rdata/sim2/model1h.MCMC.RData")
save(model2h.MCMC,file="rdata/sim2/model2h.MCMC.RData")
save(model3h.MCMC,file="rdata/sim2/model3h.MCMC.RData")
save(model4h.MCMC,file="rdata/sim2/model4h.MCMC.RData")
save(model5h.MCMC,file="rdata/sim2/model5h.MCMC.RData")
save(model6h.MCMC,file="rdata/sim2/model6h.MCMC.RData")
save(model7h.MCMC,file="rdata/sim2/model7h.MCMC.RData")
save(model8h.MCMC,file="rdata/sim2/model8h.MCMC.RData")
save(model9h.MCMC,file="rdata/sim2/model9h.MCMC.RData")
save(model10h.MCMC,file="rdata/sim2/model10h.MCMC.RData")
save(model11h.MCMC,file="rdata/sim2/model11h.MCMC.RData")
save(model12h.MCMC,file="rdata/sim2/model12h.MCMC.RData")

load("rdata/sim2/model1.MCMC.RData")
load("rdata/sim2/model2.MCMC.RData")
load("rdata/sim2/model3.MCMC.RData")
load("rdata/sim2/model4.MCMC.RData")
load("rdata/sim2/model5.MCMC.RData")
load("rdata/sim2/model6.MCMC.RData")
load("rdata/sim2/model7.MCMC.RData")
load("rdata/sim2/model8.MCMC.RData")
load("rdata/sim2/model9.MCMC.RData")
load("rdata/sim2/model10.MCMC.RData")
load("rdata/sim2/model11.MCMC.RData")
load("rdata/sim2/model12.MCMC.RData")

load("rdata/sim2/model1h.MCMC.RData")
load("rdata/sim2/model2h.MCMC.RData")
load("rdata/sim2/model3h.MCMC.RData")
load("rdata/sim2/model4h.MCMC.RData")
load("rdata/sim2/model5h.MCMC.RData")
load("rdata/sim2/model6h.MCMC.RData")
load("rdata/sim2/model7h.MCMC.RData")
load("rdata/sim2/model8h.MCMC.RData")
load("rdata/sim2/model9h.MCMC.RData")
load("rdata/sim2/model10h.MCMC.RData")
load("rdata/sim2/model11h.MCMC.RData")
load("rdata/sim2/model12h.MCMC.RData")

#-------------------------------renaming and combine for plots ---------------------------------
n.chain = 3

total   = c("total")
brunch1 = c("A","B")
leaves  = c("AA","AB","BA","BB")

MyText<- c(total,brunch1,leaves)
MyText

modelnames <- c("brunch1","brunch2","brunch3",
                "brunch4","brunch5","brunch6",
                "brunch7","brunch8","brunch9",
                "brunch10","brunch11","brunch12")
dim(model12.MCMC[[1]])
# create a chain for total of all models

modelall.MCMC = model7.MCMC

for(i in 1:3){
  modelall.MCMC[[i]][,1] <- model1.MCMC[[i]][,1]
  modelall.MCMC[[i]][,2] <- model2.MCMC[[i]][,1]
  modelall.MCMC[[i]][,3] <- model3.MCMC[[i]][,1]
  modelall.MCMC[[i]][,4] <- model4.MCMC[[i]][,1]
  modelall.MCMC[[i]][,5] <- model5.MCMC[[i]][,1]
  modelall.MCMC[[i]][,6] <- model6.MCMC[[i]][,1]
  modelall.MCMC[[i]][,7] <- model7.MCMC[[i]][,1]
  modelall.MCMC[[i]][,8] <- model8.MCMC[[i]][,1]
  modelall.MCMC[[i]][,9] <- model9.MCMC[[i]][,1]
  modelall.MCMC[[i]][,10] <- model10.MCMC[[i]][,1]
  modelall.MCMC[[i]][,11] <- model11.MCMC[[i]][,1]
  modelall.MCMC[[i]][,12] <- model12.MCMC[[i]][,1]

}

for (i in 1:n.chain) colnames(modelall.MCMC[[i]]) <- modelnames

modelA.MCMC = model7.MCMC

for(i in 1:3){
  modelA.MCMC[[i]][,1] <- model1.MCMC[[i]][,2]
  modelA.MCMC[[i]][,2] <- model2.MCMC[[i]][,2]
  modelA.MCMC[[i]][,3] <- model3.MCMC[[i]][,2]
  modelA.MCMC[[i]][,4] <- model4.MCMC[[i]][,2]
  modelA.MCMC[[i]][,5] <- model5.MCMC[[i]][,2]
  modelA.MCMC[[i]][,6] <- model6.MCMC[[i]][,2]
  modelA.MCMC[[i]][,7] <- model7.MCMC[[i]][,2]
  modelA.MCMC[[i]][,8] <- model8.MCMC[[i]][,2]
  modelA.MCMC[[i]][,9] <- model9.MCMC[[i]][,2]
  modelA.MCMC[[i]][,10] <- model10.MCMC[[i]][,2]
  modelA.MCMC[[i]][,11] <- model11.MCMC[[i]][,2]
  modelA.MCMC[[i]][,12] <- model12.MCMC[[i]][,2]
}

for (i in 1:n.chain) colnames(modelA.MCMC[[i]]) <- modelnames

modelAA.MCMC = model7.MCMC

for(i in 1:3){
  modelAA.MCMC[[i]][,1] <- model1.MCMC[[i]][,4]
  modelAA.MCMC[[i]][,2] <- model2.MCMC[[i]][,4]
  modelAA.MCMC[[i]][,3] <- model3.MCMC[[i]][,4]
  modelAA.MCMC[[i]][,4] <- model4.MCMC[[i]][,4]
  modelAA.MCMC[[i]][,5] <- model5.MCMC[[i]][,4]
  modelAA.MCMC[[i]][,6] <- model6.MCMC[[i]][,4]
  modelAA.MCMC[[i]][,7] <- model7.MCMC[[i]][,4]
  modelAA.MCMC[[i]][,8] <- model8.MCMC[[i]][,4]
  modelAA.MCMC[[i]][,9] <- model9.MCMC[[i]][,4]
  modelAA.MCMC[[i]][,10] <- model10.MCMC[[i]][,4]
  modelAA.MCMC[[i]][,11] <- model11.MCMC[[i]][,4]
  modelAA.MCMC[[i]][,12] <- model12.MCMC[[i]][,4]
}

for (i in 1:n.chain) colnames(modelAA.MCMC[[i]]) <- modelnames

modelallh.MCMC = model7h.MCMC

for(i in 1:3){
  modelallh.MCMC[[i]][,1] <- model1h.MCMC[[i]][,1]
  modelallh.MCMC[[i]][,2] <- model2h.MCMC[[i]][,1]
  modelallh.MCMC[[i]][,3] <- model3h.MCMC[[i]][,1]
  modelallh.MCMC[[i]][,4] <- model4h.MCMC[[i]][,1]
  modelallh.MCMC[[i]][,5] <- model5h.MCMC[[i]][,1]
  modelallh.MCMC[[i]][,6] <- model6h.MCMC[[i]][,1]
  modelallh.MCMC[[i]][,7] <- model7h.MCMC[[i]][,1]
  modelallh.MCMC[[i]][,8] <- model8h.MCMC[[i]][,1]
  modelallh.MCMC[[i]][,9] <- model9h.MCMC[[i]][,1]
  modelallh.MCMC[[i]][,10] <- model10h.MCMC[[i]][,1]
  modelallh.MCMC[[i]][,11] <- model11h.MCMC[[i]][,1]
  modelallh.MCMC[[i]][,12] <- model12h.MCMC[[i]][,1]
  
}

for (i in 1:n.chain) colnames(modelallh.MCMC[[i]]) <- modelnames

modelAh.MCMC = model7h.MCMC

for(i in 1:3){
  modelAh.MCMC[[i]][,1] <- model1h.MCMC[[i]][,2]
  modelAh.MCMC[[i]][,2] <- model2h.MCMC[[i]][,2]
  modelAh.MCMC[[i]][,3] <- model3h.MCMC[[i]][,2]
  modelAh.MCMC[[i]][,4] <- model4h.MCMC[[i]][,2]
  modelAh.MCMC[[i]][,5] <- model5h.MCMC[[i]][,2]
  modelAh.MCMC[[i]][,6] <- model6h.MCMC[[i]][,2]
  modelAh.MCMC[[i]][,7] <- model7h.MCMC[[i]][,2]
  modelAh.MCMC[[i]][,8] <- model8h.MCMC[[i]][,2]
  modelAh.MCMC[[i]][,9] <- model9h.MCMC[[i]][,2]
  modelAh.MCMC[[i]][,10] <- model10h.MCMC[[i]][,2]
  modelAh.MCMC[[i]][,11] <- model11h.MCMC[[i]][,2]
  modelAh.MCMC[[i]][,12] <- model12h.MCMC[[i]][,2]
}

for (i in 1:n.chain) colnames(modelAh.MCMC[[i]]) <- modelnames

modelAAh.MCMC = model7h.MCMC

for(i in 1:3){
  modelAAh.MCMC[[i]][,1]   <- model1h.MCMC[[i]][,4]
  modelAAh.MCMC[[i]][,2]   <- model2h.MCMC[[i]][,4]
  modelAAh.MCMC[[i]][,3]   <- model3h.MCMC[[i]][,4]
  modelAAh.MCMC[[i]][,4]   <- model4h.MCMC[[i]][,4]
  modelAAh.MCMC[[i]][,5]   <- model5h.MCMC[[i]][,4]
  modelAAh.MCMC[[i]][,6]   <- model6h.MCMC[[i]][,4]
  modelAAh.MCMC[[i]][,7]   <- model7h.MCMC[[i]][,4]
  modelAAh.MCMC[[i]][,8]   <- model8h.MCMC[[i]][,4]
  modelAAh.MCMC[[i]][,9]   <- model9h.MCMC[[i]][,4]
  modelAAh.MCMC[[i]][,10] <- model10h.MCMC[[i]][,4]
  modelAAh.MCMC[[i]][,11] <- model11h.MCMC[[i]][,4]
  modelAAh.MCMC[[i]][,12] <- model12h.MCMC[[i]][,4]
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

#----------------------------------DIC -----------------------------------------

dic.mod1 <- dic.samples(model1.jags, 1000, "pD")
dic.mod2 <- dic.samples(model2.jags, 1000, "pD")
dic.mod3 <- dic.samples(model3.jags, 1000, "pD")
dic.mod4 <- dic.samples(model4.jags, 1000, "pD")
dic.mod5 <- dic.samples(model5.jags, 1000, "pD")
dic.mod6 <- dic.samples(model6.jags, 1000, "pD")
dic.mod7 <- dic.samples(model7.jags, 1000, "pD")
dic.mod8 <- dic.samples(model8.jags, 1000, "pD")
dic.mod9 <- dic.samples(model9.jags, 1000, "pD")
dic.mod10 <- dic.samples(model10.jags, 1000, "pD")
dic.mod11 <- dic.samples(model11.jags, 1000, "pD")
dic.mod12 <- dic.samples(model12.jags, 1000, "pD")

dic.mod1h <- dic.samples(model1h.jags, 1000, "pD")
dic.mod2h <- dic.samples(model2h.jags, 1000, "pD")
dic.mod3h <- dic.samples(model3h.jags, 1000, "pD")
dic.mod4h <- dic.samples(model4h.jags, 1000, "pD")
dic.mod5h <- dic.samples(model5h.jags, 1000, "pD")
dic.mod6h <- dic.samples(model6h.jags, 1000, "pD")
dic.mod7h <- dic.samples(model7h.jags, 1000, "pD")
dic.mod8h <- dic.samples(model8h.jags, 1000, "pD")
dic.mod9h <- dic.samples(model9h.jags, 1000, "pD")
dic.mod10h <- dic.samples(model10h.jags, 1000, "pD")
dic.mod11h <- dic.samples(model11h.jags, 1000, "pD")
dic.mod12h <- dic.samples(model12h.jags, 1000, "pD")

M1 = summary(diffdic(dic.mod1, dic.mod1h))
M2 = summary(diffdic(dic.mod2, dic.mod2h))
M3 = summary(diffdic(dic.mod3, dic.mod3h))
M4 = summary(diffdic(dic.mod4, dic.mod4h))
M5 = summary(diffdic(dic.mod5, dic.mod5h))
M6 = summary(diffdic(dic.mod6, dic.mod6h))
M7 = summary(diffdic(dic.mod7, dic.mod7h))
M8 = summary(diffdic(dic.mod8, dic.mod8h))
M9 = summary(diffdic(dic.mod9, dic.mod9h))
M10 = summary(diffdic(dic.mod10, dic.mod10h))
M11 = summary(diffdic(dic.mod11, dic.mod11h))
M12 = summary(diffdic(dic.mod12, dic.mod12h))

compare_model = rbind(M1,M2,M3,M4,M5,M6,M7,M8,M9,M10,M11,M12)

row.names(compare_model) = c(as.character(1:12))

compare_model

xtable(compare_model ,digits = 3, type = "latex", file = "plots/findmodel/dictable.3abn.tex",
       caption = "DIC comapreisons for brunching number at A", label = "tab:dicanomaly2")

#------------------------------- MCMC Summary ---------------------------------

### Posterior summary for total of each model

postsumtotal = MCMCsummary(modelall.MCMC, round = 5 , n.eff = T)
postsumA = MCMCsummary(modelA.MCMC,   round = 5 , n.eff = T)
postsumAA = MCMCsummary(modelAA.MCMC,  round = 5 , n.eff = T)

postsumtotalh = MCMCsummary(modelallh.MCMC, round = 5 , n.eff = T)
postsumAh = MCMCsummary(modelAh.MCMC,   round = 5 , n.eff = T)
postsumAAh = MCMCsummary(modelAAh.MCMC,  round = 5 , n.eff = T)


cap1 = "Posterior distributions of different models for Total, with different brunch number at A, and calculated with independent Bayes model"
cap2 = "Posterior distributions of different models for A , with different brunch number at A, and calculated with independent Bayes model"
cap3 = "Posterior distributions of different models for AA, with different brunch number at A, and calculated with independent Bayes model"

cap4 = "Posterior distributions of different modelsfor Total, with different brunch number at A, and calculated with Hierarchiacl Bayes model"
cap5 = "Posterior distributions of different modelsfor A , with different brunch number at A, and calculated with Hierarchiacl Bayes model"
cap6 = "Posterior distributions of different modelsfor AA, with different brunch number at A, and calculated with Hierarchiacl Bayes model"


xtable(postsumtotal,digits = 4,type = "latex", file = "plots/sim2/MCMCsumatotal.tex",caption = cap1,label = "tab:pstprototal")
xtable(postsumtotalh,digits = 4,type = "latex", file = "plots/sim2/MCMCsumatotalh.tex",caption = cap4,label = "tab:pstprototalh")
xtable(postsumA ,digits = 4,type = "latex", file = "plots/sim2/MCMCsumbA.tex",caption = cap2,label = "tab:pstproA")
xtable(postsumAh ,digits = 4,type = "latex", file = "plots/sim2/MCMCsumbAh.tex",caption = cap5,label = "tab:pstproAh")
xtable(postsumAA,digits = 4,type = "latex", file = "plots/sim2/MCMCsumcAA.tex",caption = cap3,label = "tab:pstproAA")
xtable(postsumAAh,digits = 4,type = "latex", file = "plots/sim2/MCMCsumcAAh.tex",caption = cap6,label = "tab:pstprAAh")

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

pdf("plots/sim2/Densitytotal.PDF",width=8, height=12)
densityplot(modelall.MCMC,strip=F, strip.left=strip.custom(factor.levels=modelnames ,bg="skyblue"))
dev.off()

pdf("plots/sim2/DensityA.PDF",width=8, height=12)
densityplot(modelA.MCMC,strip=F, strip.left=strip.custom(factor.levels=modelnames ,bg="skyblue"))
dev.off()

pdf("plots/sim2/DensityAA.PDF",width=8, height=12)
densityplot(modelAA.MCMC,strip=F, strip.left=strip.custom(factor.levels=modelnames ,bg="skyblue"))
dev.off()

pdf("plots/sim2/Densitytotalh.PDF",width=8, height=12)
densityplot(modelallh.MCMC,strip=F, strip.left=strip.custom(factor.levels=modelnames ,bg="skyblue"))
dev.off()

pdf("plots/sim2/DensityAh.PDF",width=8, height=12)
densityplot(modelAh.MCMC,strip=F, strip.left=strip.custom(factor.levels=modelnames ,bg="skyblue"))
dev.off()

pdf("plots/sim2/DensityAAh.PDF",width=8, height=12)
densityplot(modelAAh.MCMC,strip=F, strip.left=strip.custom(factor.levels=modelnames ,bg="skyblue"))
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
setwd("H:/Desktop/7.1 No time to waste/Stats 798A Research Master/R-codes/JAGS")
source("16-posttheta.R")
library(RColorBrewer)

rhoall = rep(21.06192,12)
rhoA = rep(10.53896,12)
rhoAA = c(10.53896,5.289174,3.542965,2.616681,
          2.097515,1.734625,1.484204,1.315501,
          1.135215,1.056024,0.994524,0.968829)

postthetampic (rho = rhoall ,
               rhoh = rhoall  ,
               MCMC = modelall.MCMC,
               MCMCh = modelallh.MCMC,
               k = seq(1,0.05,by=-0.10),
               output ="plots/sim2/heattotal.PNG")


postthetampic (rho = rhoA,
               rhoh = rhoA ,
               MCMC = modelA.MCMC,
               MCMCh = modelAh.MCMC,
               k = seq(1,0.05,by=-0.10),
               output ="plots/sim2/heatA.PNG")

postthetampic (rho = rhoAA,
               rhoh = rhoAA ,
               MCMC = modelAA.MCMC,
               MCMCh = modelAAh.MCMC,
               k = seq(1,0.05,by=-0.10),
               output ="plots/sim2/heatAA.PNG")
#---------------------------------------
postthetampic (rho = rhoall,
               rhoh = rhoall ,
               MCMC = modelall.MCMC,
               MCMCh = modelallh.MCMC,
               k = seq(1,0.6,by=-0.05),
               output ="plots/sim2/heattotal2.PNG")


postthetampic (rho = rhoA,
               rhoh = rhoA ,
               MCMC = modelA.MCMC,
               MCMCh = modelAh.MCMC,
               k = seq(1,0.6,by=-0.05),
               output ="plots/sim2/heatA2.PNG")

postthetampic (rho = rhoAA,
               rhoh = rhoAA ,
               MCMC = modelAA.MCMC,
               MCMCh = modelAAh.MCMC,
               k = seq(1,0.6,by=-0.05),
               output ="plots/sim2/heatAA2.PNG")

