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

rho$yearly_bu[rho$yearly_bu<0] = 0 
rho$yearly_comb[rho$yearly_comb<0] = 0 
rhoyear   <- abs(as.vector(rho$yearly_bu))
rhoyearh  <- abs(as.vector(rho$yearly_comb))

373/12
  
Nlv1 <- 17
Nlv2 <- 132
Nlv3 <- 631
lV1b <- rep(1:17,mimic3.daily.hts$nodes$`Level 2`) #17
lV2b <- rep(1:132, mimic3.daily.hts$nodes$`Level 3`) #132

### Source models

setwd("H:/Desktop/7.1 No time to waste/Stats 798A Research Master/R-codes/JAGS")
source("modelmimic.R")
source("modelmimich.R")

dim(Y)
dim(rhoyear)
### Create Jags models       Note: can not use save load for jags.model

jags.data1  <- list(Y=Y, Nday=Nday,Nleaf=Nleaf,rho=rhoyear) 
jags.data2  <- list(Y=Y1,Nday=Nday,Nleaf=Nleaf,rho=rhoyear) 
jags.data3  <- list(Y=Y2,Nday=Nday,Nleaf=Nleaf,rho=rhoyear) 
jags.data4  <- list(Y=Y3,Nday=Nday,Nleaf=Nleaf,rho=rhoyear) 

jags.data1h <- list(Y=Y, Nday=Nday,Nleaf=Nleaf,rho=rhoyearh,Nlv1=Nlv1,Nlv2=Nlv2,Nlv3=Nlv3,lV1b=lV1b,lV2b=lV2b) 
jags.data2h <- list(Y=Y1,Nday=Nday,Nleaf=Nleaf,rho=rhoyearh,Nlv1=Nlv1,Nlv2=Nlv2,Nlv3=Nlv3,lV1b=lV1b,lV2b=lV2b) 
jags.data3h <- list(Y=Y2,Nday=Nday,Nleaf=Nleaf,rho=rhoyearh,Nlv1=Nlv1,Nlv2=Nlv2,Nlv3=Nlv3,lV1b=lV1b,lV2b=lV2b) 
jags.data4h <- list(Y=Y3,Nday=Nday,Nleaf=Nleaf,rho=rhoyearh,Nlv1=Nlv1,Nlv2=Nlv2,Nlv3=Nlv3,lV1b=lV1b,lV2b=lV2b) 

model1.jags <- jags.model(textConnection(modelmimic.txt), data = jags.data1 ,n.chains=3,n.adapt=1000)
model2.jags <- jags.model(textConnection(modelmimic.txt), data = jags.data2 ,n.chains=3,n.adapt=1000)
model3.jags <- jags.model(textConnection(modelmimic.txt), data = jags.data3 ,n.chains=3,n.adapt=1000)
model4.jags <- jags.model(textConnection(modelmimic.txt), data = jags.data4 ,n.chains=3,n.adapt=1000)

model1h.jags <- jags.model(textConnection(modelmimich.txt), data = jags.data1h ,n.chains=3,n.adapt=1000)
model2h.jags <- jags.model(textConnection(modelmimich.txt), data = jags.data2h ,n.chains=3,n.adapt=1000)
model3h.jags <- jags.model(textConnection(modelmimich.txt), data = jags.data3h ,n.chains=3,n.adapt=1000)
model4h.jags <- jags.model(textConnection(modelmimich.txt), data = jags.data4h ,n.chains=3,n.adapt=1000)


####-------------------------- Simulate MCMC Chain ------------------------------####

dic.mod1 <- dic.samples(model1.jags, 1000, "pD")
dic.mod2 <- dic.samples(model2.jags, 1000, "pD")
dic.mod3 <- dic.samples(model3.jags, 1000, "pD")
dic.mod4 <- dic.samples(model4.jags, 1000, "pD")

dic.mod1h <- dic.samples(model1h.jags, 1000, "pD")
dic.mod2h <- dic.samples(model2h.jags, 1000, "pD")
dic.mod3h <- dic.samples(model3h.jags, 1000, "pD")
dic.mod4h <- dic.samples(model4h.jags, 1000, "pD")


update(model1.jags,1000) 
update(model2.jags,1000) 
update(model3.jags,1000) 
update(model4.jags,1000) 

update(model1h.jags,1000) 
update(model2h.jags,1000) 
update(model3h.jags,1000) 
update(model4h.jags,1000) 

### Assign parameter

params = c("mu")

### run coda.samples 


model1.MCMC <- coda.samples(model1.jags, params, n.iter=4000, thin=4)
model2.MCMC <- coda.samples(model2.jags, params, n.iter=4000, thin=4)
model3.MCMC <- coda.samples(model3.jags, params, n.iter=4000, thin=4)
model4.MCMC <- coda.samples(model4.jags, params, n.iter=4000, thin=4)

model1h.MCMC <- coda.samples(model1h.jags, params, n.iter=4000, thin=4)
model2h.MCMC <- coda.samples(model2h.jags, params, n.iter=4000, thin=4)
model3h.MCMC <- coda.samples(model3h.jags, params, n.iter=4000, thin=4)
model4h.MCMC <- coda.samples(model4h.jags, params, n.iter=4000, thin=4)

setwd("H:/Desktop/7.1 No time to waste/Stats 798A Research Master/R-codes/JAGS")

save(model1.MCMC,file="rdata/mimic/model1.MCMC.RData")
save(model2.MCMC,file="rdata/mimic/model2.MCMC.RData")
save(model3.MCMC,file="rdata/mimic/model3.MCMC.RData")
save(model4.MCMC,file="rdata/mimic/model4.MCMC.RData")

save(model1h.MCMC,file="rdata/mimic/model1h.MCMC.RData")
save(model2h.MCMC,file="rdata/mimic/model2h.MCMC.RData")
save(model3h.MCMC,file="rdata/mimic/model3h.MCMC.RData")
save(model4h.MCMC,file="rdata/mimic/model4h.MCMC.RData")

load("rdata/mimic/model1.MCMC.RData")
load("rdata/mimic/model2.MCMC.RData")
load("rdata/mimic/model3.MCMC.RData")
load("rdata/mimic/model4.MCMC.RData")

load("rdata/mimic/model1h.MCMC.RData")
load("rdata/mimic/model2h.MCMC.RData")
load("rdata/mimic/model3h.MCMC.RData")
load("rdata/mimic/model4h.MCMC.RData")

#-------------------------------renaming and combine for plots ---------------------------------
n.chain = 3

mimic3.daily.hts$labels$`Level 1`

total   = c("total")
lv1names = mimic3.daily.hts$labels$`Level 1`
lv2names = substr(mimic3.daily.hts$labels$`Level 2`,8,14)
lv3names = substr(mimic3.daily.hts$labels$`Level 3`,15,17)

MyText<- c(total,lv1names,lv2names,lv3names)
MyText

# 410 Acute myocardial infarction
# 415 Acute pulmonary heart disease
# 452 Portal vein thrombosis

# create a chain for total of all models
modelnorm.MCMC = model1.MCMC
model410.MCMC  = model1.MCMC
model415.MCMC  = model1.MCMC
model452.MCMC  = model1.MCMC

modelnormh.MCMC = model1.MCMC
model410h.MCMC  = model1.MCMC
model415h.MCMC  = model1.MCMC
model452h.MCMC  = model1.MCMC

for(i in 1:3){
  modelnorm.MCMC[[i]][,1] <- model1.MCMC[[i]][,1]
  modelnorm.MCMC[[i]][,2] <- model1.MCMC[[i]][,8]
  modelnorm.MCMC[[i]][,3] <- model1.MCMC[[i]][,72]
  modelnorm.MCMC[[i]][,4] <- model1.MCMC[[i]][,73]
  modelnorm.MCMC[[i]][,5] <- model1.MCMC[[i]][,77]
  modelnorm.MCMC[[i]][,6] <- model1.MCMC[[i]][,386]
  modelnorm.MCMC[[i]][,7] <- model1.MCMC[[i]][,390]
  modelnorm.MCMC[[i]][,8] <- model1.MCMC[[i]][,421]
  modelnorm.MCMC[[i]]     <- modelnorm.MCMC[[i]][,-(9:(ncol(modelnorm.MCMC[[i]])))]
}

for(i in 1:3){
  model410.MCMC[[i]][,1] <- model2.MCMC[[i]][,1]
  model410.MCMC[[i]][,2] <- model2.MCMC[[i]][,8]
  model410.MCMC[[i]][,3] <- model2.MCMC[[i]][,72]
  model410.MCMC[[i]][,4] <- model2.MCMC[[i]][,386]
  model410.MCMC[[i]]     <- model410.MCMC[[i]][,-(5:(ncol(model410.MCMC[[i]])))]
}

for(i in 1:3){
  model415.MCMC[[i]][,1] <- model3.MCMC[[i]][,1]
  model415.MCMC[[i]][,2] <- model3.MCMC[[i]][,8]
  model415.MCMC[[i]][,3] <- model3.MCMC[[i]][,73]
  model415.MCMC[[i]][,4] <- model3.MCMC[[i]][,390]
  model415.MCMC[[i]]     <- model415.MCMC[[i]][,-(5:(ncol(model415.MCMC[[i]])))]
}

for(i in 1:3){
  model452.MCMC[[i]][,1] <- model4.MCMC[[i]][,1]
  model452.MCMC[[i]][,2] <- model4.MCMC[[i]][,8]
  model452.MCMC[[i]][,3] <- model4.MCMC[[i]][,77]
  model452.MCMC[[i]][,4] <- model4.MCMC[[i]][,421]
  model452.MCMC[[i]]     <- model452.MCMC[[i]][,-(5:(ncol(model452.MCMC[[i]])))]
}

for(i in 1:3){
  modelnormh.MCMC[[i]][,1] <- model1h.MCMC[[i]][,1]
  modelnormh.MCMC[[i]][,2] <- model1h.MCMC[[i]][,8]
  modelnormh.MCMC[[i]][,3] <- model1h.MCMC[[i]][,72]
  modelnormh.MCMC[[i]][,4] <- model1h.MCMC[[i]][,73]
  modelnormh.MCMC[[i]][,5] <- model1h.MCMC[[i]][,77]
  modelnormh.MCMC[[i]][,6] <- model1h.MCMC[[i]][,386]
  modelnormh.MCMC[[i]][,7] <- model1h.MCMC[[i]][,390]
  modelnormh.MCMC[[i]][,8] <- model1h.MCMC[[i]][,421]
  modelnormh.MCMC[[i]]     <- modelnormh.MCMC[[i]][,-(9:(ncol(modelnormh.MCMC[[i]])))]
}

for(i in 1:3){
  model410h.MCMC[[i]][,1] <- model2h.MCMC[[i]][,1]
  model410h.MCMC[[i]][,2] <- model2h.MCMC[[i]][,8]
  model410h.MCMC[[i]][,3] <- model2h.MCMC[[i]][,72]
  model410h.MCMC[[i]][,4] <- model2h.MCMC[[i]][,386]
  model410h.MCMC[[i]]     <- model410h.MCMC[[i]][,-(5:(ncol(model410h.MCMC[[i]])))]
}

for(i in 1:3){
  model415h.MCMC[[i]][,1] <- model3h.MCMC[[i]][,1]
  model415h.MCMC[[i]][,2] <- model3h.MCMC[[i]][,8]
  model415h.MCMC[[i]][,3] <- model3h.MCMC[[i]][,73]
  model415h.MCMC[[i]][,4] <- model3h.MCMC[[i]][,390]
  model415h.MCMC[[i]]     <- model415h.MCMC[[i]][,-(5:(ncol(model415h.MCMC[[i]])))]
}

for(i in 1:3){
  model452h.MCMC[[i]][,1] <- model4h.MCMC[[i]][,1]
  model452h.MCMC[[i]][,2] <- model4h.MCMC[[i]][,8]
  model452h.MCMC[[i]][,3] <- model4h.MCMC[[i]][,77]
  model452h.MCMC[[i]][,4] <- model4h.MCMC[[i]][,421]
  model452h.MCMC[[i]]     <- model452h.MCMC[[i]][,-(5:(ncol(model452h.MCMC[[i]])))]
}

### rename variable names
for (i in 1:n.chain) colnames(modelnorm.MCMC[[i]]) <- c("total", "390-459", "410-414", "415-417", "451-459", "410", "415", "452")
for (i in 1:n.chain) colnames(model410.MCMC[[i]])  <- c("total", "390-459", "410-414", "410")
for (i in 1:n.chain) colnames(model415.MCMC[[i]])  <- c("total", "390-459", "415-417", "415")
for (i in 1:n.chain) colnames(model452.MCMC[[i]])  <- c("total", "390-459", "451-459", "452")

for (i in 1:n.chain) colnames(modelnormh.MCMC[[i]]) <- c("total", "390-459", "410-414", "415-417", "451-459", "410", "415", "452")
for (i in 1:n.chain) colnames(model410h.MCMC[[i]])  <- c("total", "390-459", "410-414", "410")
for (i in 1:n.chain) colnames(model415h.MCMC[[i]])  <- c("total", "390-459", "415-417", "415")
for (i in 1:n.chain) colnames(model452h.MCMC[[i]])  <- c("total", "390-459", "451-459", "452")


####================================================================================####

save(modelnorm.MCMC,file="rdata/mimic/modelnorm.MCMC.RData")
save(model410.MCMC,file="rdata/mimic/model410.MCMC.RData")
save(model415.MCMC,file="rdata/mimic/model415.MCMC.RData")
save(model452.MCMC,file="rdata/mimic/model452.MCMC.RData")

save(modelnormh.MCMC,file="rdata/mimic/modelnormh.MCMC.RData")
save(model410h.MCMC,file="rdata/mimic/model410h.MCMC.RData")
save(model415h.MCMC,file="rdata/mimic/model415h.MCMC.RData")
save(model452h.MCMC,file="rdata/mimic/model452h.MCMC.RData")

load("rdata/mimic/modelnorm.MCMC.RData")
load("rdata/mimic/model410.MCMC.RData")
load("rdata/mimic/model415.MCMC.RData")
load("rdata/mimic/model452.MCMC.RData")

load("rdata/mimic/modelnormh.MCMC.RData")
load("rdata/mimic/model410h.MCMC.RData")
load("rdata/mimic/model415h.MCMC.RData")
load("rdata/mimic/model452h.MCMC.RData")

#------------------------------- MCMC Summary ---------------------------------

### Posterior summary for total of each model

postsumnorm = MCMCsummary(modelnorm.MCMC, round = 3 , n.eff = T)
postsum410 = MCMCsummary(model410.MCMC,   round = 3 , n.eff = T)
postsum415 = MCMCsummary(model415.MCMC,  round = 3 , n.eff = T)
postsum452 = MCMCsummary(model452.MCMC,  round = 3 , n.eff = T)

postsumnormh = MCMCsummary(modelnormh.MCMC, round = 3 , n.eff = T)
postsum410h = MCMCsummary(model410h.MCMC,   round = 3 , n.eff = T)
postsum415h = MCMCsummary(model415h.MCMC,  round = 3 , n.eff = T)
postsum452h = MCMCsummary(model452h.MCMC,  round = 3 , n.eff = T)


label1 = "Posterior distributions for ICD 410,415 and 452 and its ancestors, with no added anomalies, and calculated with independent Bayes model"
label2 = "Posterior distributions for ICD 410 and its ancestors, with added anomalies, and calculated with independent Bayes model"
label3 = "Posterior distributions for ICD 415 and its ancestors, with added anomalies, and calculated with independent Bayes model"
label4 = "Posterior distributions for ICD 452 and its ancestors, with added anomalies, and calculated with independent Bayes model"

label5 = "Posterior distributions for ICD 410,415 and 452 and its ancestors, with no added anomalies, and calculated with Hierarchiacl Bayes model"
label6 = "Posterior distributions for ICD 410 and its ancestors, with added anomalies, and calculated with Hierarchiacl Bayes model"
label7 = "Posterior distributions for ICD 415 and its ancestors, with added anomalies, and calculated with Hierarchiacl Bayes model"
label8 = "Posterior distributions for ICD 452 and its ancestors, with added anomalies, and calculated with Hierarchiacl Bayes model"

xtable(postsumnorm,digits = 3,type = "latex", file = "plots/mimic/MCMCsumnorm.tex",label = "tab:postsumnorm.mimic",caption=label1)
xtable(postsumnormh,digits = 3,type = "latex", file = "plots/mimic/MCMCsumnormh.tex",label = "tab:postsumnormh.mimic",caption=label5)

xtable(postsum410 ,digits = 3,type = "latex", file = "plots/mimic/MCMCsum410.tex" ,label = "tab:postsum410.mimic", caption=label2)
xtable(postsum410h ,digits = 3,type = "latex", file = "plots/mimic/MCMCsum410h.tex" ,label = "tab:postsum410h.mimic", caption=label6)

xtable(postsum415 ,digits = 3,type = "latex", file = "plots/mimic/MCMCsum415.tex" ,label = "tab:postsum415.mimic", caption=label3)
xtable(postsum415h ,digits = 3,type = "latex", file = "plots/mimic/MCMCsum415h.tex" ,label = "tab:postsum415h.mimic", caption=label7)

Y  [c(2,9,74,391)-1]
Y2 [c(2,9,74,391)-1]

xtable(postsum452 ,digits = 3,type = "latex", file = "plots/mimic/MCMCsum452.tex" ,label = "tab:postsum452.mimic", caption=label4)
xtable(postsum452h ,digits = 3,type = "latex", file = "plots/mimic/MCMCsum452h.tex" ,label = "tab:postsum452h.mimic", caption=label8)


#------------------------------- density plots  ---------------------------------

pdf("plots/mimic/Densitynorm.PDF",width=8, height=5)
densityplot(modelnorm.MCMC, strip=F, strip.left=strip.custom(bg="skyblue"))
dev.off()

pdf("plots/mimic/Density410.PDF",width=8, height=3)
densityplot(model410.MCMC, strip=F, strip.left=strip.custom(bg="skyblue"))
dev.off()

pdf("plots/mimic/Density415.PDF",width=8, height=3)
densityplot(model415.MCMC, strip=F, strip.left=strip.custom(bg="skyblue"))
dev.off()

pdf("plots/mimic/Density452.PDF",width=8, height=3)
densityplot(model452.MCMC, strip=F, strip.left=strip.custom(bg="skyblue"))
dev.off()

pdf("plots/mimic/Densitynormh.PDF",width=8, height=5)
densityplot(modelnormh.MCMC, strip=F, strip.left=strip.custom(bg="skyblue"))
dev.off()

pdf("plots/mimic/Density410h.PDF",width=8, height=3)
densityplot(model410h.MCMC, strip=F, strip.left=strip.custom(bg="skyblue"))
dev.off()

pdf("plots/mimic/Density415h.PDF",width=8, height=3)
densityplot(model415h.MCMC, strip=F, strip.left=strip.custom(bg="skyblue"))
dev.off()

pdf("plots/mimic/Density452h.PDF",width=8, height=3)
densityplot(model452h.MCMC, strip=F, strip.left=strip.custom(bg="skyblue"))
dev.off()

#------------------------------- post theta plot  ---------------------------------
setwd("H:/Desktop/7.1 No time to waste/Stats 798A Research Master/R-codes/JAGS")
source("16-posttheta.R")
Packages=c("rmutil","ggplot2","pdp")
lapply(Packages, library, character.only = TRUE)
library(RColorBrewer)

rhonorm = rhoyear [c(2,9,73,74,78,387,391,422)-1]
rho410  = rhoyear [c(2,9,73,387)-1]
rho415  = rhoyear [c(2,9,74,391)-1]
rho452  = rhoyear [c(2,9,78,422)-1]

rhonormh = rhoyearh [c(2,9,73,74,78,387,391,422)-1]
rho410h  = rhoyearh [c(2,9,73,387)-1]
rho415h  = rhoyearh [c(2,9,74,391)-1]
rho452h  = rhoyearh [c(2,9,78,422)-1]


theta1 = postthetam (rho = rhonorm,rhoh = rhonormh, 
                     MCMC = modelnorm.MCMC, MCMCh = modelnormh.MCMC,
                     k = c(0.8,0.85,0.9,0.95,0.99))
theta2 = postthetam (rho = rho410,rhoh = rho410h, 
                     MCMC = model410.MCMC, MCMCh = model410h.MCMC,
                     k = c(0.8,0.85,0.9,0.95,0.99))
theta3 = postthetam (rho = rho415,rhoh = rho415h, 
                     MCMC = model415.MCMC, MCMCh = model415h.MCMC,
                     k = c(0.8,0.85,0.9,0.95,0.99))
theta4 = postthetam (rho = rho415,rhoh = rho415h, 
                     MCMC = model415.MCMC, MCMCh = model415h.MCMC,
                     k = c(0.8,0.85,0.9,0.95,0.99))
theta1
theta2
theta3
theta4

xtable(theta1,digits = 3,type = "latex", file = "plots/mimic/thetanorm.tex")
xtable(theta2,digits = 3,type = "latex", file = "plots/mimic/theta410.tex")
xtable(theta3,digits = 3,type = "latex", file = "plots/mimic/theta415.tex")
xtable(theta4,digits = 3,type = "latex", file = "plots/mimic/theta452.tex")

# ---------------- heat maps ------------------------------------------------

postthetampic (rho = rhonorm,rhoh = rhonormh, MCMC = modelnorm.MCMC, MCMCh = modelnormh.MCMC,
                 k = seq(1,0.05,by=-0.10),output = "plots/mimic/heatnorm.PNG")
postthetampic (rho = rhonorm,rhoh = rhonormh, MCMC = modelnorm.MCMC, MCMCh = modelnormh.MCMC,
               k = seq(1,0.90,by=-0.01),output = "plots/mimic/heatnorm2.PNG")


postthetampic (rho = rho410,rhoh = rho410h, MCMC = model410.MCMC, MCMCh = model410h.MCMC,
                 k = seq(1,0.05,by=-0.10),output = "plots/mimic/heat410.PNG")
postthetampic (rho = rho410,rhoh = rho410h, MCMC = model410.MCMC, MCMCh = model410h.MCMC,
               k = seq(1,0.90,by=-0.01),output = "plots/mimic/heat4102.PNG")


postthetampic (rho = rho415,rhoh = rho415h, MCMC = model415.MCMC, MCMCh = model415h.MCMC,
                 k = seq(1,0.05,by=-0.10),output = "plots/mimic/heat415.PNG")
postthetampic (rho = rho415,rhoh = rho415h, MCMC = model415.MCMC, MCMCh = model415h.MCMC,
               k = seq(1,0.90,by=-0.01),output = "plots/mimic/heat4152.PNG")


postthetampic (rho = rho452,rhoh = rho452h, MCMC = model452.MCMC, MCMCh = model452h.MCMC,
                 k = seq(1,0.05,by=-0.10),output = "plots/mimic/heat452.PNG")
postthetampic (rho = rho452,rhoh = rho452h, MCMC = model452.MCMC, MCMCh = model452h.MCMC,
               k = seq(1,0.90,by=-0.01),output = "plots/mimic/heat4522.PNG")

# ---------------------------------------- DIC -------------------------------



M1 = summary(diffdic(dic.mod1, dic.mod1h))
M2 = summary(diffdic(dic.mod2, dic.mod2h))
M3 = summary(diffdic(dic.mod3, dic.mod3h))
M4 = summary(diffdic(dic.mod4, dic.mod4h))


compare_model = rbind(M1,M2,M3,M4)

row.names(compare_model) = c("No amomaly", "410", "415", "452")

compare_model

xtable(compare_model ,digits = 3, type = "latex", file = "plots/mimic/dictable.3abn.tex",
       caption = "DIC comapreisons for Independent Bayesian Model and Hierarchical Bayesian model for no anomaly, and anomaly added for common, rare and extremely rare diseases", label = "tab:dicmimic")



1/0.6
