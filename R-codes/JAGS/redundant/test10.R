library(dplyr)
library(tidyr)
library(coda)
library(rjags)
library(R2jags)
library(lattice)
library(mcmcplots)
library(bayesboot)
set.seed(1234567)

# Import datas
raw.df <- read.csv("raw3.csv")
cum.df <- read.csv("cum3.csv")
daily.df <- read.csv("daily3.csv")

# raw.df <- read.csv("raw3.abn.csv")
# cum.df <- read.csv("cum3.abn.csv")
# daily.df <- read.csv("daily3.abn.csv")

##########################################################################

### Find rho of data
source("11-rhodata.R")


#####################################################################################

# rho.df <- read.csv("rho.df.csv")
rho.df = rhodata(rawdata = raw.df,dailydata = daily.df,outputcsv='rho.df.csv')
head(rho.df)

n.cat1 = length(levels(raw.df$cat1))
n.leaf = length(levels(raw.df$leaf))
N = nrow(daily.df)

# expected rate of leafs as matrix form
rhomatrix = data.frame(rho.df[,(3+n.cat1+1):length(rho.df)])
head(rhomatrix)

# counts of leafs as matrix form
dailymatrix = data.frame(daily.df[,(3+n.cat1+1):length(rho.df)])
head(dailymatrix)

#####################################################
# expected rate as columnar form
rhocol <- gather(rho.df,key = 'group',value = 'Rho',3:length(rho.df))
head(rhocol)

# expected rate of leafs as columnar form
rholeafcol <- gather(rhomatrix,key = 'Leaf',value = 'Rho',1:length(rhomatrix))
head(rholeafcol)

# Count as columnar form
countcol  <- gather(daily.df,key = 'group',value = 'Count',3:length(daily.df))
head(countcol )

# Count leafs as columnar form
countleafcol <- gather(dailymatrix,key = 'group',value = 'Count',1:length(dailymatrix))
head(countleafcol)

#########################################################################################


head(countcol)
head(rhocol)
head(daily.df)
head(rho.df)


Y     <- daily.df[3:length(daily.df)]
Nday  <- nrow(Y)
Nleaf <- ncol(Y)
rho   <- as.vector(as.matrix(rho.df[1,3:length(rho.df)]))

rho.total <- rho[1]
scale <- as.vector(as.matrix(rho/rep(rho[1],length(rho))))


model.txt.sample =  "model{

# Likelihood

for(j in 1:Nleaf){
mu[j]        <- rho[j]*lambda
for(i in 1:Nday){
Y[i,j]      ~ dpois(mu[j])
}
}

lambda ~ dgamma(1,1)

}"



source("model1.R")
source("model2.R")
source("model3.R")
source("model4.R")
source("model5.R") 
source("model6.R") 

jags.data1 <- list(Y=Y,Nday=Nday,Nleaf=Nleaf,rho=rho) #day = daily.df$day,
jags.data2 <- list(Y=Y,Nday=Nday,Nleaf=Nleaf,rho=rho) 
jags.data3 <- list(Y=Y,Nday=Nday,Nleaf=Nleaf,rho=rho) 
jags.data4 <- list(Y=Y,Nday=Nday,Nleaf=Nleaf,rho=rho) 
jags.data5 <- list(Y=Y,Nday=Nday,Nleaf=Nleaf,rho=rho) 
jags.data6 <- list(Y=Y,Nday=Nday,Nleaf=Nleaf,rho.total=rho.total,scale=scale) 

model1.jags <- jags.model(textConnection(model1.txt), data = jags.data1 ,n.chains=3,n.adapt=1000)
model2.jags <- jags.model(textConnection(model2.txt), data = jags.data2 ,n.chains=3,n.adapt=1000)
model3.jags <- jags.model(textConnection(model3.txt), data = jags.data3 ,n.chains=3,n.adapt=1000)
model4.jags <- jags.model(textConnection(model4.txt), data = jags.data4 ,n.chains=3,n.adapt=1000)
model5.jags <- jags.model(textConnection(model5.txt), data = jags.data5 ,n.chains=3,n.adapt=1000)
model6.jags <- jags.model(textConnection(model6.txt), data = jags.data6 ,n.chains=3,n.adapt=1000)

#burn in
update(model1.jags,1000) 
update(model2.jags,1000) 
update(model3.jags,1000) 
update(model4.jags,1000) 
update(model5.jags,1000) 
update(model6.jags,1000) 

#check which samplers are being used
list.samplers(model1.jags) 
list.samplers(model2.jags) 
list.samplers(model3.jags) 
list.samplers(model4.jags) 
list.samplers(model5.jags) 
list.samplers(model6.jags) 

params1 = c("mu")    #what can I do here to change output to total cata 1 and leaf???
params2 = c("mu")
params3 = c("mu")
params4 = c("mu")
params5 = c("mu")
params6 = c("mu")

# run length control (pilot run)
### Raftery-Lewis diagnostic estimate burnin and sample
model1.test <- coda.samples(model1.jags, params1, n.iter=5000, thin=1)
model2.test <- coda.samples(model2.jags, params2, n.iter=5000, thin=1)
model3.test <- coda.samples(model3.jags, params3, n.iter=5000, thin=1)
model4.test <- coda.samples(model4.jags, params4, n.iter=5000, thin=1)
model5.test <- coda.samples(model5.jags, params5, n.iter=5000, thin=1)
model6.test <- coda.samples(model6.jags, params6, n.iter=5000, thin=1)
summary(model1.test)
summary(model2.test)
summary(model3.test)
summary(model4.test)
summary(model5.test)
summary(model6.test)
raftery.diag(model1.test)
raftery.diag(model2.test)
raftery.diag(model3.test)
raftery.diag(model4.test)
raftery.diag(model5.test)
raftery.diag(model6.test)

# run coda.samples with DIC module
model1.sim <- coda.samples(model1.jags, params1, n.iter=4000, thin=4)
model2.sim <- coda.samples(model2.jags, params2, n.iter=4000, thin=4)
model3.sim <- coda.samples(model3.jags, params3, n.iter=4000, thin=4)
model4.sim <- coda.samples(model4.jags, params4, n.iter=4000, thin=4)
model5.sim <- coda.samples(model5.jags, params5, n.iter=4000, thin=4)
model6.sim <- coda.samples(model6.jags, params6, n.iter=4000, thin=4)
# summarising the posterior distributions for parameters
summary(model1.sim)
summary(model2.sim)
summary(model3.sim)
summary(model4.sim)
summary(model5.sim)
summary(model6.sim)

# https://www.stat.auckland.ac.nz/~paul/RGraphics/chapter4.pdf
# https://www.statmethods.net/RiA/lattice.pdf
# http://www.sumsar.net/papers/baath_2015_modeling_match_resluts_in_soccer.pdf?fbclid=IwAR0Q4W-VloPFqGMkWLkkS3UYETpNBkWhXtWFNLOQ8ePxE7ekD3FONIlc-9E


MyText<-names(daily.df)[3:ncol(daily.df)]
MyText

xyplot(model1.sim, strip=F, strip.left=strip.custom(factor.levels=MyText,bg="skyblue"))
xyplot(model2.sim, strip=F, strip.left=strip.custom(factor.levels=MyText,bg="skyblue"))
xyplot(model3.sim, strip=F, strip.left=strip.custom(factor.levels=MyText,bg="skyblue"))
xyplot(model4.sim, strip=F, strip.left=strip.custom(factor.levels=MyText,bg="skyblue"))
xyplot(model5.sim, strip=F, strip.left=strip.custom(factor.levels=MyText,bg="skyblue"))
xyplot(model6.sim, strip=F, strip.left=strip.custom(factor.levels=MyText,bg="skyblue"))

densityplot(model1.sim, strip=F, strip.left=strip.custom(factor.levels=MyText,bg="skyblue"))
densityplot(model2.sim, strip=F, strip.left=strip.custom(factor.levels=MyText,bg="skyblue"))
densityplot(model3.sim, strip=F, strip.left=strip.custom(factor.levels=MyText,bg="skyblue"))
densityplot(model4.sim, strip=F, strip.left=strip.custom(factor.levels=MyText,bg="skyblue"))
densityplot(model5.sim, strip=F, strip.left=strip.custom(factor.levels=MyText,bg="skyblue"))
densityplot(model6.sim, strip=F, strip.left=strip.custom(factor.levels=MyText,bg="skyblue"))

acfplot(model1.sim, strip=F, strip.left=strip.custom(factor.levels=MyText,bg="skyblue"))  #,aspect = 'fill' 
acfplot(model2.sim, strip=F, strip.left=strip.custom(factor.levels=MyText,bg="skyblue"))
acfplot(model3.sim, strip=F, strip.left=strip.custom(factor.levels=MyText,bg="skyblue"))
acfplot(model4.sim, strip=F, strip.left=strip.custom(factor.levels=MyText,bg="skyblue"))
acfplot(model5.sim, strip=F, strip.left=strip.custom(factor.levels=MyText,bg="skyblue"))
acfplot(model6.sim, strip=F, strip.left=strip.custom(factor.levels=MyText,bg="skyblue"))

HPDinterval(model1.sim)
HPDinterval(model2.sim)
HPDinterval(model3.sim)
HPDinterval(model4.sim)
HPDinterval(model5.sim)
HPDinterval(model6.sim)

caterplot(model1.sim,labels.loc = "above",labels = MyText)
caterplot(model2.sim,labels.loc = "above",labels = MyText)
caterplot(model3.sim,labels.loc = "above",labels = MyText)
caterplot(model4.sim,labels.loc = "above",labels = MyText)
caterplot(model5.sim,labels.loc = "above",labels = MyText)
caterplot(model6.sim,labels.loc = "above",labels = MyText)

plotPost(model1.sim)
plotPost(model2.sim)
plotPost(model3.sim)
plotPost(model4.sim)
plotPost(model5.sim)
plotPost(model6.sim)

?HPDinterval
?caterplot
?densityplot
### Raftery-Lewis diagnostic estimate burnin and sample


# goodness of fit??? AIC BIC DIC  
# any way to combine xyplot and density plot??
# making predictions???


require("AICcmodavg")
Cand.mods <- list(model1.sim, model2.sim, model6.sim)
Model.names <- "hierarchical model"
##other models can be added to Cand.mods
##to compare them to the top model

##model selection table
dictab(cand.set = Cand.mods, modnames = Model.names)


