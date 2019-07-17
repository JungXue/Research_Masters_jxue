setwd("H:/Desktop/7.1 No time to waste/Stats 798A Research Master/R-codes/Simulation")

source("00-permeables.R")
source("01-rand.day.time.R")
source("02-rand.day.R")
source("03-simdata.R")
source("04-cumdata.R")
source("05-ggcumplot.R")
source("06-tabulatedata.R")
source("07-ggdailyplot.R")

library(forecast)
library(lubridate)
library(hts)
library(stringr)
library(xts)
library(weights)
set.seed(123456)

# --------------------Import data ---------------------------------
# EASTERN TIME ZONE 

setwd("H:/Desktop/7.1 No time to waste/Stats 798A Research Master/R-codes/MIMIC")

MIMIC3 <- read.csv("data/MIMIC3.csv")

names(MIMIC3)
nrow(MIMIC3)                       # number of hospital event = 50944
length(table(MIMIC3$SUBJECT_ID))   # number of patients = 38868


MIMIC3.df = data.frame(MIMIC3[,c(1,2,4,5,6,11)])
head(MIMIC3.df )
date = MIMIC3.df$ADMITDATE
MIMIC3.df$date  = strptime(paste(substr(date,7,10),"-",substr(date,4,5),"-",substr(date,1,2),sep=""), "%Y-%m-%d",tz="est")
MIMIC3.df$date$zone <- NULL
MIMIC3.df$HADM_ID = as.factor(MIMIC3.df$HADM_ID)
MIMIC3.df$ICD9_lv3 = as.factor(MIMIC3.df$ICD9_lv3)

str(MIMIC3.df) # lv1  17 categories
               # lv2 132 categories
               # lv3 631 categories

nlv3 = unname(table(MIMIC3.df$ICD9_lv3))
hist(nlv3,breaks=200)                     # hist of lv3 categories
                                                   # most disease are rare 
                                                   # few big groups made up of big portion

                                          # 3307 410 Acute myocardial infarction    everyday
                                          #  373 415 Acute pulmonary heart disease  once/month
                                          #   12 452 Portal vein thrombosis         once/year
                         
                 # Chapter 7 390-459	Diseases Of The Circulatory System is good to look at

17+132+631+1

### -------------------- create dummy variables --------------------

lv1 = MIMIC3.df[3]
lv2 = MIMIC3.df[4]
lv3 = MIMIC3.df[5]

result1 <- fastDummies::dummy_cols(lv1)
result2 <- fastDummies::dummy_cols(lv2)
result3 <- fastDummies::dummy_cols(lv3)

sum(colSums(result1[,-1]))
sum(colSums(result2[,-1]))
sum(colSums(result3[,-1]))

### -------------------- rename dummy variables --------------------

oldname1 = names(result1)[-1]
oldname2 = names(result2)[-1]
oldname3 = names(result3)[-1]

newname1  = str_sub(oldname1,10,-1)
newname2  = str_sub(oldname2,10,-1)
newname3  = str_sub(oldname3,10,-1)

names(result1) = c("data",newname1)
names(result2) = c("data",newname2)
names(result3) = c("data",newname3)

###---------------create raw dateset ---------------------------
# "index" "EventID" "time.char" "time.day" "time.int" "cat1" "cat2" "leaf" "A" "B" "AA" "AB" "BA" "BB" 

head(MIMIC3.df)

tz(MIMIC3.df$date) 

index = c(1:nrow(MIMIC3.df))
EventID = MIMIC3.df$HADM_ID
time.char = as.character(MIMIC3.df$ADMITTIME)
time.int = as.numeric(MIMIC3.df$ADMITTIME)
time.day = MIMIC3.df$date
cat1 = MIMIC3.df$ICD9_lv1
cat2 = MIMIC3.df$ICD9_lv2
cat3 = MIMIC3.df$ICD9_lv3
leaf = paste(cat1,cat2,cat3,sep = "_")
  
MIMIC3.final.df = cbind(index, EventID, time.char, time.day, time.int, cat1, cat2, cat3, leaf, 
                        result1[,-1],result2[,-1],result3[,-1])

MIMIC3.final.df[1:10,1:10]
dim(MIMIC3.final.df)

### test hierarchical structure
all(rowSums(MIMIC3.final.df[,10:26]) == rowSums(MIMIC3.final.df[, 27:158]))
all(rowSums(MIMIC3.final.df[,10:26]) == rowSums(MIMIC3.final.df[,159:789]))

### tabulate data
source("tabulatemimic.R")
MIMIC3.daily.df = tabulatemimic(MIMIC3.final.df)
MIMIC3.daily.df$day = as.Date(MIMIC3.daily.df$day)

str(MIMIC3.daily.df)
dim(MIMIC3.daily.df)
table(MIMIC3.daily.df$total)

### test hierarchical structure
all(MIMIC3.daily.df$total == rowSums(MIMIC3.daily.df[, 3:19]))      #good
all(MIMIC3.daily.df$total == rowSums(MIMIC3.daily.df[,20:151]))
all(MIMIC3.daily.df$total == rowSums(MIMIC3.daily.df[,152:782]))


sum(table(MIMIC3.daily.df$total)) # 36889/365.25 = 100.9966

### -------convert daily to monthly and yearly---------------------------

MIMIC3.daily.df[1,]
ts1 <- xts(MIMIC3.daily.df[,2], MIMIC3.daily.df$day )

MIMIC3_m = apply.monthly(ts1, sum)
MIMIC3_y = apply.yearly(ts1, sum)
MIMIC3_q = apply.quarterly(ts1, sum)

head(MIMIC3_m)  #timezone is right, no obs in 2099 or 2201
head(MIMIC3_y)
head(MIMIC3_q)

names_m = paste(rep(2100:2200,rep(12,101)),c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"),sep="")
names_y = paste(2100:2200)
names_q = paste(rep(2100:2200,rep(4,101)),c("q1","q2","q3","q4"),sep="")

dimnamesm = list(names_m,c("month",  colnames(MIMIC3.daily.df)[-1]))
dimnamesy = list(names_y,c("year",   colnames(MIMIC3.daily.df)[-1]))
dimnamesq = list(names_q,c("quarter",colnames(MIMIC3.daily.df)[-1]))

MIMIC3_m = matrix(NA,nrow = nrow(MIMIC3_m), ncol = ncol(MIMIC3.daily.df), dimnames = dimnamesm)
MIMIC3_y = matrix(NA,nrow = nrow(MIMIC3_y), ncol = ncol(MIMIC3.daily.df), dimnames = dimnamesy)
MIMIC3_q = matrix(NA,nrow = nrow(MIMIC3_q), ncol = ncol(MIMIC3.daily.df), dimnames = dimnamesq)


for (i in 2:(length(MIMIC3.daily.df))){
  
  MIMIC3_m[,1] = rownames(MIMIC3_m)
  MIMIC3_y[,1] = rownames(MIMIC3_y) 
  MIMIC3_q[,1] = rownames(MIMIC3_q) 

  MIMIC3_m[,i] = apply.monthly(xts(MIMIC3.daily.df[,i], MIMIC3.daily.df$day ), sum)
  MIMIC3_y[,i] = apply.yearly(xts(MIMIC3.daily.df[,i], MIMIC3.daily.df$day ), sum)
  MIMIC3_q[,i] = apply.quarterly(xts(MIMIC3.daily.df[,i], MIMIC3.daily.df$day ), sum)
  
}

MIMIC3_m.df = as.data.frame(MIMIC3_m)
MIMIC3_y.df = as.data.frame(MIMIC3_y)
MIMIC3_q.df = as.data.frame(MIMIC3_q)


for (i in 2:782){
MIMIC3_m.df[,i] = as.numeric(as.character(MIMIC3_m.df[,i]))
MIMIC3_y.df[,i] = as.numeric(as.character(MIMIC3_y.df[,i]))
MIMIC3_q.df[,i] = as.numeric(as.character(MIMIC3_q.df[,i]))
}

str(MIMIC3_m.df)
str(MIMIC3_y.df)
str(MIMIC3_q.df)


### check for hierarchical structure 


all(MIMIC3_m.df$total == rowSums(MIMIC3_m.df[,3:19]))
all(MIMIC3_m.df$total == rowSums(MIMIC3_m.df[,20:151]))
all(MIMIC3_m.df$total == rowSums(MIMIC3_m.df[,152:782]))


# convert daily data
MIMIC3_d.df = MIMIC3.daily.df

### ------------------------ Save data ---------------------------------

setwd("H:/Desktop/7.1 No time to waste/Stats 798A Research Master/R-codes/MIMIC")

write.csv(MIMIC3_d.df,'data/MIMIC3_d.csv') 
write.csv(MIMIC3_m.df,'data/MIMIC3_m.csv') 
write.csv(MIMIC3_y.df,'data/MIMIC3_y.csv') 
write.csv(MIMIC3_q.df,'data/MIMIC3_q.csv') 

save(MIMIC3_d.df,file = 'data/MIMIC3_d.RData')
save(MIMIC3_m.df,file = 'data/MIMIC3_m.RData')
save(MIMIC3_y.df,file = 'data/MIMIC3_y.RData')
save(MIMIC3_q.df,file = 'data/MIMIC3_q.RData')


### ---------------------- Assign hts ------------------------------

daily.df     <- MIMIC3_d.df[1:(nrow(MIMIC3_d.df)-3729),]  
monthly.df   <- MIMIC3_m.df[1:(nrow(MIMIC3_m.df)-122),]   
yearly.df    <- MIMIC3_y.df[1:(nrow(MIMIC3_y.df)-10),]   
quarterly.df <- MIMIC3_q.df[1:(nrow(MIMIC3_q.df)-40),]   

daily.df    [nrow(daily.df)    ,1] 
monthly.df  [nrow(monthly.df)  ,1]  
yearly.df   [nrow(yearly.df )  ,1]  
quarterly.df[nrow(quarterly.df),1] 


daily.ts     <- ts(daily.df    [-nrow(daily.df ),152:782])      #delete last obs in df to get ts
monthly.ts   <- ts(monthly.df  [-nrow(monthly.df),152:782])
yearly.ts    <- ts(yearly.df   [-nrow(yearly.df ),152:782])
quarterly.ts <- ts(quarterly.df[-nrow(quarterly.df),152:782])

allname = names(daily.df)

allname[3:19]
allname[20:151]

replv1 = c()
replv2 = c()


for (i in 1:17){
  replv1[i] = sum(as.numeric(allname[152:782]) <= as.numeric(substr(allname[3:19],5,7))[i])
}

for (j in 1:132){
  replv2[j] =sum(as.numeric(allname[152:782]) <= as.numeric(substr(allname[20:151],5,7))[j])
}

replv1 = replv1 - c(0,replv1[-17])
replv2 = replv2 - c(0,replv2[-132])

tsname = paste(rep(allname[3:19],replv1),
               rep(allname[20:151],replv2),
               allname[152:782],sep="")

colnames(daily.ts)     <- tsname
colnames(monthly.ts)   <- tsname
colnames(yearly.ts)    <- tsname
colnames(quarterly.ts) <- tsname

mimic3.daily.hts     <- hts(    daily.ts, characters = c(7, 7, 3))
mimic3.monthly.hts   <- hts(  monthly.ts, characters = c(7, 7, 3))
mimic3.yearly.hts    <- hts(   yearly.ts, characters = c(7, 7, 3))
mimic3.quarterly.hts <- hts(quarterly.ts, characters = c(7, 7, 3))

mimic3.daily.df     <- daily.df
mimic3.monthly.df   <- monthly.df
mimic3.yearly.df    <- yearly.df
mimic3.quarterly.df <- quarterly.df

mimic3.daily.ts     <- daily.ts
mimic3.monthly.ts   <- monthly.ts
mimic3.yearly.ts    <- yearly.ts
mimic3.quarterly.ts <- quarterly.ts

### -------- save df, ts and hts object ---------------------------------

save(     mimic3.daily.df, file ='data/mimic3.daily.df.RData')
save(   mimic3.monthly.df, file ='data/mimic3.monthly.df.RData')
save(    mimic3.yearly.df, file ='data/mimic3.yearly.df.RData')
save( mimic3.quarterly.df, file ='data/mimic3.quarterly.df.RData')

save(     mimic3.daily.ts, file ='data/mimic3.daily.ts.RData')
save(   mimic3.monthly.ts, file ='data/mimic3.monthly.ts.RData')
save(    mimic3.yearly.ts, file ='data/mimic3.yearly.ts.RData')
save( mimic3.quarterly.ts, file ='data/mimic3.quarterly.ts.RData')

save(    mimic3.daily.hts, file = 'data/mimic3.daily.hts.RData')
save(  mimic3.monthly.hts, file = 'data/mimic3.monthly.hts.RData')
save(   mimic3.yearly.hts, file = 'data/mimic3.yearly.hts.RData')
save(mimic3.quarterly.hts, file = 'data/mimic3.quarterly.hts.RData')

# ----------------- estimates -------------------------------------

load("data/forecast.list.RData")

rho = list()
loclv2 = list()
loclv3 = list()
lv0est = list()
lv1est = list()
lv2est = list()
lv3est = list()

for (i in 1:8){
  loclv2[[i]] = (cumsum(mimic3.yearly.hts$nodes$`Level 2`)+1)[-17]
  loclv3[[i]] = (cumsum(mimic3.yearly.hts$nodes$`Level 3`)+1)[-132]

  splitAt <- function(x, pos) unname(split(x, cumsum(seq_along(x) %in% pos)))
  
  lv3est[[i]] = forecast.list[[i]]
  lv2est[[i]] = unlist(lapply(splitAt(lv3est[[i]], loclv3[[i]]), sum))
  lv1est[[i]] = unlist(lapply(splitAt(lv2est[[i]], loclv2[[i]]), sum))
  lv0est[[i]] = sum (lv1est[[i]])
  
  rho[[i]] = c(lv0est[[i]],lv1est[[i]],lv2est[[i]],forecast.list[[i]])
}
rho 
names(rho) = names(forecast.list)
save( rho, file ='data/rho.RData')

###------------- prior -------------------------

all(rowSums(MIMIC3.final.df[,10:26]) == rowSums(MIMIC3.final.df[, 27:158]))
all(rowSums(MIMIC3.final.df[,10:26]) == rowSums(MIMIC3.final.df[,159:789]))

Packages=c("rmutil","ggplot2","pdp")
lapply(Packages, library, character.only = TRUE)


y1 = as.numeric(mimic3.yearly.df$total)
y1.df <- data.frame(y1)
y2 = rnorm(10000,1,sqrt(0.005))*mean(as.numeric(mimic3.yearly.df$total))
y2.df <- data.frame(y2)

y3 = as.numeric(rowMeans(mimic3.yearly.df[,3:19]))
y3.df <- data.frame(y3)
y4 = rnorm(10000,1,sqrt(0.005))*mean(rowMeans(mimic3.yearly.df[,3:19]))
y4.df <- data.frame(y4)

y1 = as.numeric(mimic3.yearly.df$total)
y1.df <- data.frame(y1)
y2 = rnorm(10000,1,sqrt(0.005))*mean(as.numeric(mimic3.yearly.df$total))
y2.df <- data.frame(y2)

y1 = as.numeric(mimic3.yearly.df$total)
y1.df <- data.frame(y1)
y2 = rnorm(10000,1,sqrt(0.005))*mean(as.numeric(mimic3.yearly.df$total))
y2.df <- data.frame(y2)


p1 <- ggplot(y1.df, aes(x=y1)) +   
  geom_histogram(binwidth=20,aes(y=..density..),color="dark grey",fill="light blue") + 
  geom_density(aes(y=..density..),alpha=.2, fill="light blue")+scale_x_continuous(limits = c(300, 700))+
  geom_vline(xintercept = mean(as.numeric(mimic3.yearly.df$total)), linetype="dashed", color = "red", size=0.5)

p2 <- ggplot(y2.df, aes(x=y2)) +   
  geom_histogram(binwidth=20,aes(y=..density..),color="dark grey",fill="light blue") + 
  geom_density(aes(y=..density..),alpha=.2, fill="light blue")+scale_x_continuous(limits = c(300, 700))+
  geom_vline(xintercept = mean(as.numeric(mimic3.yearly.df$total)), linetype="dashed", color = "red", size=0.5)

p3 <- ggplot(y3.df, aes(x=y3)) +   
  geom_histogram(binwidth=1,aes(y=..density..),color="dark grey",fill="light blue") + 
  geom_density(aes(y=..density..),alpha=.2, fill="light blue")+scale_x_continuous(limits = c(20, 40))+
  geom_vline(xintercept = mean(as.numeric(rowMeans(mimic3.yearly.df[,3:19]))), linetype="dashed", color = "red", size=0.5)

p4 <- ggplot(y4.df, aes(x=y4)) +   
  geom_histogram(binwidth=1,aes(y=..density..),color="dark grey",fill="light blue") + 
  geom_density(aes(y=..density..),alpha=.2, fill="light blue")+scale_x_continuous(limits = c(20, 40))+
  geom_vline(xintercept = mean(as.numeric(rowMeans(mimic3.yearly.df[,3:19]))), linetype="dashed", color = "red", size=0.5)

grid.arrange(p1 + ggtitle("Density of observed total daily arrival"), 
             p2 + ggtitle("Normal(1,0.005) prior"),
             p3 + ggtitle("Density of observed level 1 daily arrival"), 
             p4 + ggtitle("Normal(1,0.005) prior"),nrow=2)


mean(as.numeric(rowMeans(mimic3.yearly.df[,3:19])))


setwd("H:/Desktop/7.1 No time to waste/Stats 798A Research Master/R-codes/JAGS")
pdf("plots/mimic/Priorcompare.PDF",width=8, height=4)
grid.arrange(p1 + ggtitle("Density of observed total daily arrival"), 
             p2 + ggtitle("Normal(1,0.005) prior"),nrow=1)
dev.off()
