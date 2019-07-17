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

# h  = 1 predict 1 day

# system.time({fc1 = forecast(mimic3.daily.hts, h = 1, method = "comb", fmethod = "arima",
#                           FUN = function(x) tbats(x, use.parallel = TRUE))})

fc1 = forecast(mimic3.daily.hts, h = 1, method = "comb", fmethod = "arima",
               FUN = function(x) tbats(x, use.parallel = TRUE)) 
fc2 = forecast(mimic3.monthly.hts, h = 1, method = "comb", fmethod = "arima",
               FUN = function(x) tbats(x, use.parallel = TRUE)) 
fc3 = forecast(mimic3.yearly.hts, h = 1, method = "comb", fmethod = "arima",
               FUN = function(x) tbats(x, use.parallel = TRUE)) 
fc4 = forecast(mimic3.quarterly.hts, h = 1, method = "comb", fmethod = "arima",
               FUN = function(x) tbats(x, use.parallel = TRUE)) 

fc5 = forecast(mimic3.daily.hts, h = 1, method = "bu", fmethod = "arima",
               FUN = function(x) tbats(x, use.parallel = TRUE)) 
fc6 = forecast(mimic3.monthly.hts, h = 1, method = "bu", fmethod = "arima",
               FUN = function(x) tbats(x, use.parallel = TRUE)) 
fc7 = forecast(mimic3.yearly.hts, h = 1, method = "bu", fmethod = "arima",
               FUN = function(x) tbats(x, use.parallel = TRUE)) 
fc8 = forecast(mimic3.quarterly.hts, h = 1, method = "bu", fmethod = "arima",
               FUN = function(x) tbats(x, use.parallel = TRUE)) 

muleaf1 = as.vector(fc1[[1]])
muleaf2 = as.vector(fc2[[1]])
muleaf3 = as.vector(fc3[[1]])
muleaf4 = as.vector(fc4[[1]])
muleaf5 = as.vector(fc5[[1]])
muleaf6 = as.vector(fc6[[1]])
muleaf7 = as.vector(fc7[[1]])
muleaf8 = as.vector(fc8[[1]])


forecast.list = list(    daily_comb = muleaf1,
                         monthly_comb = muleaf2,
                         yearly_comb = muleaf3,
                         quarterly_comb = muleaf4,
                         daily_bu   = muleaf5,
                         monthly_bu   = muleaf6,
                         yearly_bu   = muleaf7,
                         quarterly_bu   = muleaf8)