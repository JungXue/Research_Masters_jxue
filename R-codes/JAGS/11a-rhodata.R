# setwd("H:/Desktop/7.1 No time to waste/Stats 798A Research Master/R-codes/JAGS")

source("12-htsmodel.R")
source("14-assignleaf.R")
source("15-assignleaf1.R")

###################################################################################

# Testing
rhodata.test <- function(){
  
    raw.df <- read.csv("data/raw1.csv")
    cum.df <- read.csv("data/cum1.csv")
  daily.df <- read.csv("data/daily1.csv")

    rawdata <- raw.df
  dailydata <- daily.df
 outputdata <- "rdata/function/rho.RData"
     method <- "mean"       
       data <- "simulation"

}

##################################################################################

# Create Rho

rhodata <- function(   rawdata = raw.df,     
                     dailydata = daily.df,
                    outputdata = "rdata/function/rho.RData",      # outputfile as rdata, save time for recalculations
                        method = "mean",                          #
                          data = "simulation"){                   # 

### Number of observation, brunch and leafs, for simulation
  head(daily.df)
  n.cat1 = length(levels(rawdata$cat1))
  n.leaf = length(levels(rawdata$leaf))
       N = nrow(dailydata)
  n.leaf
### mu predictions for each leaf and level
  
  # method 1 same mu for all leaf
  # method 2 diff mu, base on leaf mean
  
  muleaf1 = rep(sum(colMeans(dailydata[(3 + n.cat1+1):length(dailydata)]))/n.leaf,n.leaf)
  muleaf2 = colMeans(dailydata[(2 + n.cat1+1):length(dailydata)])

### reassign all mu
  
  muleaf1.df <- assignleaf(muleaf1)
  muleaf2.df <- assignleaf(muleaf2)

  head(muleaf1.df)
### return rho
  
  rho.df <-switch(method, "theoritical" = muleaf1.df,
                          "mean"        = muleaf2.df)
  head(rho.df)
  
### export data as rdata
  
  save(rho.df,file=outputdata)
  # load(outputdata)
  
  return(rho.df)

}
