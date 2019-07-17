# Premeables

library(forecast)
library(lubridate)
library(hts)

# https://stackoverflow.com/questions/20210504/r-times-series-arima-model-forecasting-daily-data 
# https://robjhyndman.com/hyndsight/dailydata/ good stuff here 
# https://cran.r-project.org/web/packages/hts/hts.pdf
# https://robjhyndman.com/papers/Hierarchical6.pdf good stuff here  pg 9 



rhodata.htspred <- function(data = read.csv("daily1.csv"),   # use csv file of all date befor certain day
                            nodes = list(2,c(2,2)),          # N of nodes at each level
                            names = c("AA","AB","BA","BB"),  # name of leafs
                            characterz = c(1,1)              # how each character was assigned
                            ){   

###  input data, and assign as time series data 
daily.df <- data

lb_date <- as.POSIXlt(min(as.POSIXlt(daily.df$day)), format = "%m/%d/%Y") # oldest date in the DF.
lb_year  <- year         (lb_date)
lb_month <- month        (lb_date)
lb_msday <- days_in_month(lb_date)
lb_wkday <- weekdays     (lb_date)
lb_juday <- yday         (lb_date)

x        <- daily.df[,6:ncol(daily.df)]
abc      <- ts(x,start = c(lb_year,lb_date$yday), frequency=365)
colnames(abc) <- names
x <- hts (abc,nodes,characters =characterz)

# comb Optimal combination forecasts; 
# bu Bottom-up forecasts; 
# tdfp Top-down forecasts using forecast proportions

fc1 = forecast(x , h = 1, method = "comb", fmethod = "arima",
               FUN = function(x) tbats(x, use.parallel = TRUE)) # will take a while
fc2 = forecast(x , h = 1, method = "bu", fmethod = "arima",
               FUN = function(x) tbats(x, use.parallel = TRUE)) # will take a while
fc3 = forecast(x , h = 1, method = "tdfp", fmethod = "arima",
               FUN = function(x) tbats(x, use.parallel = TRUE)) # will take a while

muleaf3 = as.vector(fc1[[1]])
muleaf4 = as.vector(fc2[[1]])
muleaf5 = as.vector(fc3[[1]])

leafvals = list(optimcomb = muleaf3,bottomup = muleaf4,topdownfp = muleaf5)

  return(leafvals )

}









############################################################
# Tests
###############################################################
rhodata.test <- function(){
  
  
  
  daily.df <- read.csv("daily1.csv")
  dailydata = daily.df
  
  test = as.POSIXlt("2015-12-21 NZDT")
  
  lb_year  = year         (test)
  lb_month = month        (test)
  lb_msday = days_in_month(test)
  lb_wkday = weekdays     (test)
  lb_juday = yday         (test) # julian day
  
  lb_year
  lb_month
  lb_msday
  lb_wkday
  lb_juday
  
  
  
}

##############################################################

rhodata.test1 <- function(){
  
  
  
  year(as.POSIXlt("2010-03-12  NZDT"))
  as.POSIXlt("2010-03-12 NZDT")$yday-31-28
  
  head(daily.df$day)
  
  lb_date <- as.POSIXlt(min(as.POSIXlt(daily.df$day)), format = "%m/%d/%Y") # oldest date in the DF.
  lb_date
  
  lb_year  = year         (lb_date)
  lb_month = month        (lb_date)
  lb_msday = days_in_month(lb_date)
  lb_wkday = weekdays     (lb_date)
  lb_juday = yday         (lb_date)
  lb_date$yday
  
  
  day_ts <- ts(daily.df$total, start = c(lb_year,lb_date$yday), frequency=7)
  
  plot.ts(day_ts)
  
  y <- msts(day_ts, seasonal.periods=c(7,365.25))
  fit <- tbats(y)       # this takes ages and gives an error
  # > system.time(fit <- tbats(y) )
  # user  system elapsed 
  # 24.05    0.07   50.30 
  
  fc <- forecast(fit,1)
  fc[[2]][1]               #forecast seem pretty good
  
  
  
  
}
######################################################################

rhodata.test2 <- function(){
  
  
  print(htseg1)
  summary(htseg1)
  
  names(htseg1)
  
  aggts1 <- aggts(htseg1)
  aggts2 <- aggts(htseg1, levels = 0)
  aggts2 <- aggts(htseg1, levels = 1)
  aggts2 <- aggts(htseg1, levels = 2)
  aggts3 <- aggts(htseg1, levels = c(0, 2))
  plot(htseg1, levels = 1)
}

#######################################################################

rhodata.test3 <- function(){
  
  
  data = read.csv("daily1.csv")
  nodes = list(2,c(2,2))
  names = c("AA","AB","BA","BB")
  characterz = c(1,1)
  
  aggtsA <- aggts(x)
  aggts0 <- aggts(x, levels = 0)
  aggts1 <- aggts(x, levels = 1)
  aggts2 <- aggts(x, levels = 2)
  head(aggtsA)
  head(aggts0)
  head(aggts1)
  head(aggts2)
  
  plot(x, levels = 0)
  plot(x, levels = 1)
  plot(x, levels = 2)
  plot(x)
  
}