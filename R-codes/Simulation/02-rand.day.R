source("00-permeables.R")
source("01-rand.day.time.R")

# Function 2

## Create random day 

rand.day <- function(N = 2,                         # Number of days
                     st = "2006/01/01 00:00:01", 
                     et = "2018/12/31 23:59:59",
                     period = c(5,3)){              # each period of days
  
  if(length(period)!=N)
    warning("Length of period need to be same as N")
  
  day1 = rand.day.time(N, st, et)
  
  time.char = as.character(day1)
  time.day  = as.character(as.Date(time.char, units = "days"))  
  time.int  = as.integer(as.Date(time.char, units = "days"))

  days=c()
  for(i in 1:N){
    days = c(days,time.int[i]:(time.int[i]+period[i]-1))
  }
  days
  
  days2 = as.character(as.Date(days), units = "days")
  return(days2)
}




### testing 
rand.day.test <- function(){
  
  time.start = "2006/01/01 00:00:01" 
  time.end = "2006/01/03 23:59:59"
  N=10
  
  ### no value outside of date range
  test1 = rand.day(N = 10000,time.start,time.end, period = c(rep(1,10000)))
  table(test1)

  
  rand.day(N=1,period=1)
  rand.day(N=1,period=5)
  rand.day(N=3,period=c(1,1,1))
  rand.day(N=3,period=c(5,3,2))
  
  
}