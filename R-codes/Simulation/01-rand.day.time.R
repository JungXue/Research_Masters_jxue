source("00-permeables.R")

# Function 1 

## Create a number of random times

### rand.day.time originally by Dirk Eddelbuettel 2012
### https://stackoverflow.com/questions/14720983/efficiently-generate-a-random-sample-of-times-and-dates-between-two-dates
### "POSIXct" representing calendar times
### Make sure time zone is right otherwise first and last day have higher probability to occur

rand.day.time <- function(N, st="2006/01/01 00:00:01", et="2018/12/31 23:59:59") {
  st <- as.POSIXct(strptime(st, format="%Y/%m/%d %H:%M:%S", tz="Pacific/Auckland"))  
  et <- as.POSIXct(strptime(et, format="%Y/%m/%d %H:%M:%S", tz="Pacific/Auckland")) 
  dt <- as.numeric(difftime(et, st, unit="sec"))
  ev <- sort(runif(N, 0, dt))
  rt <- st + ev
  return(rt)
  
}



### testing
rand.day.time.test <- function(){
  
  time.start = "2006/01/01 00:00:01"
  time.end = "2018/12/31 23:59:59"
  
  print(rand.day.time(5,time.start,time.end)) 
  
  time.start = "2006/01/01 00:00:01"
  time.end = "2006/01/03 23:59:59"
  
  print(rand.day.time(10,time.start,time.end)) 
  
  print(rand.day.time(N = 10, st="2006/01/01 00:00:01", et="2006/01/03 00:00:01"))
  
  # system.time(rand.day.time(1000))
  # system.time(rand.day.time(1000000))
  
  test0 = rand.day.time (100000, time.start, time.end)
  head(test0)
  tail(test0)
  
  t.char = as.character(test0)                           # this line is taking a while
  t.day = as.character(as.Date(t.char, units = "days"))  # this line is taking a while
  dailycount = tabulate(as.factor(t.day))
  
  plot(dailycount,type="l", col="blue",main="Daily count plot")
  abline(h=mean(dailycount), col="red", lwd=2)
  points(dailycount[1], pch=19, col="red")
  points(length(dailycount), dailycount[length(dailycount)], pch=19, col="red")
  
  hist(dailycount)
  
  
}