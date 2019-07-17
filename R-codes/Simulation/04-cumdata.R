source("00-permeables.R")
source("01-rand.day.time.R")
source("02-rand.day.R")
source("03-simdata.R")

### Function 4 ###

### codes for debugging cumdata

cumdata.test <- function(){
  
  cat2.val = rbind(            
    c(200,200,100),                 
    c(200,200, 50),
    c(100,  0,  0))
  
  cat2.val= cbind(            
    c(200,185,100,  0),                 
    c(150,200, 50, 15),
    c(100,  0,  0,  0))
  
  sample1 = simdata (100000,cat2.val = cat2.val)
  time.start = "2006/01/01 00:00:01"
  time.end = "2018/12/31 23:59:59"
  
  head(sample1)
  str(sample1)
  x = sample1
  
}

### cumulative data 

cumdata<-function(x){
  catnames = colnames(x)[-(1:8)]
  for(i in 1:length(catnames)){
    x[8+i] = cumsum(x[8+i])
  }
  return(x)
} 

### Test for cumdata
cumdata.test2 <- function(){
  
  cumdata.df = cumdata(sample1)
  head(cumdata.df)
  
}

