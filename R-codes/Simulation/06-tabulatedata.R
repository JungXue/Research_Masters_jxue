source("00-permeables.R")
source("01-rand.day.time.R")
source("02-rand.day.R")
source("03-simdata.R")
source("04-cumdata.R")
source("05-ggcumplot.R")


# Function 6 

## Function to create cumulative and daily data


### codes for debugging cum and daily data

tabulatedata.test <- function(){
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
  
  sample1 = MIMIC3.final.df
}

### tabulate date, ie daily counts

tabulatedata <- function(x=sample1){
  
  time.start = min(as.Date(x[,4]),na.rm = TRUE)
  time.end =  max(as.Date(x[,4]),na.rm = TRUE)
 
  time.start.int = as.integer(as.Date(time.start, units = "days")) 
  time.end.int = as.integer(as.Date(time.end , units = "days")) 
  time.start.int
  time.end.int
  
  library("zoo")
  timelevel =  as.factor(as.Date(time.start.int:time.end.int))
  head(timelevel)
  tail(timelevel)
  
  catname = c("day","total",names(x)[9:length(x)])
  catname
  
  catname2 = catname[3:length(catname)]
  
  nchar(catname2)
  lettercount = table(nchar(catname2))
  lettercount 
  
  loc.b1 = which(names(lettercount)==as.character(nchar(as.character(x[1,6]))))
  
  cat1names = sort(catname2[1:lettercount[loc.b1]])
  leafnames = sort(catname2[(lettercount[loc.b1]+1):length(catname2)])
  catname3 = c(cat1names,leafnames)
  catname3
  
 
  datedata = factor(as.Date(x[,4]),levels=timelevel)
  str(datedata)
  
  daily.df = data.frame(matrix(NA,nrow = length(timelevel),ncol = length(x)-8+2,
                               dimnames = list(c(1:length(timelevel)),catname)))
  head(daily.df)
  
  for(i in 1:(ncol(daily.df)-2)){
    daily.df[,1] = timelevel 
    daily.df[,2] = tabulate(datedata)
    daily.df[,(2+i)] = table(datedata,x[,(8+i)])[,2]
  }
  
  colnames(daily.df) = catname
  head(daily.df)
  daily.df = daily.df[,c("day","total",catname3)]
  
  daily.df[1:20,]
  
  return(daily.df)
  return()
}


### checks on tabulatedata


tabulatedata.test <- function(){
  
  dailycount.df = tabulatedata(sample1)
  head(dailycount.df)
  
  all(dailycount.df$total == dailycount.df$A+dailycount.df$B+dailycount.df$C)
  all(dailycount.df$A == dailycount.df$AA + dailycount.df$AB + dailycount.df$AC)
  all(dailycount.df$B == dailycount.df$BA + dailycount.df$BB + dailycount.df$Bc + dailycount.df$BD)
  all(dailycount.df$C == dailycount.df$CA)
  
}