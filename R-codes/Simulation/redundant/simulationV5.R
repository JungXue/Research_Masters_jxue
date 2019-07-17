
##########################################################################
#                                                                        #
# Simulation                                                             #
#                                                                        #
##########################################################################
# general hints cor coding:
# monitor time each chunk code took, 
# dont have to optimise chunks that used insignificant amount of time 
# use for loop is it doesnt take too long, 
# use interaction function
# debug and testing codes are contained within an empty  functions
#
# V1: rand.day.time
#     simdata (sim.number, time.start, time.end, cat1n, cat2n)
#     check simulation with cumulative plot and count plot
#
# V2: Fixed the bug that caused as.POSIXct to outout different time zone
#     simdata (cat2.val:Able to add custome matrix as terminal roots)
#
# V3: rand.day
#     simdata(added leaf variable, various debugging, cleanning)
#     contain test and debug codes in an empty function
#     cumdata
#     tabulatedata 
#
# V4: fig bug in tabulatedata 
#     Anomaly
#     
# V5: time.int
#     ggplot for cumulative counts
#     ggplot for daily counts
#
# V6: 

# packages
# qpcr
# minpack.lm
# Values rrquired for sim 


###################################################################################################

### Function 1 ###

### Create a number of random times

# rand.day.time originally by Dirk Eddelbuettel 2012
# Debugged by Thomas Lumley on 28 Oct 2018
# https://stackoverflow.com/questions/14720983/efficiently-generate-a-random-sample-of-times-and-dates-between-two-dates

rand.day.time <- function(N, st="2006/01/01 00:00:01", et="2018/12/31 23:59:59") {
    	 st <- as.POSIXct(strptime(st, format="%Y/%m/%d %H:%M:%S", tz="Pacific/Auckland"))
     	 et <- as.POSIXct(strptime(et, format="%Y/%m/%d %H:%M:%S", tz="Pacific/Auckland"))
       dt <- as.numeric(difftime(et, st, unit="sec"))
       ev <- sort(runif(N, 0, dt))
       rt <- st + ev
}

### testing
rand.day.time.test <- function(){

time.start = "2006/01/01 00:00:01"
time.end = "2018/12/31 23:59:59"
print(rand.day.time(5,time.start,time.end)) 

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

}
##########################################################################################################

### Function 2 ###

### Create random day 

rand.day <- function(N = 1, st = "2006/01/01 00:00:01", et = "2018/12/31 23:59:59"){
day = as.character(as.Date(rand.day.time(N, st, et)), units = "days")
return(day)
}

### testing 
rand.day.test <- function(){
  
rand.day()
rand.day(1)
str(rand.day(1))

rand.day(2)
length(rand.day(2))  

}
############################################################################################################

### Function 3 ###

### Function to simulate raw data

# 1,000 to 1,000,000 simulation recommended 
# time consuming parts
   # create id
   # as.character(as.date(time))
   # ifelse to assign leaf dummies

###################################################

### codes for debugging simdata
simdata.debug <- function(){
  
sim.number = 1000000
time.start = "2006/01/01 00:00:01"
time.end = "2018/12/31 23:59:59"

cat2.val = qpcR:::cbind.na(             
  c(2,2),                       
  c(2,2))

cat2.val = qpcR:::cbind.na(             
  c(2,2),                       
  c(2))

cat2.val = qpcR:::cbind.na(             
  c(200,200,100),                       
  c(200,200,  0),
  c(100,  0,  0))

cat2.val
str(cat2.val)
dim(cat2.val)
}
###################################################

simdata <-function(sim.number = 1000000,                   # number of simulations
                   repeats = 1,                            # how many time we run simulation and take mean
                                                           # Is this useful? how to do this????
                   time.start = "2006/01/01 00:00:01",     # start and end of period 
                   time.end = "2018/12/31 23:59:59",
                   cat2.val = qpcR:::cbind.na(             # default number in leafs in matrix, sum to 1000
                     c(250,250),                           # Use 0 for empty values in matrix 
                     c(250,250))){                         
  
###error checking
  
  if (sim.number > 10000000)
    stop("too many simulations, may be too slow")
  if (any(is.na(as.vector(cat2.val))) == T)
    stop("cat2.val should not contain NA, replace NA with 0")

#  x=as.vector(cat2.val)
#  if (!is.numeric(x) || !all(is.finite(x) || x < 0))
#    stop("invalid matrix values, use real numbers")
    
### identifiers/key variables
  
index = 1:sim.number 
EventID = sprintf("%08d", sample(1:paste(rep(9,8), collapse=""), 
                  sim.number,replace=F))
 
### select random times
#
rand.day.time  <- function(N, st=time.start, et = time.end) {
    st <- as.POSIXct(strptime(st, format="%Y/%m/%d %H:%M:%S", tz="Pacific/Auckland"))
    et <- as.POSIXct(strptime(et, format="%Y/%m/%d %H:%M:%S", tz="Pacific/Auckland"))
    dt <- as.numeric(difftime(et, st, unit="sec"))
    ev <- sort(runif(N, 0, dt)) #<------------------------------- add seasonality here???
    rt <- st + ev
}

time = rand.day.time(sim.number, time.start, time.end)
time.char = as.character(time)
time.day = as.character(as.Date(time.char, units = "days"))  #This is taking some time!!!
time.int = as.integer(as.Date(time.char, units = "days"))    #This is taking some time!!!
head(time)
head(time.int)
#----------------------------------------------------------#
### simulate level 1 and 2 brunch using leaf matrix

cat1.val = colSums(cat2.val, na.rm=T)
cat2.tot = sum(colSums(!is.na(cat2.val)))

cat1.n = length(cat1.val)
cat2.n = nrow(cat2.val)
  
cat1.let = c(LETTERS[1:cat1.n])
cat2.let = c(LETTERS[1:cat2.n])

sim.cat1 = rep(cat1.let, cat1.val)
cat1 = sample(sim.cat1 , sim.number, replace = TRUE)
cat2 = rep(NA,sim.number)
tabulate(as.factor(sim.cat1))
tabulate(as.factor(cat1))

Category = cbind(cat1, cat2)
head(Category)

for(i in 1:cat2.n){
  Category[which(Category[,1] == LETTERS[i]),2] = sample(rep(cat2.let, cat2.val[,i]), 
          sum(Category[,1] == LETTERS[i]), replace = TRUE)
}
head(Category)
table(Category[,1], Category[,2])

Category = as.data.frame(Category)
Category$leaf = paste(Category$cat1, Category$cat2, sep = "")
head(Category)

###assign name and value to dummy variable, name same as value

cat1.name = cat1.let
cat1.name
cat2.name = levels(interaction(cat1.let, cat2.let, sep = "", lex.order = TRUE))
cat2.name 
names = c(cat1.name, cat2.name)
names

substr(cat2.name,1,1)
substr(cat2.name,2,2)

for(i in 1:(cat1.n+cat1.n*cat2.n)){
Category[,i+3] = rep(names[i],sim.number)
}
colnames(Category) = c("cat1","cat2","leaf",cat1.name,cat2.name)
head(Category)

### use ifelse to assign dummies variables

for(i in 1:cat1.n){
  Category[,i+3] = ifelse(Category$cat1 == Category[,i+3], 1, 0)
}

for(i in 1:(cat1.n*cat2.n)){
  Category[,i+3+cat1.n] = ifelse(Category$cat1 == substr(Category[,i+3+cat1.n],1,1) &
                                 Category$cat2 == substr(Category[,i+3+cat1.n],2,2), 1, 0)
}
head(Category)

### delete columns if column sum is 0

dummies = Category[-(1:3)]
cat2.val
colSums(dummies)

emptyleaf = which(colSums(dummies) == 0)

if(any(colSums(dummies) == 0))
  warning("some of the leaf have 0 value")

if(any(colSums(dummies)==0)) {
  Category = Category[-(emptyleaf+3)]
} else {
  Category = Category
}
head(Category)




### create dataframe

sim.data.df = data.frame(cbind(index, EventID, time.char, time.day, time.int, Category)) # this is taking some time
sim.data.df$index     = as.numeric  (sim.data.df$index)
sim.data.df$EventID   = as.character(sim.data.df$EventID)
sim.data.df$time.char = as.character(sim.data.df$time.char)
sim.data.df$time.day  = as.Date     (sim.data.df$time.day)
sim.data.df$time.int  = as.numeric  (sim.data.df$time.int)

return(sim.data.df)
# head(sim.data.df)
} 


###Extras

#add categorical variables by random sampling? 
#add contineous variables by random sampling??? or by using some kind of model??

#######################################################################################

###Testing 
simdata.test <- function(){
  
test1 = simdata (1000)
head(test1)
str(test1)
colSums(test1[,9:ncol(test1)])    

cat2.val = cbind(                       # number in terminal roots, sum to 1000
  c(200,200,100),                 
  c(200,200,  0),
  c(100,  0,  0))

test2 = simdata (1000,cat2.val = cat2.val)
head(test2)
str(test2)
colSums(test2[,9:ncol(test2)])

test3 = simdata (1000000,cat2.val = cat2.val)
head(test3)
str(test3)

system.time(simdata (1000))
system.time(simdata (1000000))

# > system.time(simdata (1000))
# user  system elapsed 
# 0.13    0.02    0.14 
#
# > system.time(simdata (1000000))
# user  system elapsed 
# 99.86    1.32  102.25 

}
###################################################################################################

###################################################################################################

# Function 5, 6

## functions to check simulation
## use ggplot 2 later if we want a good presentable plot

# create a function for tabulation
#################################################

#debug for simulation checking codes

  
cat2.val = cbind(            
  c(200,200,100),                 
  c(200,200,  0),
  c(100,  0,  0))

sample1 = simdata (100000,cat2.val = cat2.val)
head(sample1)
str(sample1)
x = sample1

##################################################

### cumulative data 

cumdata<-function(x){
  catnames = colnames(x)[-(1:8)]
  for(i in 1:length(catnames)){
    x[8+i] = cumsum(x[8+i])
  }
  return(x)
} 

  cumdata.df = cumdata(sample1)
  head(cumdata.df)
  
 
  #create cumulative plot

  Nplot = nrow(cat2.val)*(1+nrow(cat2.val))

  par(mfrow=c(nrow(cat2.val)+1,nrow(cat2.val)))
  plot(cumdata.df$A,col="blue",type = "s",lwd=2)
  plot(cumdata.df$B,col="blue",type = "s",lwd=2)
  plot(cumdata.df$C,col="blue",type = "s",lwd=2)
  plot(cumdata.df$AA,col="blue",type = "s",lwd=2)
  plot(cumdata.df$AB,col="blue",type = "s",lwd=2)
  plot(cumdata.df$AC,col="blue",type = "s",lwd=2)
  plot(cumdata.df$BA,col="blue",type = "s",lwd=2)
  plot(cumdata.df$BB,col="blue",type = "s",lwd=2)
  plot(cumdata.df$BC,col="blue",type = "s",lwd=2)
  plot(cumdata.df$CA,col="blue",type = "s",lwd=2)
  plot(cumdata.df$CB,col="blue",type = "s",lwd=2)
  plot(cumdata.df$CC,col="blue",type = "s",lwd=2)
  par(mfrow=c(1,1))


# library(dplyr)
# library(tidyr)
# library(ggplot2)

head(cumdata.df)

nB1 = nrow(cat2.val)
nleaf = ncol(cumdata.df) - 8 - nB1


cumLeaf.df = cumdata.df[,c(8,(8+nB1+1):(8+nB1+nleaf))]
cumLeaf.df[1:10,]
cumCat1.df = cumdata.df[,c(6,(6+nB1):(6+2*nB1-1))]
cumCat1.df [1:10,]

gatherLeaf.df <- gather(cumLeaf.df,key = 'parameterLeaf',value = 'Value',2:7)
gatherLeaf.df$index = rep(1:nrow(cumdata.df),nleaf)
gatherLeaf.df[1:10,]

gatherB1.df <- gather(cumCat1.df,key = 'parameterB1',value = 'Value',2:(2+(nrow(cat2.val)-1)))
gatherB1.df$index = rep(1:nrow(cumdata.df),nB1)
gatherB1.df[1:10,]

#library(ggplot2)
  ggplot(gatherLeaf.df,aes(x = index, y = Value, group = parameterLeaf, col = parameterLeaf)) + 
    geom_step() + 
    facet_grid(parameterLeaf ~ .) +       #cat1 for other plot
    ggtitle("Cumulative plot for the leafs") +
    ylab("Cumulative counts") + 
    xlab("index") 
  
  ggplot(gatherB1.df,aes(x = index, y = Value, group = parameterB1, col = parameterB1)) + 
    geom_step() + 
    facet_grid(parameterB1 ~ .) +       #cat1 for other plot
    ggtitle("Cumulative plot for the first brunchs") +
    ylab("Cumulative counts") + 
    xlab("index") 
  
  
#################################################
  
###tabulate date, ie daily counts

tabulatedata <- function(x=sample1){

  time.start = "2006/01/01 00:00:01"
  time.end = "2018/12/31 23:59:59"
    
  time.start.int = as.integer(as.Date(time.start, units = "days")) 
  time.end.int = as.integer(as.Date(time.end , units = "days")) 
  time.start.int
  time.end.int
  
  library("zoo")
  timelevel =  as.factor(as.Date(time.start.int:time.end.int))
  head(timelevel)
  tail(timelevel)

  datedata = factor(as.Date(x$time.day),levels=timelevel)
  str(datedata)
  
  daily.df = data.frame(matrix(NA,nrow = length(timelevel),ncol = length(x)-8+2,
                        dimnames = list(c(1:length(timelevel)),catname)))
  head(daily.df)
  
    for(i in 1:(ncol(daily.df)-2)){
      daily.df[,1] = timelevel 
      daily.df[,2] = tabulate(datedata)
      daily.df[,(2+i)] = table(datedata,x[,(8+i)])[,2]
    }
  
  catname = c("day","total",names(x)[9:length(x)])
  catname
  colnames(daily.df) = catname
  
  daily.df[1:20,]

  return(daily.df)
}
  
dailycount.df = tabulatedata(sample1)
head(dailycount.df)

##? why does it not work inside an function

### checks
# all(daily.df$total == daily.df$A+daily.df$B+daily.df$C)
# all(daily.df$A == daily.df$AA +daily.df$AB + daily.df$AC)
# all(daily.df$B == daily.df$BA +daily.df$BB)
# all(daily.df$C == daily.df$CA)

dailycountleaf.df = dailycount.df[,c(1,(3+nB1):length(dailycount.df))]
dailycountleaf.df[1:10,]
dailycountbr1.df = dailycount.df[,c(1,3:(3+nB1-1))]
dailycountbr1.df [1:10,]

gatherdailyLeaf.df <- gather(dailycountleaf.df,key = 'paraLeaf',value = 'count',2:length(dailycountleaf.df))
gatherdailyLeaf.df[1:10,]

gatherdailyB1.df <- gather(dailycountbr1.df,key = 'paraB1',value = 'count',2:length(dailycountbr1.df))
gatherdailyB1.df[1:10,]


#library(ggplot2)
ggplot(gatherdailyLeaf.df,aes(x = day, y = count, group = paraLeaf, col = paraLeaf)) + 
  geom_step() + 
  facet_grid(paraLeaf ~ .) +       
  ggtitle("dailycount plot for the leafs") +
  ylab("Cumulative counts") + 
  xlab("time") 

ggplot(gatherdailyB1.df,aes(x = day, y = count, group = paraB1, col = paraB1)) + 
  geom_step() + 
  facet_grid(paraB1 ~ .) +       
  ggtitle("dailycount plot for the first brunchs") +
  ylab("Cumulative counts") + 
  xlab("time") 


#################################################################






















### Function 7 & 8
### ddition of anomalies


#at count data level


#at raw data level

#howmuch to add o decrease???


rand.day()

simdata <-function(sim.number = 1000000,                   # number of simulations
                   time.start = "2006/01/01 00:00:01",     # period
                   time.end = "2018/12/31 23:59:59",
                   cat2.val = qpcR:::cbind.na(             # number in terminal roots, sum to 1000
                     c(250,250),
                     c(250,250))){                         # Use 0 for empty values in matrix 
  
  
day = rand.day 

point.anomaly <- function(data, anomalyCount = 99, day = "2006/01/01", whichroot=c("A","A")){
  #return warning day must be in a character as YYYY/MM/DD format
  point.anomaly.time.start = paste(day,"00:00:01")
  point.anomaly.time.end = paste(day,"23:59:59")
  whichroot2 = whichroot[1]
               whichroot[1]
  
  
anomaly = simdata(sim.number = anomalyCount,time.start = ,time.end =, cat2.val =whichroot2,)
return(anomaly )
}


period.anomaly <- function(data, start.day = NA,end.day = NA, 
                    anomalyCount = 9999, distribution = "Normal"){
  distribution = 
  data[,] = data[,]+anomalyCount
  return(data )
}
#####################################################################################################

###running simulations


####################################################################################################

###output files 

head(daily.df)
str(daily.df)
write.csv(daily.df,'dailydf.csv') 



