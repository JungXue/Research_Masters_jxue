
##########################################################################
#                                                                        #
# Simulation                                                             #
#                                                                        #
##########################################################################
# general hints cor coding:
# monitor time each chunk code took, 
# dont have to optimise chunks that used insignificant amount of time 
# use for loop isf it doesnt take too long, 
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
# Todo
# V4: fig bug in tabulatedata 
#     anomaly
#     
#packages
#qpcr
#minpack.lm
###################################################################################################

### Function 1 ###

###Create a number of random times

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

t.char = as.character(test0)                           # it is taking a while
t.day = as.character(as.Date(t.char, units = "days"))  # it is taking a while
dailycount = tabulate(as.factor(t.day))

plot(dailycount,type="l", col="blue")
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
}
###################################################

simdata <-function(sim.number = 1000000,                   # number of simulations
                   repeats =1,                             # how many time we run simulation and take mean  ##########
                                                           # how to do this????
                   time.start = "2006/01/01 00:00:01",     # start and end of period 
                   time.end = "2018/12/31 23:59:59",
                   cat2.val = qpcR:::cbind.na(             # number in leafs, sum to 1000
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
time.day = as.character(as.Date(time.char, units = "days"))

#----------------------------------------------------------#
### simulate level 1 and 2 brunch using terminal root matrix

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

emptyroot = which(colSums(dummies) == 0)
emptyroot2 = which(colSums(dummies) == 2) # <------------------------fix here for no col == 0
emptyroot
emptyroot2
test1 = Category[-(emptyroot+3)]
head(test1)
test2 =  Category
head(test2)

Category = Category[-(emptyroot+3)]
head(Category)


### create dataframe

sim.data.df = data.frame(cbind(index, EventID, time.char, time.day, Category))
sim.data.df$index     = as.numeric  (sim.data.df$index)
sim.data.df$EventID   = as.character(sim.data.df$EventID)
sim.data.df$time.char = as.character(sim.data.df$time.char)
sim.data.df$time.day  = as.Date     (sim.data.df$time.day)

return(sim.data.df)
} 


###Extras

#add categorical variables by random sampling? 
#add contineous variables by random sampling??? using some kind of model??

########################################################################################################3333#

###Testing 
simdata.test <- function(){
  
test1 = simdata (1000)
head(test1)
str(test1)
colSums(test1[,7:ncol(test1)])    

v2 = cbind(                       # number in terminal roots, sum to 1000
  c(200,200,100),                 
  c(200,200,  0),
  c(100,  0,  0))

test2 = simdata (1000,cat2.val = v2)
head(test2)
str(test2)
colSums(test2[,7:ncol(test2)])

test3 = simdata (1000000,cat2.val = v2)
head(test3)
str(test3)

system.time(simdata (1000))
system.time(simdata (1000000))

}
#########################################################################################################

###################################################################################################

# Function 5, 6

## functions to check simulation
## use ggplot 2 later if we want a good presentable plot

# create a function for tabulation
#################################################

#debug for simulation checking codes
simcheck.debug <- function(){
  
v2 = cbind(            
  c(200,200,100),                 
  c(200,200,  0),
  c(100,  0,  0))

sample1 = simdata (100000,cat2.val = v2)
head(sample1)
str(sample1)
x = sample1
}
##################################################

### cumulative data 

head(sample1)
x = sample1
cumdata<-function(x){
  catnames = colnames(x)[-(1:7)]
  for(i in 1:length(catnames)){
    x[7+i] = cumsum(x[7+i])
  }
  return(x)
} 

###testing
cumdata.test <- function(){
  cumdata.df = cumdata(sample1)
  head(cumdata.df)
  
  par(mfrow=c(3,3))
  #layout(matrix(c(1,1,2,3), 2, 2, byrow = TRUE), widths=c(3,1), heights=c(1,2))
  plot(cumdata.df$A,col="blue",type = "s",lwd=2)
  plot(cumdata.df$B,col="blue",type = "s",lwd=2)
  plot(cumdata.df$C,col="blue",type = "s",lwd=2)
  plot(cumdata.df$AA,col="blue",type = "s",lwd=2)
  plot(cumdata.df$AB,col="blue",type = "s",lwd=2)
  plot(cumdata.df$AC,col="blue",type = "s",lwd=2)
  plot(cumdata.df$BA,col="blue",type = "s",lwd=2)
  plot(cumdata.df$BB,col="blue",type = "s",lwd=2)
  plot(cumdata.df$CA,col="blue",type = "s",lwd=2)
  par(mfrow=c(1,1))
} 

#df.gathered <- gather(x[,7:16],key = 'parameter',value = 'Value',5:10)
# df.gathered [1:20,]
#  ggplot(df.gathered,aes(x = parameter, y = Value, group = leaf, col = leaf)) + 
#    geom_step() + 
#    facet_grid(leaf ~ .) +       #cat1 for other plot
#    ggtitle("New Plot Title") +
#    ylab("Cumulative counts") + 
#    xlab("index") 
  
###tab date, ie daily counts

tabulatedata <- function(x){

  head(x)
  catname = c("day","total",names(x)[8:length(x)])
  days = levels(as.factor(sample1$time.day))
  daily.df = data.frame(matrix(NA,nrow = length(days),ncol = length(x)-7+2,
                        dimnames = list(c(1:length(days)),catname)))
  head(daily.df)
  
    for(i in 1:length(catname)){
      daily.df[,1] = levels(as.factor(x$time.day))  
      daily.df[,2] = tabulate(as.factor(x$time.day))
      A = table(x$time.day,x[7+i])
      daily.df[,2+i] = A[(length(A)/2+1):length(A)]
    }
  head(daily.df)
  tail(daily.df)
  nrow(daily.df)
  return(daily.df)
}

tabulatedata(sample1)

##? why does it not work inside an function

### checks
# all(daily.df$total == daily.df$A+daily.df$B+daily.df$C)
# all(daily.df$A == daily.df$AA +daily.df$AB + daily.df$AC)
# all(daily.df$B == daily.df$BA +daily.df$BB)
# all(daily.df$C == daily.df$CA)






#check simulation
daily   = tabulate(as.factor(sample1$time.day))
dailyA  = tabulate(as.factor(sample1$time.day[test1$A==1]))
dailyB  = tabulate(as.factor(sample1$time.day[test1$B==1]))
dailyAA = tabulate(as.factor(sample1$time.day[test1$AA==1]))
dailyAB = tabulate(as.factor(sample1$time.day[test1$AB==1]))
dailyBA = tabulate(as.factor(sample1$time.day[test1$BA==1]))
dailyBB = tabulate(as.factor(sample1$time.day[test1$BB==1]))

library(scales)
plot(daily,type = "l",col="blue",ylim=c(0,300))
lines(dailyA,col="pink")
lines(dailyB,col="green")
lines(dailyAA,col=alpha("red",0.5))
lines(dailyAB,col=alpha("red",0.5))
lines(dailyBA,col=alpha("yellow",0.5))
lines(dailyBB,col=alpha("yellow",0.5))

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



