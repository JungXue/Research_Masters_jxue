
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
#
# V1: rand.day.time
#     simdata (sim.number, time.start, time.end, cat1n, cat2n)
#     check simulation with cumulkative plot and count plot
#
# V2: Fixed the bug that caused as.POSIXct to outout different time zone
#     Added Rand.day
#     simdata (cat2.val:Able to add custome matrix as terminal roots)
#     started on point anomalies
#
# V3:
#
#
#packages
#qpcr
#minpack.lm
###################################################################################################

### Function 1
# rand.day.time originally by Dirk Eddelbuettel 2012
# Debugged by Thomas Lumley
# https://stackoverflow.com/questions/14720983/efficiently-generate-a-random-sample-of-times-and-dates-between-two-dates

rand.day.time  <- function(N, st="2006/01/01 00:00:01", et="2018/12/31 23:59:59") {
    	 st <- as.POSIXct(strptime(st, format="%Y/%m/%d %H:%M:%S",tz="Pacific/Auckland"))
     	 et <- as.POSIXct(strptime(et, format="%Y/%m/%d %H:%M:%S",tz="Pacific/Auckland"))
       dt <- as.numeric(difftime(et,st,unit="sec"))
       ev <- sort(runif(N, 0, dt))
       rt <- st + ev
}

time.start = "2006/01/01 00:00:01"
time.end = "2018/12/31 23:59:59"
print(rand.day.time(5,time.start,time.end)) 

system.time(rand.day.time (1000))
system.time(rand.day.time (1000000))

test0 = rand.day.time (1000000,time.start,time.end)
head(test0)
tail(test0)
t.char = as.character(test0)                          # taking a while
t.day = as.character(as.Date(t.char,units = "days"))  # taking a while
dailycount  = tabulate(as.factor(t.day))

plot(dailycount,type="l",col="blue")
abline(h=mean(dailycount),col="red",lwd=2)
points(dailycount[1], pch=19,col="red")
points(length(dailycount),dailycount[length(dailycount)], pch=19,col="red")

##########################################################################################################

### Function 2 random day
rand.day <-function(N=1, st="2006/01/01 00:00:01", et="2018/12/31 23:59:59"){
day = as.character(print(rand.day.time(N,st,et)),units = "days")
return(day)
}

rand.day(1)
str(rand.day(1))

rand.day(2)
length(rand.day(2))                  #why is there a name? how to delete it? 

############################################################################################################
############################################################################################################

## Function 3

## Function to simulate data
# 1 million simulation recommended
# no less than 1000
# how to add warnings properly ???

#time consuming parts
   #create id
   #as.character(time)
   #ifelse to assign terminal root dummies
###################################################
## codes for debugging simdata
sss<-function(){
sim.number = 1000000
time.start = "2006/01/01 00:00:01"
time.end = "2018/12/31 23:59:59"

cat2.val = qpcR:::cbind.na(             
  c(2,2),                       
  c(2,2))

cat2.val = qpcR:::cbind.na(             
  c(2,2,1),                       
  c(2,0,0))

cat2.val = qpcR:::cbind.na(             
  c(2,2,1),                       
  c(2,2,0),
  c(1,0,0))

cat2.val = qpcR:::cbind.na(             
  c(200,200,100),                       
  c(200,200,  0),
  c(100,  0,  0))

cat2.val
str(cat2.val)
}
###################################################

simdata <-function(sim.number = 1000000,                   # number of simulations
                   time.start = "2006/01/01 00:00:01",     # period
                   time.end = "2018/12/31 23:59:59",
                   cat2.val = qpcR:::cbind.na(             # number in terminal roots, sum to 1000
                     c(250,250),
                     c(250,250))){                         # Use 0 for empty values in matrix 
  
###error checking
 # if (sim.number > 10000000)
 #   stop("too many simulations, may be too slow")
  
### identifiers/key variables
  index = 1:sim.number 
  EventID = sprintf("%08d", sample(1:paste(rep(9,8), collapse=""), 
                                   sim.number,replace=F))
 
### select random times
  rand.day.time  <- function(N, st=time.start, et = time.end) {
    st <- as.POSIXct(strptime(st, format="%Y/%m/%d %H:%M:%S",tz="Pacific/Auckland"))
    et <- as.POSIXct(strptime(et, format="%Y/%m/%d %H:%M:%S",tz="Pacific/Auckland"))
    dt <- as.numeric(difftime(et,st,unit="sec"))
    ev <- sort(runif(N, 0, dt)) ##########3<----------delete sort??? or add seasonality???
    rt <- st + ev
  }
time = rand.day.time(sim.number, time.start, time.end)
time.char = as.character(time)
time.day = as.character(as.Date(time.char,units = "days"))

#----------------------------------------------------------#
### simulate level 1 and 2 brunch using terminal root matrix

cat1.val = colSums(cat2.val,na.rm=T)
cat1.n = length(cat1.val)

cat2.tot = sum(colSums(!is.na(cat2.val)))
cat2.n = nrow(cat2.val)
  
cat1.let = c(LETTERS[1:cat1.n])
cat2.let = c(LETTERS[1:cat2.n])

sim.cat1 = rep(cat1.let,cat1.val)
tabulate(as.factor(sim.cat1))
cat1 = sample(sim.cat1 , sim.number, replace = TRUE)
tabulate(as.factor(cat1))

cat2 = rep(NA,sim.number)
Category = cbind(cat1,cat2)
head(Category)

for(i in 1:cat2.n){
  Category[which(Category[,1] == LETTERS[i]),2] = sample(rep(cat2.let,cat2.val[,i]), 
          sum(Category[,1] == LETTERS[i]), replace = TRUE)
}

Category[1:20,]
table(Category[,1], Category[,2])

###assign name and value to dummy variable, name same as value

cat1.name = cat1.let
cat1.name
cat2.name = levels(interaction(cat1.let,cat2.let,sep = "", lex.order = TRUE))
cat2.name 

substr(cat2.name,1,1)
substr(cat2.name,2,2)
names = c(cat1.name,cat2.name)
Category = as.data.frame(Category)

for(i in 1:(cat1.n+cat1.n*cat2.n)){
Category[,i+2] = rep(names[i],sim.number)
}

colnames(Category) = c("cat1","cat2",cat1.name,cat2.name)
head(Category)

### use ifelse to assign dummies variables

for(i in 1:cat1.n){
  Category[,i+2] = ifelse(Category$cat1 == Category[,i+2], 1, 0)
}

for(i in 1:(cat1.n*cat2.n)){
  Category[,i+2+cat1.n] = ifelse(Category$cat1 == substr(Category[,i+2+cat1.n],1,1) &
                                 Category$cat2 == substr(Category[,i+2+cat1.n],2,2), 1, 0)
}
head(Category)

### delete columns if column sum is 0

dummies = Category[-(1:2)]
cat2.val
colSums(dummies)
emptyroot = which(colSums(dummies) == 0)            #<------if no empty terminal root i get a error

Category = Category[-(emptyroot+2)]
head(Category)

### create dataframe

sim.data.df = data.frame(cbind(index, EventID, time.char, time.day, Category))
sim.data.df$index     = as.numeric  (sim.data.df$index)
sim.data.df$EventID   = as.character(sim.data.df$EventID)
sim.data.df$time.char = as.character(sim.data.df$time.char)
sim.data.df$time.day  = as.Date     (sim.data.df$time.day)

return(sim.data.df)
} 

########################################################################################################3333#

test1 = simdata (1000)
head(test1)
str(test1)
colSums(test1[,7:ncol(test1)])    

v2 = qpcR:::cbind.na(             # number in terminal roots, sum to 1000
  c(200,200,100),                 
  c(200,200,  0),
  c(100,  0,  0))

v2 = cbind(                       # number in terminal roots, sum to 1000
  c(200,200,100),                 
  c(200,200,  0),
  c(100,  0,  0))

test2 = simdata (1000,cat2.val = v2)
head(test2)
str(test2)
colSums(test2[,7:ncol(test2)])


test1 = simdata (1000000)
head(test1)

# system.time(simdata (1000))
# system.time(simdata (1000000))

#########################################################################################################

###################################################################################################

# Function 5
## check simulation
##use ggplot 2 later if we want a good presentable plot

#create 1 plot for up to 12 A-Zs
#Subsequent plot for AA,AB...ZZs
#sim plot, normal plot, value of n, mean and SD
# add custom palette
# ________________________________________
#|                      |     | n
#|                      |     | mean
#|                      |     | SD
#|______________________|     |_______________
# create a function for tabulation


sample1 = test1[1:10000,]

#tabulate()

# daily counts
#daily.df = 
# cumulative plot

cumplot.df = sample1[,c(-1,-3),]

cumplot.df$A  = cumsum(cumplot.df$A)
cumplot.df$B  = cumsum(cumplot.df$B)
cumplot.df$AA = cumsum(cumplot.df$AA)
cumplot.df$AB = cumsum(cumplot.df$AB)
cumplot.df$BA = cumsum(cumplot.df$BA)
cumplot.df$BB = cumsum(cumplot.df$BB)


head(cumplot.df)

plot(cumplot.df$A,col="blue",type = "l",lwd=2)
lines(cumplot.df$B,col="red",type = "l",lwd=2)
lines(cumplot.df$AA,col="lightblue",type = "l")
lines(cumplot.df$AB,col="lightblue",type = "l")
lines(cumplot.df$BA,col="pink",type = "l")
lines(cumplot.df$BB,col="pink",type = "l")


#check simulation
daily   = tabulate(as.factor(test1$time.day))
dailyA  = tabulate(as.factor(test1$time.day[test1$A==1]))
dailyB  = tabulate(as.factor(test1$time.day[test1$B==1]))
dailyAA = tabulate(as.factor(test1$time.day[test1$AA==1]))
dailyAB = tabulate(as.factor(test1$time.day[test1$AB==1]))
dailyBA = tabulate(as.factor(test1$time.day[test1$BA==1]))
dailyBB = tabulate(as.factor(test1$time.day[test1$BB==1]))

library(scales)
plot(daily,type = "l",col="blue",ylim=c(0,300))
lines(dailyA,col="pink")
lines(dailyB,col="green")
lines(dailyAA,col=alpha("red",0.5))
lines(dailyAB,col=alpha("red",0.5))
lines(dailyBA,col=alpha("yellow",0.5))
lines(dailyBB,col=alpha("yellow",0.5))

#################################################################


#Function 5 & 6
#Addition of anomalies

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

#Function 6
#add categorical variables by random sampling
#add contineous variables by random sampling??? using some kind of model??
#add name to vector and extract name to named vector
Gender = c(50,50)

#how to create pool? use some kind of model?

#Function 7



# For simulated data
# how to add variables such as demographis
# how to add trends such as season etc?

####################################################################

###create a dataframe or dump that is good for bug jag stan


