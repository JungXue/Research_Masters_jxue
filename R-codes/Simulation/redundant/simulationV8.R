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
# V6: Bug in simdata
#     Export data
#
# V7: make ggdailyplot and ggcumplot into functions
#
# V8: Add anomalies

# Premeables

library(dplyr)
library(tidyr)
library(ggplot2)
library(qpcR)


##########################################################################

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

}
##########################################################################

### Function 2 ###

### Create random day 

rand.day <- function(N = 1, st = "2006/01/01 00:00:01", et = "2018/12/31 23:59:59"){
day = as.character(as.Date(rand.day.time(N, st, et)), units = "days")
return(day)
}



### testing 
rand.day.test <- function(){
  
  time.start = "2006/01/01 00:00:01" 
  time.end = "2006/01/03 23:59:59"
  N=10
  
  
rand.day(10,time.start,time.end)

rand.day(1)
str(rand.day(1))

rand.day(2)
length(rand.day(2))  

}
##########################################################################

### Function 3 ###

### Function to simulate raw data

# 1,000 to 1,000,000 simulation recommended 
# time consuming parts
   # create id
   # as.character(as.date(time))
   # ifelse to assign leaf dummies


### codes for debugging simdata
simdata.test <- function(){
  cat2.val= cbind(            
    c(200,185,100,  0),                 
    c(150,200, 50, 15),
    c(100,  0,  0,  0))
  sim.number=1000
  time.start = "2006/01/01 00:00:01"
  time.end = "2018/12/31 23:59:59"
}
  

simdata <-function(sim.number = 1000000,                   # number of simulations
                   repeats = 1,                            # how many time we run simulation and take mean
                                                           # Is this useful? how to do this????
                   time.start = "2006/01/01 00:00:01",     # start and end of period 
                   time.end = "2018/12/31 23:59:59",
                   cat2.val = qpcR:::rbind.na(             # default number in leafs in matrix, sum to 1000
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
cat1.val
cat2.tot

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

for(i in 1:cat1.n){
  Category[which(Category[,1] == LETTERS[i]),2] = 
    sample(rep(cat2.let, cat2.val[,i]), 
           sum(Category[,1] == LETTERS[i]), 
           replace = TRUE)
}

i=3
head(Category)
table(Category[,1], Category[,2])

Category = as.data.frame(Category)
Category$leaf = factor(paste(Category$cat1, Category$cat2, sep = ""))
head(Category)
str(Category)


###assign name and value to dummy variable, name same as value

cat1.name = cat1.let
cat1.name
expandLet = expand.grid(cat1.let,cat2.let)
cat2.name  =  paste(expandLet[,1],expandLet[,2], sep = "")
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

# if(any(colSums(dummies) == 0))
#   message("some of the leaf have 0 value, assumed to be null and deleted, if a leaf exist with 0, add manually with data$leaf at the end ")

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


###Testing simdata
simdata.test2 <- function(){
  
test1 = simdata (1000)
head(test1)
str(test1)
colSums(test1[,9:ncol(test1)])    

cat2.val = rbind(                       # number in terminal roots, sum to 1000
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

##########################################################################

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

##########################################################################

### Function 5 ###

### codes for debugging ggcumplot 

ggcumplot.test <- function(){
  
  cat2.val = rbind(            
    c(200,200,100),                 
    c(200,200, 50),
    c(100,  0,  0))
  
  cat2.val= cbind(            
    c(200,185,100,  0),                 
    c(150,200, 50, 15),
    c(100,  0,  0,  0))
  cat2.val = cbind(            
    c(200,200),                 
    c(100,500))
  
  sample1 = simdata (100000,cat2.val = cat2.val)
  time.start = "2006/01/01 00:00:01"
  time.end = "2018/12/31 23:59:59"
  
  head(sample1)
  str(sample1)
  x = sample1
  
  cumdata.df = cumdata(sample1)
  head(cumdata.df)
  
  data=cumdata.df
  
}



ggcumplot <- function(data=cumdata.df,
                      nameB1= "ggcumplotB1.jpg",
                      nameLeaf= "ggcumplotleaf.jpg"){

  library(ggplot2)
 
  nB1 =  length(levels(data$cat1))
  nleaf = ncol(data) - 8 - nB1
  head(data)
  cumLeaf.df = data[,c(8,(8+nB1+1):(8+nB1+nleaf))]
  cumLeaf.df[1:10,]
  cumCat1.df = data[,c(6,9:(9+nB1-1))]
  cumCat1.df [1:10,]
  
  gatherLeaf.df <- gather(cumLeaf.df,key = 'parameterLeaf',value = 'Value',2:length(cumLeaf.df))
  gatherLeaf.df$index = rep(1:nrow(data),nleaf)
  gatherLeaf.df[1:10,]
  
  gatherB1.df <- gather(cumCat1.df,key = 'parameterB1',value = 'Value',2:length(cumCat1.df ))
  gatherB1.df$index = rep(1:nrow(data),nB1)
  gatherB1.df[1:10,]
  
jpeg(nameLeaf, width = 800, height = nleaf*100+100)
  plot1 = ggplot(gatherLeaf.df,aes(x = index, y = Value, group = parameterLeaf, col = parameterLeaf)) + 
    geom_step() + 
    facet_grid(parameterLeaf ~ .) +       #cat1 for other plot
    ggtitle("Cumulative plot for the leafs") +
    ylab("Cumulative counts") + 
    xlab("index")
  print(plot1)
dev.off()

jpeg(nameB1, width = 800, height = nB1*200+100)
  plot2 = ggplot(gatherB1.df,aes(x = index, y = Value, group = parameterB1, col = parameterB1)) + 
    geom_step() + 
    facet_grid(parameterB1 ~ .) +       #cat1 for other plot
    ggtitle("Cumulative plot for the first brunchs") +
    ylab("Cumulative counts") + 
    xlab("index") 
  print(plot2)
dev.off()
}

### Test ggcumplot
ggcumplot.test2 <- function(){

ggcumplot(cumdata.df)

}


#################################################
### Function 6 ###

### Function to create cumulative and daily data


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
}

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

  catname = c("day","total",names(x)[9:length(x)])
  catname
  
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
  
  colnames(daily.df) = catname

  daily.df[1:20,]

  return(daily.df)
  return()
}


### checks on tabulatedata

  
tabulatedata.test <- function(){
  
dailycount.df = tabulatedata(sample1)
head(dailycount.df)

all(dailycount.df$total == dailycount.df$A+dailycount.df$B+dailycount.df$C)
all(dailycount.df$A == dailycount.df$AA +dailycount.df$AB + dailycount.df$AC)
all(dailycount.df$B == dailycount.df$BA +dailycount.df$BB)
all(dailycount.df$C == dailycount.df$CA)

}

##################################################

###Function 7 ###

### Function to create daily plot

ggdailyplot.test <- function(){
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
  
  dailycount.df = tabulatedata(sample1)
  data=dailycount.df
  head(data)
}

ggdailyplot <- function(data = dailycount.df,
                      nameB1= "ggdailyplotB1.jpg",
                      nameLeaf= "ggdailyplotleaf.jpg"){
  
library(ggplot2)

namez = table(nchar(names(data)))
nB1 =  namez[1]
nleaf = namez[2]
  
dailycountleaf.df = data [,c(1,(3+nB1):length(data))]
dailycountleaf.df[1:10,]
dailycountbr1.df = data [,c(1,3:(3+nB1-1))]
dailycountbr1.df [1:10,]

gatherdailyLeaf.df <- gather(dailycountleaf.df,key = 'paraLeaf',value = 'count',2:length(dailycountleaf.df))
gatherdailyLeaf.df[1:10,]
gatherdailyB1.df <- gather(dailycountbr1.df,key = 'paraB1',value = 'count',2:length(dailycountbr1.df))
gatherdailyB1.df[1:10,]


jpeg(nameLeaf, width = 800, height = nleaf*100+100)
plot1 = ggplot(gatherdailyLeaf.df,aes(x = day, y = count, group = paraLeaf, col = paraLeaf)) + 
  geom_step() + 
  facet_grid(paraLeaf ~ .) +       
  ggtitle("dailycount plot for the leafs") +
  ylab("Daily counts") + 
  xlab("time") 
print(plot1)
dev.off()

jpeg(nameB1, width = 800, height = nB1*200+100)
plot2 = ggplot(gatherdailyB1.df,aes(x = day, y = count, group = paraB1, col = paraB1)) + 
  geom_step() + 
  facet_grid(paraB1 ~ .) +       
  ggtitle("dailycount plot for the first brunchs") +
  ylab("Daily counts") + 
  xlab("time") 
print(plot2)
dev.off()
}

#################################################################
### output files 


matrix1 = cbind(            
  c(200,200),                 
  c(100,500))

matrix2 = cbind(            
  c(200,185,100,  0),                 
  c(150,200, 50, 15),
  c(100,  0,  0,  0))

x=sample.int(1000, 4)
matrix3= matrix(x,nrow=2,byrow = T)           

x=sample.int(1000, 9)
matrix4= matrix(x,nrow=3,byrow = T)

matrix1
matrix2
matrix3
matrix4

t.start = "2006/01/01 00:00:01"   
t.end = "2018/12/31 23:59:59"

raw1.df = simdata (10000,cat2.val = matrix1, time.start = t.start, time.end = t.end)
raw2.df = simdata (10000,cat2.val = matrix2, time.start = t.start, time.end = t.end)
raw3.df = simdata (10000,cat2.val = matrix3, time.start = t.start, time.end = t.end)
raw4.df = simdata (10000,cat2.val = matrix4, time.start = t.start, time.end = t.end)

head(raw1.df)
head(raw2.df)
head(raw3.df)
head(raw4.df)

cum1.df = cumdata(raw1.df)
cum2.df = cumdata(raw2.df)
cum3.df = cumdata(raw3.df)
cum4.df = cumdata(raw4.df)

head(cum1.df)
head(cum2.df)
head(cum3.df)
head(cum4.df)


daily1.df = tabulatedata(raw1.df)
daily2.df = tabulatedata(raw2.df) 
daily3.df = tabulatedata(raw3.df)
daily4.df = tabulatedata(raw4.df)

#Sometime have these errors
# Error in `[<-.data.frame`(`*tmp*`, , 2, value = c(4L, 1L, 2L, 3L, 1L,  : 
# replacement has 4747 rows, data has 4748 

head(daily1.df)
head(daily2.df)
head(daily3.df)
head(daily4.df)

###############################



write.csv(raw1.df,'raw1.csv') 
write.csv(raw2.df,'raw2.csv') 
write.csv(raw3.df,'raw3.csv') 
write.csv(raw4.df,'raw4.csv') 

write.csv(cum1.df,'cum1.csv') 
write.csv(cum2.df,'cum2.csv') 
write.csv(cum3.df,'cum3.csv') 
write.csv(cum4.df,'cum4.csv') 

write.csv(daily1.df,'daily1.csv') 
write.csv(daily2.df,'daily2.csv') 
write.csv(daily3.df,'daily3.csv') 
write.csv(daily4.df,'daily4.csv') 


###############################

ggcumplot(cum1.df,nameB1= "ggcumplotB1a.jpg",nameLeaf= "ggcumplotleafa.jpg")
ggcumplot(cum2.df,nameB1= "ggcumplotB1b.jpg",nameLeaf= "ggcumplotleafb.jpg")
ggcumplot(cum3.df,nameB1= "ggcumplotB1c.jpg",nameLeaf= "ggcumplotleafc.jpg")
ggcumplot(cum4.df,nameB1= "ggcumplotB1d.jpg",nameLeaf= "ggcumplotleafd.jpg")

ggdailyplot(daily1.df,nameB1= "ggdailyplotB1a.jpg",nameLeaf= "ggdailyplotleafa.jpg")
ggdailyplot(daily2.df,nameB1= "ggdailyplotB1b.jpg",nameLeaf= "ggdailyplotleafb.jpg")
ggdailyplot(daily3.df,nameB1= "ggdailyplotB1c.jpg",nameLeaf= "ggdailyplotleafc.jpg")
ggdailyplot(daily4.df,nameB1= "ggdailyplotB1d.jpg",nameLeaf= "ggdailyplotleafd.jpg")



###############################
head(raw1.df)
head(raw2.df)
head(raw3.df)
head(raw4.df)

head(daily1.df)
head(daily2.df)
head(daily3.df)
head(daily4.df)



###########################################################################

  


daily.anomaly<- function(data = daily1.df,
                         type = "point",   #point of period anomaly
                         day = rand.day(1),
                         Leaf = c("AA","AB","BA","BB"),
                         Leafpercent = c(0.25,0.25,0.25,0.25),
                         TotalN = 100,
                         usepercent=T,
                         TotalNpercent = 100,
                         Distribution = "Normal",
                         output = 'dailyanomaly.csv') #
{
  
  #original data
  originaldaily = daily1.df
  
  #0 value data
  adddaily = daily1.df
    for (i in 1:nrow(daily1.df)){
    for (j in 2:ncol(daily1.df)){
      adddaily[i,j] = 0
    }
  }
  head(adddaily)
    
  day = "2010-05-01"
  Leaf = c("AA","AB","BA","BB")
  
  
  leaf = cbind(c("AA","AB","BA","BB"),
               c("AA","BA","BB","BB"))
  Leafpercent = cbind(c(0.25,0.25,0.25,0.25),
                      c(0.2 ,0.8 ,0   ,0   ))
                      
  
 
  nleaf = length(Leaf)
  probs = switch(Distribution,"Normal"  = dnorm(1:nleaf,mean = (1+nleaf)/2, sd = sd(1:nleaf)),#???right
                              "Uniform" = dunif(1:nleaf,1,nleaf+1))
  
  addcounts = sample(1:nleaf,100,replace = TRUE,prob=probs)
  addcountstab = table(addcounts )
  addcountstab 
  plot(addcountstab )
  
  anomalymatrix = matrix(addcountstab ,nrow=length(day),ncol=length(Leaf),byrow =T)
  row.names(anomalymatrix) = day
  colnames(anomalymatrix) = Leaf
  anomalymatrix = data.frame(anomalymatrix)
  anomalymatrix
  
  for (i in 1:nrow(daily1.df)){
    for (j in 2:ncol(daily1.df)){
      adddaily[i,j] = adddaily[i,j]+1
    }
  }
  
  

  
  originaldaily + anomalymatrix
  
  for (i in 1:nrow(adddaily)){
    for (j in 2:ncol(adddaily)){
      adddaily[i,j] = adddaily[i,j]+1
    }
  }
  
  
  anomaly.df = originaldaily + adddaily
    
  
  for (i in 1:nrow(anomaly.df)){
    for (j in 1:ncol(anomaly.df)){
      ifelse(anomaly.df[i,j]< 0, anomaly.df[i,j] = 0, anomaly.df[i,j] = anomaly.df[i,j])
    }
  }
  dailyamy.df = anomaly.df
  
  return(dailyanomaly.df)
  write.csv(dailyanomaly,output) 
}
  



 
anomaly.df = data.frame(c(3,4,6,0),c(-2,2,3,-5))

for (i in 1:nrow(anomaly.df)){
  for (j in 1:ncol(anomaly.df)){
    ifelse(anomaly.df[i,j]< 0, anomaly.df[i,j] = 0, anomaly.df[i,j] = anomaly.df[i,j])
  }
}
anomaly.df 





anomaly.df[logic] = 7
head(anomaly.df)

nd <- apply(anomaly.df[ ,2:length(anomaly.df)], 1, which.min) # build column index by row
res <- d[,5:8][cbind(1:nrow(d), ind)]


  
anomaly.df = daily1.df
head(anomaly.df)

for (i in 1:nrow(anomaly.df)){
  for (j in 2:ncol(anomaly.df)){
    anomaly.df[i,j] = anomaly.df[i,j]+count[i,j]
  }
}
head(anomaly.df)

?runif()

test = round(runif(20, 1, 5))
table(test)

