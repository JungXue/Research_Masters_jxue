
##########################################################################
#                                                                        #
# Simulation                                                             #
#                                                                        #
##########################################################################

# Function 1
# rand.day.time originally by Dirk Eddelbuettel 2012
# https://stackoverflow.com/questions/14720983/efficiently-generate-a-random-sample-of-times-and-dates-between-two-dates
rand.day.time  <- function(N, st="2006/01/01 07:08:11", et="2018/12/31 22:56:43") {
       st <- as.POSIXct(as.Date(st))
       et <- as.POSIXct(as.Date(et))
       dt <- as.numeric(difftime(et,st,unit="sec"))
       ev <- sort(runif(N, 0, dt))
       rt <- st + ev
}

day.start <- "2006/01/01 00:00:00"
day.end <- "2018/12/31 00:00:00"

print(rand.day.time(5,day.start,day.end)) 

system.time(rand.day.time (1000))
system.time(rand.day.time (1000000))

test0 = rand.day.time (1000000)

t.char = as.character(test0)
t.day = as.character(as.Date(t.char,units = "days"))
dailycount  = tabulate(as.factor(t.day))

plot(dailycount,type="l",col="blue")
abline(h=mean(dailycount),col="red",lwd=2)
points(dailycount[1], pch=19,col="red")
points(length(dailycount),dailycount[length(dailycount)], pch=19,col="red")



############################################################################################################
############################################################################################################

## Function to simulate data
# 1 million simulation recommended

#first and last day counts only half? 

simdata <-function(sim.number = 1000000,                  # number of simulations
                   time.start = "2006/01/01 00:00:01",    # period
                   time.end = "2018/12/31 23:59:59",
                   cat1n = 2, cat2n = 2,                  # brunches
                   lvl1prop = c(0,0),lv2prop = list(0,0)){# proportion in each brunches
  
# identifiers/key variables
  index = 1:sim.number 
  EventID = sprintf("%08d", sample(1:paste(rep(9,8), collapse=""), sim.number,replace=F))
  
# select random times
  rand.day.time  <- function(N, st=time.start-1, et = time.end+1) {
    st <- as.POSIXct(as.Date(st))
    et <- as.POSIXct(as.Date(et))
    dt <- as.numeric(difftime(et,st,unit="sec"))
    ev <- sort(runif(N, 0, dt))
    rt <- st + ev
  }
time = rand.day.time(sim.number, time.start, time.end)
time.char = as.character(time)
time.day = as.character(as.Date(time.char,units = "days"))

#simulate level 1 and 2 brunch
#maybe add a argument for different proportions? maybe in sample function?
Cat1.val = c(LETTERS[1:cat1n])
Cat2.val = c(LETTERS[1:cat2n])
Cat1 = sample(Cat1.val, sim.number, replace = TRUE)
Cat2 = sample(Cat2.val, sim.number, replace = TRUE)
Category = cbind(Cat1,Cat2)

#randomly select letters digit by digit? double outer paste?
catnames = c( LETTERS[1:cat1n],t(outer( LETTERS[1:cat1n], LETTERS[1:cat2n],FUN=paste ,sep="")))
mat = matrix(NA, nrow = sim.number, ncol = 2 + cat1n^cat2n)
colnames(mat) = catnames
Category2=data.frame(mat)

#create dataframe
sim.data.df = data.frame(cbind(index, EventID, time.char, time.day, Category,Category2))
sim.data.df$index     = as.numeric  (sim.data.df$index)
sim.data.df$EventID   = as.character(sim.data.df$EventID)
sim.data.df$time.char = as.character(sim.data.df$time.char)
sim.data.df$time.day  = as.Date     (sim.data.df$time.day)

#add dummy variables
#maybe write an forloop so i can manipulate cats easily, get rid of hardcoding
#do this before dataframe is created?
#if else not effecient, how to do it in vector?
#outer? mapply?
sim.data.df$A  = ifelse(sim.data.df$Cat1 =="A", 1, 0)
sim.data.df$B  = ifelse(sim.data.df$Cat1 =="B", 1, 0)
sim.data.df$AA = ifelse(sim.data.df$Cat1 =="A" & sim.data.df$Cat2 =="A", 1, 0)
sim.data.df$AB = ifelse(sim.data.df$Cat1 =="A" & sim.data.df$Cat2 =="B", 1, 0)          
sim.data.df$BA = ifelse(sim.data.df$Cat1 =="B" & sim.data.df$Cat2 =="A", 1, 0)
sim.data.df$BB = ifelse(sim.data.df$Cat1 =="B" & sim.data.df$Cat2 =="B", 1, 0)

return(sim.data.df)
}


test1 = simdata (1000)
head(test1)
str(test1)

test1 = simdata (1000000)
head(test1)

# system.time(simdata (1000))
# system.time(simdata (1000000))


#########################################################################################################

## check simulation
##use ggplot 2 later if we want a good presentable plot

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

# For simulated data
# how to add variables such as demographis
# how to add trends such as season etc?




