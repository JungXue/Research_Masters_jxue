source("00-permeables.R")
source("01-rand.day.time.R")
source("02-rand.day.R")
source("03-simdata.R")
source("04-cumdata.R")
source("05-ggcumplot.R")
source("06-tabulatedata.R")
source("07-ggdailyplot.R")
# source("91-createdata.R")
set.seed(123456)


daily.anomaly.test<- function(){
  
  load("rdata/test/raw1.RData")
  load("rdata/test/daily1.RData")
  
  data = daily1.df
  usepercent = T
  TotalNpercent = 10 
  Anomalytype = "Point"
  day = c("2015-11-15") 
  nperiod=c(1)
  Leafpercent = rbind(c(1,
                        1,0,
                        1,0,0,0))
  output = 'rdata/anomaly/daily1.S10.RData'
  
}



adddaily.anomaly<- function(data = daily1.df,                             # import data
                            usepercent    = T,                            # are we using number or percentage? 
                            TotalN        = 100,                          # count interms of absoulte number 
                            TotalNpercent = 1000,                         # count interms of % of mean total
                    
                            Anomalytype = "Period",                       # Add Point or Period Anomaly
                            nperiod = c(5,3),                             # length of days of the period
                            day = rand.day(N=2,period=c(5,3)),            # random periods 
                            Leafpercent = rbind(c(1,1,0,0.25,0.75,0.00,0.00),   # distribution of anomaly at leafs
                                                c(1,1,0,1,   0,   0,   0   )),   #
                            Distribution  = "Normal",                     # What distribution does period anomaly have
                            output = "rdata/function/anomalytest.RData")                  # Name of output file
{
  
  # use percentage of counts
  
#  https://stattrek.com/statistics/dictionary.aspx?definition=z_score
#  switch(method,
#         "count"   = TotalN,
#         "percent" = mean(data$total)*TotalNpercent/100,
#         "z-value" = 3*sd(data$total))
  
  TotalN2 = ifelse(usepercent==T,mean(data$total)*TotalNpercent/100,TotalN)
  TotalN2 = round(TotalN2, digit=0)
  TotalN2
  
  # Leaf of data
  names = names(data [,2:ncol(data )])
  cat1n = table(nchar(names))[1]
  leafn = table(nchar(names))[2]
  leaf = names[1:length(names)]
  leaf
  
  # proportion of anomaly at each leaf for each day
  Leafpercent
  Leafpercent2 = data.frame(rbind(Leafpercent[rep(1:length(nperiod), nperiod),]))
  Leafpercent2$day = day
  
  colnames(Leafpercent2) = c(leaf,"day")
  Leafpercent2
  Leafpercent2 = Leafpercent2[,c("day",leaf)]
  Leafpercent2
  
  # Assign probabilities
  point.prob<- function(){
    probs2 = list(c(1))
    return(probs2)
  }
  
  period.prob<- function(Distribution2 = Distribution){
    probs2=list()
    
    for (i in 1:length(nperiod)){
      probs2[[i]] =  switch(Distribution2,
                            "Normal"  = dnorm(1:nperiod[i],mean = (1+nperiod[i])/2, sd = sd(1:nperiod[i])),#???right
                            "Uniform" = dunif(1:nperiod[i],1,nperiod[i]+1))
    }
    return(probs2)
  }
  
  probs2 = switch(Anomalytype, "Period" = period.prob(Distribution),
                               "Point"  = point.prob())
  probs2
 
  
  # Assign counts from the distributions
  point.tab<- function(){
    addcountstab = list(c(TotalN2))
    return(addcountstab)
  }
 
  period.tab<- function(TotalN2b = TotalN2){
    
  addcountstab = list()
  for (i in 1:length(nperiod)){
  k = nperiod[i]
  addcounts = c(sample(1:k,TotalN2b,replace = TRUE,prob=probs2[[i]]))
  addcountstab[[i]] = table(addcounts)
  # plot(addcountstab)[[i]]
  }
  addcountstab
  
  fullcounts = c(unlist(addcountstab, recursive = TRUE, use.names = F))
  return(fullcounts)
  }

  fullcounts = switch(Anomalytype, "Period" = period.tab(TotalN2),
                                    "Point" = point.tab())
  fullcounts 
  
  
  fullcounts.matrix = matrix(rep(fullcounts, length(leaf)),ncol= length(leaf))
  fullcounts.matrix
  
  finalcounts = round(Leafpercent2[2:length(Leafpercent2)]*fullcounts.matrix,digits = 0)
  finalcounts = cbind(Leafpercent2[1],finalcounts)
  finalcounts$day = factor(as.character(finalcounts$day),levels = levels(data$day))
  finalcounts
  #str(finalcounts)
  
  # original data frame
  originaldaily = data
  head(originaldaily)
  #str(originaldaily)
  
  
  # empty data frame
  adddaily = data
  for (i in 1:nrow(data)){
    for (j in 2:ncol(data)){
      adddaily[i,j] = 0
    }
  }
  head(adddaily)
  
  # Figure out rows that need to change
  testa = rowSums(outer(adddaily$day,finalcounts$day,"=="))
  numdate = which(testa==1)
  numdate
  logicdate = as.logical(testa)
  logicdate[numdate]


 # sum anomaly to daily data
  for (i in 1:nrow(finalcounts)){
    k = numdate[i]
    adddaily[k,2:ncol(adddaily)] = finalcounts[i,2:ncol(finalcounts)]
  }
  adddaily [numdate,]
  colSums(adddaily[,2:ncol(adddaily)])
  
  
  finaldaily = originaldaily
  finaldaily[,2:ncol(finaldaily)] = originaldaily[2:ncol(originaldaily)]+adddaily[2:ncol(adddaily)]
  adddaily [numdate,]
  originaldaily[numdate,]
  finaldaily[numdate,]

  dailyamy.df =  finaldaily
  dailyamy.df[numdate,]
  #str(originaldaily)
  #subset(originaldaily , day = as.factor(finalcounts$day))

### change all negative value to 0 
  
#####################  
#  for(j in 1:ncol(dailyamy.df)){
#  dailyamy.df[dailyamy.df[, j]< 0, j] <- 0
#  }
  
#  for (i in 1:nrow(dailyamy.df)){
#    for (j in 1:ncol(dailyamy.df)){
#      ifelse(dailyamy.df[i,j]< 0, dailyamy.df[i,j] <- 0, dailyamy.df[i,j] <- dailyamy.df[i,j])
#    }
# }
  
  #testing 
  test2 <- function(){
    
    test = dailyamy.df[numdate,]
    test[5,4] = -10
    test
    
    for(j in 2:ncol(test)){
      test[,j] <- ifelse(test[,j]< 0, 0, test[,j])
      test
    }
    
  }
  
###################
  
 for(j in 2:ncol(dailyamy.df)){
    dailyamy.df[,j] <- ifelse(dailyamy.df[,j]< 0, 0, dailyamy.df[,j])

 }
  
  dailyamy.df[numdate,]
  
### export data frame

  save(dailyamy.df,file = output)
  return(dailyamy.df)
  
}










daily.anomaly.test2<- function(){
  
daily1.df = read.csv("daily1.csv")
  
test = adddaily.anomaly(data = daily1.df, usepercent = T,TotalNpercent = 1000, 
                Anomalytype = "Period",nperiod = c(5,3),
                day = rand.day(N=2,period=c(5,3)),
                Leafpercent = rbind(c(1,1,0,0.25,0.75,0.00,0.00),
                                    c(1,1,0,1,   0,   0,   0   )),
                Distribution = "Normal",
                output = 'dailyanomaly.csv')

test2 = adddaily.anomaly(data = daily1.df, usepercent = T,TotalNpercent = 1000, 
                        Anomalytype = "Point",
                        day = c("2015-10-21"),
                        nperiod=c(1),
                        Leafpercent = rbind(c(1,1,0,0.25,0.75,0.00,0.00)),
                        Distribution = "Normal",
                        output = 'dailyanomaly2.csv')
                        
numdate =  c(3578, 3579, 3580, 3581, 3582, 3788, 3789, 3790)
test[numdate,]
daily1.df [numdate,]

head(test)
head(daily1.df)

test[numdate,]
daily1.df [numdate,]

head(test2)
head(daily1.df)

test[daily1.df$day =="2015-10-21",]
daily1.df [daily1.df$day =="2015-10-21",]

}