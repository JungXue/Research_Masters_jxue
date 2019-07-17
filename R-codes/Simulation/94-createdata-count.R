source("00-permeables.R")
source("01-rand.day.time.R")
source("02-rand.day.R")
source("03-simdata.R")
source("04-cumdata.R")
source("05-ggcumplot.R")
source("06-tabulatedata.R")
source("07-ggdailyplot.R")
# source("91-createdata.R")
source("08-dailyanomaly.R")

set.seed(1234567)

##############################################################################

# Data matrix with different count

matrix12 = cbind(c(100,100,100,100,100, 100,100,100,100,100, 100,100),c(600,600,0,0,0, 0,0,0,0,0, 0,0))
matrix12
t.start = "2006/01/01 00:00:01"   
t.end = "2018/12/31 23:59:59"

rawp12.df = simdata (100000,cat2.val = matrix12, time.start = t.start, time.end = t.end)
head(rawp12.df)

dailyp12.df = tabulatedata(rawp12.df)
head(dailyp12.df)

save(rawp12.df,file = "rdata/count/rawp12.RData")
save(dailyp12.df,file = "rdata/count/dailyp12.RData")

#------------add anomaly-------------------

dailyp12a1.df = adddaily.anomaly(data = dailyp12.df, usepercent = T,TotalNpercent = 10, 
                                 Anomalytype = "Point", day = c("2015-11-15"), nperiod=c(1),
                                 Leafpercent = rbind(c(1,
                                                       1,0,
                                                       1,0,0,0,0,0, 0,0,0,0,0,0, 0,0)),
                                 output = 'rdata/count/dailyp12.a1.RData') 
dailyp12a2.df = adddaily.anomaly(data = dailyp12.df, usepercent = T,TotalNpercent = 10, 
                                  Anomalytype = "Point", day = c("2015-11-15"), nperiod=c(1),
                                  Leafpercent = rbind(c(2,
                                                        2,0,
                                                        1,1,0,0,0,0, 0,0,0,0,0,0, 0,0)),
                                  output = 'rdata/count/dailyp12.a1.RData') 
dailyp12a3.df = adddaily.anomaly(data = dailyp12.df, usepercent = T,TotalNpercent = 10, 
                                  Anomalytype = "Point", day = c("2015-11-15"), nperiod=c(1),
                                  Leafpercent = rbind(c(3,
                                                        3,0,
                                                        1,1,1,0,0,0, 0,0,0,0,0,0, 0,0)),
                                  output = 'rdata/count/dailyp12.a1.RData') 
dailyp12a4.df = adddaily.anomaly(data = dailyp12.df, usepercent = T,TotalNpercent = 10, 
                                  Anomalytype = "Point", day = c("2015-11-15"), nperiod=c(1),
                                  Leafpercent = rbind(c(4,
                                                        4,0,
                                                        1,1,1,1,0,0, 0,0,0,0,0,0, 0,0)),
                                  output = 'rdata/count/dailyp12.a1.RData') 
dailyp12a5.df = adddaily.anomaly(data = dailyp12.df, usepercent = T,TotalNpercent = 10, 
                                  Anomalytype = "Point", day = c("2015-11-15"), nperiod=c(1),
                                  Leafpercent = rbind(c(5,
                                                        5,0,
                                                        1,1,1,1,1,0, 0,0,0,0,0,0, 0,0)),
                                  output = 'rdata/count/dailyp12.a1.RData') 
dailyp12a6.df = adddaily.anomaly(data = dailyp12.df, usepercent = T,TotalNpercent = 10, 
                                  Anomalytype = "Point", day = c("2015-11-15"), nperiod=c(1),
                                  Leafpercent = rbind(c(6,
                                                        6,0,
                                                        1,1,1,1,1,1, 0,0,0,0,0,0, 0,0)),
                                  output = 'rdata/count/dailyp12.a1.RData') 
dailyp12a7.df = adddaily.anomaly(data = dailyp12.df, usepercent = T,TotalNpercent = 10, 
                                  Anomalytype = "Point", day = c("2015-11-15"), nperiod=c(1),
                                  Leafpercent = rbind(c(7,
                                                        7,0,
                                                        1,1,1,1,1,1, 1,0,0,0,0,0, 0,0)),
                                  output = 'rdata/count/dailyp12.a1.RData') 
dailyp12a8.df = adddaily.anomaly(data = dailyp12.df, usepercent = T,TotalNpercent = 10, 
                                  Anomalytype = "Point", day = c("2015-11-15"), nperiod=c(1),
                                  Leafpercent = rbind(c(8,
                                                        8,0,
                                                        1,1,1,1,1,1, 1,1,0,0,0,0, 0,0)),
                                  output = 'rdata/count/dailyp12.a1.RData') 
dailyp12a9.df = adddaily.anomaly(data = dailyp12.df, usepercent = T,TotalNpercent = 10, 
                                  Anomalytype = "Point", day = c("2015-11-15"), nperiod=c(1),
                                  Leafpercent = rbind(c(9,
                                                        9,0,
                                                        1,1,1,1,1,1, 1,1,1,0,0,0, 0,0)),
                                  output = 'rdata/count/dailyp12.a1.RData') 
dailyp12a10.df = adddaily.anomaly(data = dailyp12.df, usepercent = T,TotalNpercent = 10, 
                                  Anomalytype = "Point", day = c("2015-11-15"), nperiod=c(1),
                                  Leafpercent = rbind(c(10,
                                                        10,0,
                                                        1,1,1,1,1,1, 1,1,1,1,0,0, 0,0)),
                                  output = 'rdata/count/dailyp12.a1.RData') 
dailyp12a11.df = adddaily.anomaly(data = dailyp12.df, usepercent = T,TotalNpercent = 10, 
                                  Anomalytype = "Point", day = c("2015-11-15"), nperiod=c(1),
                                  Leafpercent = rbind(c(11,
                                                        11,0,
                                                        1,1,1,1,1,1, 1,1,1,1,1,0, 0,0)),
                                  output = 'rdata/count/dailyp12.a1.RData') 
dailyp12a12.df = adddaily.anomaly(data = dailyp12.df, usepercent = T,TotalNpercent = 10, 
                                  Anomalytype = "Point", day = c("2015-11-15"), nperiod=c(1),
                                  Leafpercent = rbind(c(12,
                                                        12,0,
                                                        1,1,1,1,1,1, 1,1,1,1,1,1, 0,0)),
                                  output = 'rdata/count/dailyp12.a1.RData') 
### Export data ###############################################################

save(dailyp12a1.df,file = "rdata/count/dailyp12a1.RData")
save(dailyp12a2.df,file = "rdata/count/dailyp12a2.RData")
save(dailyp12a3.df,file = "rdata/count/dailyp12a3.RData")
save(dailyp12a4.df,file = "rdata/count/dailyp12a4.RData")
save(dailyp12a5.df,file = "rdata/count/dailyp12a5.RData")
save(dailyp12a6.df,file = "rdata/count/dailyp12a6.RData")
save(dailyp12a7.df,file = "rdata/count/dailyp12a7.RData")
save(dailyp12a8.df,file = "rdata/count/dailyp12a8.RData")
save(dailyp12a9.df,file = "rdata/count/dailyp12a9.RData")
save(dailyp12a10.df,file = "rdata/count/dailyp12a10.RData")
save(dailyp12a11.df,file = "rdata/count/dailyp12a11.RData")
save(dailyp12a12.df,file = "rdata/count/dailyp12a12.RData")

##############################################################################

### Output ggplots

ggdailyplot(dailyp12a1.df, nameB1= "plot/count/ggdailyp12a1.B1.jpg", nameLeaf= "plot/count/ggdailyP12a1.leaf.jpg")
ggdailyplot(dailyp12a2.df, nameB1= "plot/count/ggdailyp12a2.B1.jpg", nameLeaf= "plot/count/ggdailyP12a2.leaf.jpg")
ggdailyplot(dailyp12a3.df, nameB1= "plot/count/ggdailyp12a3.B1.jpg", nameLeaf= "plot/count/ggdailyP12a3.leaf.jpg")
ggdailyplot(dailyp12a4.df, nameB1= "plot/count/ggdailyp12a4.B1.jpg", nameLeaf= "plot/count/ggdailyP12a4.leaf.jpg")
ggdailyplot(dailyp12a5.df, nameB1= "plot/count/ggdailyp12a5.B1.jpg", nameLeaf= "plot/count/ggdailyP12a5.leaf.jpg")
ggdailyplot(dailyp12a6.df, nameB1= "plot/count/ggdailyp12a6.B1.jpg", nameLeaf= "plot/count/ggdailyP12a6.leaf.jpg")
ggdailyplot(dailyp12a7.df, nameB1= "plot/count/ggdailyp12a7.B1.jpg", nameLeaf= "plot/count/ggdailyP12a7.leaf.jpg")
ggdailyplot(dailyp12a8.df, nameB1= "plot/count/ggdailyp12a8.B1.jpg", nameLeaf= "plot/count/ggdailyP12a8.leaf.jpg")
ggdailyplot(dailyp12a9.df, nameB1= "plot/count/ggdailyp12a9.B1.jpg", nameLeaf= "plot/count/ggdailyP12a9.leaf.jpg")
ggdailyplot(dailyp12a10.df,nameB1= "plot/count/ggdailyp12a10.B1.jpg",nameLeaf= "plot/count/ggdailyP12a10.leaf.jpg")
ggdailyplot(dailyp12a11.df,nameB1= "plot/count/ggdailyp12a11.B1.jpg",nameLeaf= "plot/count/ggdailyP12a11.leaf.jpg")
ggdailyplot(dailyp12a12.df,nameB1= "plot/count/ggdailyp12a12.B1.jpg",nameLeaf= "plot/count/ggdailyP12a12.leaf.jpg")
