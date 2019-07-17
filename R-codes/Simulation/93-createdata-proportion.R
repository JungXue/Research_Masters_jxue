setwd("H:/Desktop/7.1 No time to waste/Stats 798A Research Master/R-codes/Simulation")

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

# Data matrix with different proportions

matrix1 = cbind(c(200,  0),c(100,100))
matrix2 = cbind(c(100,100),c(100,100))
matrix3 = cbind(c(100,100,100),c(150,150,0))
matrix4 = cbind(c(100,100,100,100),c(200,200,0,0))
matrix5 = cbind(c(100,100,100,100,100),c(250,250,0,0,0))
matrix6 = cbind(c(100,100,100,100,100,100),c(300,300,0,0,0,0))
matrix7 = cbind(c(100,100,100,100,100,100,100),c(350,350,0,0,0,0,0))
matrix8 = cbind(c(100,100,100,100,100,100,100,100),c(400,400,0,0,0,0,0,0))
matrix9 = cbind(c(100,100,100,100,100,100,100,100,100),c(450,450,0,0,0,0,0,0,0))
matrix10 = cbind(c(100,100,100,100,100,100,100,100,100,100),c(500,500,0,0,0,0,0,0,0,0))
matrix11 = cbind(c(100,100,100,100,100,100,100,100,100,100,100),c(500,500,0,0,0,0,0,0,0,0,0))
matrix12 = cbind(c(100,100,100,100,100,100,100,100,100,100,100,100),c(500,500,0,0,0,0,0,0,0,0,0,0))

### Create raw data

t.start = "2006/01/01 00:00:01"   
t.end = "2018/12/31 23:59:59"

rawp1.df = simdata (100000,cat2.val = matrix1, time.start = t.start, time.end = t.end)
rawp2.df = simdata (100000,cat2.val = matrix2, time.start = t.start, time.end = t.end)
rawp3.df = simdata (100000,cat2.val = matrix3, time.start = t.start, time.end = t.end)
rawp4.df = simdata (100000,cat2.val = matrix4, time.start = t.start, time.end = t.end)
rawp5.df = simdata (100000,cat2.val = matrix5, time.start = t.start, time.end = t.end)
rawp6.df = simdata (100000,cat2.val = matrix6, time.start = t.start, time.end = t.end)
rawp7.df = simdata (100000,cat2.val = matrix7, time.start = t.start, time.end = t.end)
rawp8.df = simdata (100000,cat2.val = matrix8, time.start = t.start, time.end = t.end)
rawp9.df = simdata (100000,cat2.val = matrix9, time.start = t.start, time.end = t.end)
rawp10.df = simdata (100000,cat2.val = matrix10, time.start = t.start, time.end = t.end)
rawp11.df = simdata (100000,cat2.val = matrix11, time.start = t.start, time.end = t.end)
rawp12.df = simdata (100000,cat2.val = matrix12, time.start = t.start, time.end = t.end)

head(rawp1.df)
head(rawp2.df)
head(rawp3.df)
head(rawp4.df)
head(rawp5.df)
head(rawp6.df)
head(rawp7.df)
head(rawp8.df)
head(rawp9.df)
head(rawp10.df)
head(rawp11.df)
head(rawp12.df)

### Create daily data

dailyp1.df = tabulatedata(rawp1.df)
dailyp2.df = tabulatedata(rawp2.df) 
dailyp3.df = tabulatedata(rawp3.df)
dailyp4.df = tabulatedata(rawp4.df)
dailyp5.df = tabulatedata(rawp5.df)
dailyp6.df = tabulatedata(rawp6.df) 
dailyp7.df = tabulatedata(rawp7.df)
dailyp8.df = tabulatedata(rawp8.df)
dailyp9.df = tabulatedata(rawp9.df)
dailyp10.df = tabulatedata(rawp10.df)
dailyp11.df = tabulatedata(rawp11.df)
dailyp12.df = tabulatedata(rawp12.df)

head(dailyp1.df)
head(dailyp2.df)
head(dailyp3.df)
head(dailyp4.df)
head(dailyp5.df)
head(dailyp6.df)
head(dailyp7.df)
head(dailyp8.df)
head(dailyp9.df)
head(dailyp10.df)
head(dailyp11.df)
head(dailyp12.df)

##################################################################

### Add anomaly to proportions

dailyp1a1.df = adddaily.anomaly(data = dailyp1.df, usepercent = T,TotalNpercent = 10, 
                                 Anomalytype = "Point",day = c("2015-11-15"), nperiod=c(1),
                                 Leafpercent = rbind(c(1,
                                                       1,0,
                                                       1,0,0)),
                                 output = 'rdata/proportion/daily1.S10.RData') 
dailyp2a1.df = adddaily.anomaly(data = dailyp2.df, usepercent = T,TotalNpercent = 10, 
                                Anomalytype = "Point",day = c("2015-11-15"), nperiod=c(1),
                                Leafpercent = rbind(c(1,
                                                      1,0,
                                                      1,0,0,0)),
                                output = 'rdata/proportion/daily1.S10.RData') 
dailyp3a1.df = adddaily.anomaly(data = dailyp3.df, usepercent = T,TotalNpercent = 10, 
                                Anomalytype = "Point",day = c("2015-11-15"), nperiod=c(1),
                                Leafpercent = rbind(c(1,
                                                      1,0,
                                                      1,0,0,0,0)),
                                output = 'rdata/proportion/daily1.S10.RData') 
dailyp4a1.df = adddaily.anomaly(data = dailyp4.df, usepercent = T,TotalNpercent = 10, 
                                Anomalytype = "Point",day = c("2015-11-15"), nperiod=c(1),
                                Leafpercent = rbind(c(1,
                                                      1,0,
                                                      1,0,0,0,0,0)),
                                output = 'rdata/proportion/daily1.S10.RData') 
dailyp5a1.df = adddaily.anomaly(data = dailyp5.df, usepercent = T,TotalNpercent = 10, 
                                Anomalytype = "Point",day = c("2015-11-15"), nperiod=c(1),
                                Leafpercent = rbind(c(1,
                                                      1,0,
                                                      1,0,0,0,0,0,0)),
                                output = 'rdata/proportion/dailyp5a1.RData') 
dailyp6a1.df = adddaily.anomaly(data = dailyp6.df, usepercent = T,TotalNpercent = 10, 
                                Anomalytype = "Point",day = c("2015-11-15"), nperiod=c(1),
                                Leafpercent = rbind(c(1,
                                                      1,0,
                                                      1,0,0,0,0,0,0,0)),
                                output = 'rdata/proportion/dailyp6a1..RData') 
dailyp7a1.df = adddaily.anomaly(data = dailyp7.df, usepercent = T,TotalNpercent = 10, 
                                Anomalytype = "Point",day = c("2015-11-15"), nperiod=c(1),
                                Leafpercent = rbind(c(1,
                                                      1,0,
                                                      1,0,0,0,0,0,0,0,0)),
                                output = 'rdata/proportion/dailyp7a1.RData') 
dailyp8a1.df = adddaily.anomaly(data = dailyp8.df, usepercent = T,TotalNpercent = 10, 
                                Anomalytype = "Point",day = c("2015-11-15"), nperiod=c(1),
                                Leafpercent = rbind(c(1,
                                                      1,0,
                                                      1,0,0,0,0,0,0,0,0,0)),
                                output = 'rdata/proportion/dailyp8a1.RData') 
dailyp9a1.df = adddaily.anomaly(data = dailyp9.df, usepercent = T,TotalNpercent = 10, 
                                Anomalytype = "Point",day = c("2015-11-15"), nperiod=c(1),
                                Leafpercent = rbind(c(1,
                                                      1,0,
                                                      1,0,0,0,0,0,0,0,0,0,0)),
                                output = 'rdata/proportion/dailyp9a1.RData') 
dailyp10a1.df = adddaily.anomaly(data = dailyp10.df, usepercent = T,TotalNpercent = 10, 
                                Anomalytype = "Point",day = c("2015-11-15"), nperiod=c(1),
                                Leafpercent = rbind(c(1,
                                                      1,0,
                                                      1,0,0,0,0,0,0,0,0,0,0,0)),
                                output = 'rdata/proportion/dailyp10a1.RData') 
dailyp11a1.df = adddaily.anomaly(data = dailyp11.df, usepercent = T,TotalNpercent = 10, 
                                Anomalytype = "Point",day = c("2015-11-15"), nperiod=c(1),
                                Leafpercent = rbind(c(1,
                                                      1,0,
                                                      1,0,0,0,0,0,0,0,0,0,0,0,0)),
                                output = 'rdata/proportion/dailyp11a1.RData') 
dailyp12a1.df = adddaily.anomaly(data = dailyp12.df, usepercent = T,TotalNpercent = 10, 
                                Anomalytype = "Point",day = c("2015-11-15"), nperiod=c(1),
                                Leafpercent = rbind(c(1,
                                                      1,0,
                                                      1,0,0,0,0,0,0,0,0,0,0,0,0,0)),
                                output = 'rdata/proportion/dailyp12a1.RData') 



### Export data ###############################################################

save(rawp1.df,file = "rdata/proportion/rawp1.RData")
save(rawp2.df,file = "rdata/proportion/rawp2.RData")
save(rawp3.df,file = "rdata/proportion/rawp3.RData")
save(rawp4.df,file = "rdata/proportion/rawp4.RData")
save(rawp5.df,file = "rdata/proportion/rawp5.RData")
save(rawp6.df,file = "rdata/proportion/rawp6.RData")
save(rawp7.df,file = "rdata/proportion/rawp7.RData")
save(rawp8.df,file = "rdata/proportion/rawp8.RData")
save(rawp9.df,file = "rdata/proportion/rawp9.RData")
save(rawp10.df,file = "rdata/proportion/rawp10.RData")
save(rawp11.df,file = "rdata/proportion/rawp11.RData")
save(rawp12.df,file = "rdata/proportion/rawp12.RData")

save(cump1.df,file = "rdata/proportion/cump1.RData")
save(cump2.df,file = "rdata/proportion/cump2.RData")
save(cump3.df,file = "rdata/proportion/cump3.RData")
save(cump4.df,file = "rdata/proportion/cump4.RData")
save(cump5.df,file = "rdata/proportion/cump5.RData")
save(cump6.df,file = "rdata/proportion/cump6.RData")
save(cump7.df,file = "rdata/proportion/cump7.RData")
save(cump8.df,file = "rdata/proportion/cump8.RData")
save(cump9.df,file = "rdata/proportion/cump9.RData")
save(cump10.df,file = "rdata/proportion/cump10.RData")
save(cump11.df,file = "rdata/proportion/cump12.RData")
save(cump12.df,file = "rdata/proportion/cump13.RData")

save(dailyp1.df,file = "rdata/proportion/dailyp1.RData")
save(dailyp2.df,file = "rdata/proportion/dailyp2.RData")
save(dailyp3.df,file = "rdata/proportion/dailyp3.RData")
save(dailyp4.df,file = "rdata/proportion/dailyp4.RData")
save(dailyp5.df,file = "rdata/proportion/dailyp5.RData")
save(dailyp6.df,file = "rdata/proportion/dailyp6.RData")
save(dailyp7.df,file = "rdata/proportion/dailyp7.RData")
save(dailyp8.df,file = "rdata/proportion/dailyp8.RData")
save(dailyp9.df,file = "rdata/proportion/dailyp9.RData")
save(dailyp10.df,file = "rdata/proportion/dailyp10.RData")
save(dailyp11.df,file = "rdata/proportion/dailyp11.RData")
save(dailyp12.df,file = "rdata/proportion/dailyp12.RData")

save(dailyp1a1.df,file = "rdata/proportion/dailyp1a1.RData")
save(dailyp2a1.df,file = "rdata/proportion/dailyp2a1.RData")
save(dailyp3a1.df,file = "rdata/proportion/dailyp3a1.RData")
save(dailyp4a1.df,file = "rdata/proportion/dailyp4a1.RData")
save(dailyp5a1.df,file = "rdata/proportion/dailyp5a1.RData")
save(dailyp6a1.df,file = "rdata/proportion/dailyp6a1.RData")
save(dailyp7a1.df,file = "rdata/proportion/dailyp7a1.RData")
save(dailyp8a1.df,file = "rdata/proportion/dailyp8a1.RData")
save(dailyp9a1.df,file = "rdata/proportion/dailyp9a1.RData")
save(dailyp10a1.df,file = "rdata/proportion/dailyp10a1.RData")
save(dailyp11a1.df,file = "rdata/proportion/dailyp11a1.RData")
save(dailyp12a1.df,file = "rdata/proportion/dailyp12a1.RData")

##############################################################################

### Output ggplots

ggdailyplot(dailyp1a1.df [3000:4000,], nameB1= "plot/proportion/ggdailyp1a1.B1.jpg", nameLeaf= "plot/proportion/ggdailyP1a1.leaf.jpg")
ggdailyplot(dailyp2a1.df [3000:4000,], nameB1= "plot/proportion/ggdailyP2a1.B1.jpg", nameLeaf= "plot/proportion/ggdailyP2a1.leaf.jpg")
ggdailyplot(dailyp3a1.df [3000:4000,], nameB1= "plot/proportion/ggdailyP3a1.B1.jpg", nameLeaf= "plot/proportion/ggdailyP3a1.leaf.jpg")
ggdailyplot(dailyp4a1.df [3000:4000,], nameB1= "plot/proportion/ggdailyP4a1.B1.jpg", nameLeaf= "plot/proportion/ggdailyP4a1.leaf.jpg")
ggdailyplot(dailyp5a1.df [3000:4000,], nameB1= "plot/proportion/ggdailyP5a1.B1.jpg", nameLeaf= "plot/proportion/ggdailyP5a1.leaf.jpg")
ggdailyplot(dailyp6a1.df [3000:4000,], nameB1= "plot/proportion/ggdailyP6a1.B1.jpg", nameLeaf= "plot/proportion/ggdailyP6a1.leaf.jpg")
ggdailyplot(dailyp7a1.df [3000:4000,], nameB1= "plot/proportion/ggdailyP7a1.B1.jpg", nameLeaf= "plot/proportion/ggdailyP7a1.leaf.jpg")
ggdailyplot(dailyp8a1.df [3000:4000,], nameB1= "plot/proportion/ggdailyP8a1.B1.jpg", nameLeaf= "plot/proportion/ggdailyP8a1.leaf.jpg")
ggdailyplot(dailyp9a1.df [3000:4000,], nameB1= "plot/proportion/ggdailyP9a1.B1.jpg", nameLeaf= "plot/proportion/ggdailyP9a1.leaf.jpg")
ggdailyplot(dailyp10a1.df[3000:4000,],nameB1= "plot/proportion/ggdailyP10a1.B1.jpg",nameLeaf= "plot/proportion/ggdailyP10a1.leaf.jpg")
ggdailyplot(dailyp11a1.df[3000:4000,],nameB1= "plot/proportion/ggdailyP11a1.B1.jpg",nameLeaf= "plot/proportion/ggdailyP11a1.leaf.jpg")
ggdailyplot(dailyp12a1.df[3000:4000,],nameB1= "plot/proportion/ggdailyP12a1.B1.jpg",nameLeaf= "plot/proportion/ggdailyP12a1.leaf.jpg")

##############################################################################
