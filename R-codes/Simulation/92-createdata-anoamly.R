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

##################################################################

### simulated data 

load("rdata/test/raw1.RData")
load("rdata/test/daily1.RData")

save(raw1.df,file = "rdata/anomaly/raw1.RData")
save(daily1.df,file = "rdata/anomaly/daily1.RData")

t.start = "2006/01/01 00:00:01"   
t.end = "2018/12/31 23:59:59"

##################################################################

### Add anomaly to proportions

    ### diff scale         
    ### s = scale (1 = 1%)

daily1.S10.df = adddaily.anomaly(data = daily1.df, usepercent = T,TotalNpercent = 10, 
                                 Anomalytype = "Point", day = c("2015-11-15"), nperiod=c(1),
                                 Leafpercent = rbind(c(1,
                                                       1,0,
                                                       1,0,0,0)),
                                 output = 'rdata/anomaly/daily1.S10.RData') 

daily1.S25.df = adddaily.anomaly(data = daily1.df, usepercent = T,TotalNpercent = 25, 
                                 Anomalytype = "Point", day = c("2015-11-15"), nperiod=c(1),
                                 Leafpercent = rbind(c(1,
                                                       1,0,
                                                       1,0,0,0)),
                                 output = 'rdata/anomaly/daily1.S25.RData') 

daily1.S50.df = adddaily.anomaly(data = daily1.df, usepercent = T,TotalNpercent = 50, 
                                 Anomalytype = "Point", day = c("2015-11-15"), nperiod=c(1),
                                 Leafpercent = rbind(c(1,
                                                       1,0,
                                                       1,0,0,0)),
                                 output = 'rdata/anomaly/daily1.S50.RData') 

daily1.S100.df = adddaily.anomaly(data = daily1.df, usepercent = T,TotalNpercent = 100, 
                                 Anomalytype = "Point", day = c("2015-11-15"), nperiod=c(1),
                                 Leafpercent = rbind(c(1,
                                                       1,0,
                                                       1,0,0,0)),
                                 output = 'rdata/anomaly/daily1.S100.RData') 

daily1.S250.df = adddaily.anomaly(data = daily1.df, usepercent = T,TotalNpercent = 250, 
                                 Anomalytype = "Point", day = c("2015-11-15"), nperiod=c(1),
                                 Leafpercent = rbind(c(1,
                                                       1,0,
                                                       1,0,0,0)),
                                 output = 'rdata/anomaly/daily1.S250.RData') 

daily1.S500.df = adddaily.anomaly(data = daily1.df, usepercent = T,TotalNpercent = 500, 
                                 Anomalytype = "Point", day = c("2015-11-15"), nperiod=c(1),
                                 Leafpercent = rbind(c(1,
                                                       1,0,
                                                       1,0,0,0)),
                                 output = 'rdata/anomaly/daily1.S500.RData') 
daily1.S1000.df = adddaily.anomaly(data = daily1.df, usepercent = T,TotalNpercent = 1000, 
                                 Anomalytype = "Point", day = c("2015-11-15"), nperiod=c(1),
                                 Leafpercent = rbind(c(1,
                                                       1,0,
                                                       1,0,0,0)),
                                 output = 'rdata/anomaly/daily1.S1000.RData') 

#############################################################################

### Output files (already there from adddaily function, but do this just incase) 

save(daily1.S10.df,file   = "rdata/anomaly/daily1.S10.RData")
save(daily1.S25.df,file   = "rdata/anomaly/daily1.S25.RData")
save(daily1.S50.df,file   = "rdata/anomaly/daily1.S50.RData")
save(daily1.S100.df,file  = "rdata/anomaly/daily1.S100.RData")
save(daily1.S250.df,file  = "rdata/anomaly/daily1.S250.RData")
save(daily1.S500.df,file  = "rdata/anomaly/daily1.S500.RData")
save(daily1.S1000.df,file = "rdata/anomaly/daily1.S1000.RData")

################################################################################

### plot data

ggdailyplot(daily1.S10.df  [3000:4000,],nameB1= "plot/anomaly/ggdaily1.S10.B1jpg",   nameLeaf= "plot/anomaly/ggdaily1.S10.leaf.jpg")
ggdailyplot(daily1.S25.df  [3000:4000,],nameB1= "plot/anomaly/ggdaily1.S25.B1.jpg",  nameLeaf= "plot/anomaly/ggdaily1.S25.leaf.jpg")
ggdailyplot(daily1.S50.df  [3000:4000,],nameB1= "plot/anomaly/ggdaily1.S50.B1.jpg",  nameLeaf= "plot/anomaly/ggdaily1.S50.leaf.jpg")
ggdailyplot(daily1.S100.df [3000:4000,],nameB1= "plot/anomaly/ggdaily1.S100.B1.jpg", nameLeaf= "plot/anomaly/ggdaily1.S100.leaf.jpg")
ggdailyplot(daily1.S250.df [3000:4000,],nameB1= "plot/anomaly/ggdaily1.S250.B1.jpg", nameLeaf= "plot/anomaly/ggdaily1.S250.leaf.jpg")
ggdailyplot(daily1.S500.df [3000:4000,],nameB1= "plot/anomaly/ggdaily1.S500.B1.jpg", nameLeaf= "plot/anomaly/ggdaily1.S500.leaf.jpg")
ggdailyplot(daily1.S1000.df[3000:4000,],nameB1= "plot/anomaly/ggdaily1.S1000.B1.jpg",nameLeaf= "plot/anomaly/ggdaily1.S1000.leaf.jpg")














