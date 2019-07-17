
# setwd("H:/Desktop/7.1 No time to waste/Stats 798A Research Master/R-codes/Simulation")

source("00-permeables.R")
source("01-rand.day.time.R")
source("02-rand.day.R")
source("03-simdata.R")
source("04-cumdata.R")
source("05-ggcumplot.R")
source("06-tabulatedata.R")
source("07-ggdailyplot.R")

set.seed(1234567)

########################################################################################

### Custom matrixes

matrix1 = cbind(            
  c(100,100),                 
  c(100,100))

matrix2 = cbind(            
  c(100,100,100,100),                 
  c(100,100,100,100),
  c(100,100,100,100))

matrix3 = matrix(sample.int(1000, 4),nrow=2,byrow = T)           

matrix4 = matrix(sample.int(1000, 15),nrow=5,byrow = T)

matrix1
matrix2
matrix3
matrix4

########################################################################################

### Create raw cum and daily data

t.start = "2006/01/01 00:00:01"   
t.end = "2018/12/31 23:59:59"


# https://www.stuff.co.nz/auckland/106247467/auckland-city-hospital-emergency-department-straining-under-unprecedented-winter-demand
# total = 200

raw1.df = simdata (100000,cat2.val = matrix1, time.start = t.start, time.end = t.end)
raw2.df = simdata (100000,cat2.val = matrix2, time.start = t.start, time.end = t.end)
raw3.df = simdata (100000,cat2.val = matrix3, time.start = t.start, time.end = t.end)
raw4.df = simdata (100000,cat2.val = matrix4, time.start = t.start, time.end = t.end)

head(raw1.df)
head(raw2.df)
head(raw3.df)
head(raw4.df)

### Create cumulative data

cum1.df = cumdata(raw1.df)
cum2.df = cumdata(raw2.df)
cum3.df = cumdata(raw3.df)
cum4.df = cumdata(raw4.df)

head(cum1.df)
head(cum2.df)
head(cum3.df)
head(cum4.df)

### Create daily data

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

#############################################################################

### Output files

save(raw1.df,file = "rdata/test/raw1.RData")
save(raw2.df,file = "rdata/test/raw2.RData")
save(raw3.df,file = "rdata/test/raw3.RData")
save(raw4.df,file = "rdata/test/raw4.RData")

save(cum1.df,file = "rdata/test/cum1.RData")
save(cum2.df,file = "rdata/test/cum2.RData")
save(cum3.df,file = "rdata/test/cum3.RData")
save(cum4.df,file = "rdata/test/cum4.RData")

save(daily1.df,file = "rdata/test/daily1.RData")
save(daily2.df,file = "rdata/test/daily2.RData")
save(daily3.df,file = "rdata/test/daily3.RData")
save(daily4.df,file = "rdata/test/daily4.RData")

################################################################################

### Output ggplots

ggdailyplot(daily1.df,nameB1= "plot/test/ggdaily1.B1.jpg",nameLeaf= "plot/test/ggdaily1.leaf.jpg")
ggdailyplot(daily2.df,nameB1= "plot/test/ggdaily2.B1.jpg",nameLeaf= "plot/test/ggdaily2.leaf.jpg")
ggdailyplot(daily3.df,nameB1= "plot/test/ggdaily3.B1.jpg",nameLeaf= "plot/test/ggdaily3.leaf.jpg")
ggdailyplot(daily4.df,nameB1= "plot/test/ggdaily4.B1.jpg",nameLeaf= "plot/test/ggdaily4.leaf.jpg")

