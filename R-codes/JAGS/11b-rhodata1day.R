# setwd("H:/Desktop/7.1 No time to waste/Stats 798A Research Master/R-codes/JAGS")

source("12-htsmodel.R")
source("14-assignleaf.R")
source("15-assignleaf1.R")


###################################################################################

# Testing
rhodata.test <- function(){
  
    raw.df <- read.csv("data/raw1.csv")
    cum.df <- read.csv("data/cum1.csv")
  daily.df <- read.csv("data/daily1.csv")
  
     rawdata <- raw.df
   dailydata <- daily.df
  outputdata <- "rdata/function/rho1day.RData"
        type <- "1daypred"
        data <- "simulation"
  
}

##################################################################################

# Create Rho



rhodata1day <- function(rawdata = raw.df,     
                      dailydata = daily.df,
                     outputdata = "rdata/function/rho1day.RData", # outputfile as csv, so we dont hav to recalc for ages 
                           type = "allpred",                      # all sim pred or 1 day pred "allpred"  or "1daypred"
                           data = "simulation"){                  # simulation, MIMIC or Box jellyfish
  
### Number of observation, brunch and leafs, for simulation
  
  n.cat1 <- 2    # length(levels(rawdata$cat1))
  n.leaf <- 4    # length(levels(rawdata$leaf))
       N <- 4748 # nrow(dailydata)
  
### simple and time-series predictions for each leaf and level
 
  # method 1 same mu for all leaf
  # method 2 diff mu, base on leaf mean
       
  # method 3 ts prediction on total -> leaf (top down)
  # method 4 ts prediction on leafs (agg to total) (bottom up)
  # method 5 optimal reconcilation (hts package )
  # https://cran.r-project.org/web/packages/hts/hts.pdf
  
  # simple preds
  
  muleaf1 = rep(sum(colMeans(dailydata[(3 + n.cat1+1):length(dailydata)]))/n.leaf,n.leaf)
  muleaf2 = colMeans(dailydata[(3 + n.cat1+1):length(dailydata)])
  
  # hts preds
  
  sim.list = list(2,c(2,2))          # how was grouping structured
  MIMIC.list = list(2,c(2,2))        # work this out<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
  BoxJellyfish.list = list(2,c(2,2)) # work this out<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
  
  simname = c("AA","AB","BA","BB")          # name of categories & leaf combined
  #          MIMICname = read.csv("MIMICname.csv")       # add with a csv file<<<<<<<<<<<<<<<<<<<
  #    BoxJellfishname = read.csv("BoxJellfishname.csv") # add with a csv file<<<<<<<<<<<<<<<<<<
  
  simread = c(1,1)  # how to read names
  #          MIMICread = c(1,1)
  #    BoxJellfishread = c(1,1)
  
  htsleafs = switch(data, "simulation"    = rhodata.htspred(daily.df,          sim.list,        simname,        simread),
                          "MIMIC"         = rhodata.htspred(daily.df,        MIMIC.list,      MIMICname,      MIMICread),
                          "Box jellyfish" = rhodata.htspred(daily.df, BoxJellyfish.list,BoxJellfishname,BoxJellfishread))
  # htsleafs = rhodata.htspred("daily1.csv",list(2,c(2,2)),c("AA","AB","BA","BB"),c(1,1))
  
  # htsleafs = rhodata.htspred("daily1.csv",list(2,c(2,2)),c("AA","AB","BA","BB"),c(1,1))
  # htsleafs = leafvals
  
  htsleafs
  
  ### reassign all mu
  muleaf1.df <- assignleaf(muleaf1)
  muleaf2.df <- assignleaf(muleaf2)
  #unable to do hts forecastign for all, took way too much time
  
  rho.list.df1 <-list("theoritical" = muleaf1.df,
                      "mean"        = muleaf2.df)
  
  ### assign 1 mu to 1 prediction day
  
  muleaf1.df <- assignleaf1(muleaf1)
  muleaf2.df <- assignleaf1(muleaf2)
  muleaf3.df <- assignleaf1(htsleafs[[1]])
  muleaf4.df <- assignleaf1(htsleafs[[2]])
  muleaf5.df <- assignleaf1(htsleafs[[3]])
  
  rho.list.df2 <-list("theoritical" = muleaf1.df,
                      "mean"        = muleaf2.df,
                      "optimcomb"   = muleaf3.df,
                      "bottomup"    = muleaf4.df,
                      "topdownfp"   = muleaf5.df)
  
  rho.list.df <- switch( type, "allpred"   = rho.list.df1,
                               "1daypred"  = rho.list.df2)
  rho.list.df
  
  # write as csv 
  # excel_export(rho.list.df,outputxlsx,table_names=c("theoritical","mean","optimcomb","bottomup","topdownfp"))
  
  save(rho.list.df,file=outputdata)
  # load(outputdata)

  return(rho.list.df)
  
}















#############

# Final tests

rhodata.test2 <- function(){
  
  test = rhodata(raw.df,daily.df,"rho.csv","1daypred","simulation")
  test
  
  test = rho.list.df
  test$theoritical
  
  cat(as.yaml(test), file='blah.txt')
  test2 <- yaml.load_file('blah.txt')
  test2
  test2[[1]]
  
  
  library(ImportExport)
  excel_export(test,"test1.xlsx",table_names=c("theoritical","mean","optimcomb","bottomup","topdownfp"))
  read_excel_allsheets <- function(filename, tibble = FALSE) {
    sheets <- readxl::excel_sheets(filename)
    x <- lapply(sheets, function(X) readxl::read_excel(filename, sheet = X))
    if(!tibble) x <- lapply(x, as.data.frame)
    names(x) <- sheets
    x
  }
  test3 = read_excel_allsheets("test1.xlsx")
  test3
  
  
}


