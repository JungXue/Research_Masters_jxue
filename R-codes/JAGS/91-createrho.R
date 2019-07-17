

source("11-rhodata.R")





######### Create Rho 


daily.S10.df <- read.csv("data/daily1.S10.csv")
daily.S50.df <- read.csv("data/daily1.S50.csv")
daily.S100.df <- read.csv("data/daily1.S100.csv")
daily.S500.df <- read.csv("data/daily1.S500.csv")
daily.S1000.df <- read.csv("data/daily1.S1000.csv")
daily.S5000.df <- read.csv("data/daily1.S5000.csv")


### Subset data 

testdate = "2015-11-15"
day = which(daily.df$day ==testdate)

daily.S10.df   <- daily.S10.df  [-(day:nrow(daily.S10.df)),-(1:2)]
daily.S50.df   <- daily.S50.df  [-(day:nrow(daily.S50.df)),-(1:2)]
daily.S100.df  <- daily.S100.df [-(day:nrow(daily.S100.df)),-(1:2)]
daily.S500.df  <- daily.S500.df [-(day:nrow(daily.S500.df)),-(1:2)]
daily.S1000.df <- daily.S1000.df[-(day:nrow(daily.S1000.df)),-(1:2)]
daily.S5000.df <- daily.S5000.df[-(day:nrow(daily.S5000.df)),-(1:2)]

tail(daily.S10.df )
tail(daily.S50.df )
tail(daily.S100.df )
tail(daily.S500.df )
tail(daily.S1000.df )
tail(daily.S5000.df )


### workout Rho 

raw.df <- read.csv("data/raw1.csv")

Rho.S10.ist    = rhodata(raw.df,daily.S10.df,  "data/Rho.S10.xlsx",  "1daypred","simulation")
Rho.S50.list   = rhodata(raw.df,daily.S50.df,  "data/Rho.S50.xlsx",  "1daypred","simulation")
Rho.S100.list  = rhodata(raw.df,daily.S100.df, "data/Rho.S100.xlsx", "1daypred","simulation")
Rho.S500.list  = rhodata(raw.df,daily.S500.df, "data/Rho.S500.xlsx", "1daypred","simulation")
Rho.S1000.list = rhodata(raw.df,daily.S1000.df,"data/Rho.S1000.xlsx","1daypred","simulation")
Rho.S5000.list = rhodata(raw.df,daily.S5000.df,"data/Rho.S5000.xlsx","1daypred","simulation")

Rho.S10.list <- read_excel_allsheets("test1.xlsx")
Rho.S50.list <- read_excel_allsheets("test1.xlsx")
Rho.S100.list <- read_excel_allsheets("test1.xlsx")
Rho.S500.list <- read_excel_allsheets("test1.xlsx")
Rho.S1000.list<- read_excel_allsheets("test1.xlsx")
Rho.S5000.list <- read_excel_allsheets("test1.xlsx")

Rho.S10.list 
Rho.S50.list 
Rho.S100.list
Rho.S500.list 
Rho.S1000.list
Rho.S5000.list 



