source("00-permeables.R")
source("01-rand.day.time.R")
source("02-rand.day.R")

# Function 3 

## Function to simulate raw data

### 1,000 to 1,000,000 simulation recommended 
# time consuming parts
# - create id
# - as.character(as.date(time))
# - ifelse to assign leaf dummies


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
  
  ### sort the order of cat1 and cat2 variables
  catnames = names(Category[4:length(Category)])
  cat1.name.sorted = sort(catnames[(1):(cat1.n)])
  cat2.name.sorted = sort(catnames[(cat1.n+1):(length(catnames))])
  cat.name.sorted = c(cat1.name.sorted,cat2.name.sorted)
  cat.name.sorted
  
  Category = cbind(Category[,1:3],Category[,cat.name.sorted])
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
  #    user  system elapsed 
  #    0.06    0.00    0.06
  #
  # > system.time(simdata (1000000))
  #    user  system elapsed 
  #   54.38    0.60   54.96 
  
}
