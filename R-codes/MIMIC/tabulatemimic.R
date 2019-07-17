
#x <- MIMIC3.final.df

tabulatemimic <- function(x=sample1){
  
  ### figure out level of days 
  
  # tz(x$time.day) est, right
  time.start = min(as.Date(x[,4]),na.rm = TRUE)
  time.end =  max(as.Date(x[,4]),na.rm = TRUE)
  
  time.start.int = as.integer(as.Date(time.start, units = "days")) 
  time.end.int = as.integer(as.Date(time.end , units = "days")) 
  time.start.int
  time.end.int
  
  library("zoo")
  timelevel =  as.factor(as.Date(time.start.int:time.end.int))
  head(timelevel)
  tail(timelevel)
  # 40284/365 = 110.3671
  
  ### colnames of tabulated data 
  catname = c("day","total",names(x)[10:length(x)])
  catname
  
  catname2 = catname[3:length(catname)]
  
  cat1names = sort(catname2[1:17])
  cat2names = sort(catname2[(17+1):149])
  cat3names = sort(formatC(as.numeric(catname2[(149+1):length(catname2)]), width=3, flag="0"))

  catname3 = c(cat1names,cat2names,cat3names)
  
 ### dates
  
  datedata = factor(as.Date(x[,4]),levels=timelevel)
  str(datedata)
  head(sort(datedata))
  tail(sort(datedata))
  
  daily.df = data.frame(matrix(NA,
                               nrow = length(timelevel),
                               ncol = length(x)-8+2-1,
                               dimnames = list(1:length(timelevel),catname)
                               ))
  
  for(i in 1:(ncol(daily.df)-2)){
    daily.df[,1] = timelevel
    daily.df[,2] = tabulate(sort(datedata))
    daily.df[,(2+i)] = table(datedata,x[,(9+i)])[,2]
  }
  
  colnames(daily.df) = catname
  head(daily.df)
  colnames(daily.df) = c(names(daily.df)[1:151],formatC(as.numeric(names(daily.df)[152:length(names(daily.df))]), width=3, flag="0"))

  
  daily.df = daily.df[,c("day","total",catname3)]
  
  
  # table(daily.df$total)
  # 0     1     2     3     4     5     6     7     8     9 
  # 12601 13058  8606  4000  1423   420   109    22     7     2 
  
  all(daily.df$total == rowSums(daily.df[, 3:19]))    
  all(daily.df$total == rowSums(daily.df[,20:151]))
  all(daily.df$total == rowSums(daily.df[,152:782]))
  
  
  return(daily.df)
  return()
}