
daily.df <- read.csv("daily3.csv")

sortcountdata <- function(data = daily.df){
  
  catleafcount = table(nchar(names(data[4:length(data)])))
  ncat = catleafcount[1]
  nleaf = catleafcount[2]
  cat1loc =(3+1):(3+ncat)
  leafloc = (3+1+ncat):(length(data))
  
  sortcat1 = sort(names(data[cat1loc]))
  sortleaf = sort(names(data[leafloc]))
  
  data2 = cbind(data[,1:3],data[,sortcat1],data[,sortleaf])
  return(data2)
}

daily.df=sortcountdata(daily.df)
head(daily.df)
daily.df[7,]