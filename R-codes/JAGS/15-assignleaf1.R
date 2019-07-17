

assignleaf1 <- function(muleaf = muleaf){  # create a dataframe of 1 day, of mu for each obs at each level muleaf =muleaf1
  
  muleaf.matrix = matrix(muleaf,ncol=n.cat1 ,byrow=T)
  rownames(muleaf.matrix) = levels(rawdata$cat2)
  colnames(muleaf.matrix) = levels(rawdata$cat1)
  muleaf.matrix
  mucat1 = colSums(muleaf.matrix) 
  muTotal = sum(mucat1)
  
  muleaf
  mucat1
  muTotal
  
  rho.df = daily.df[1,]
  
  p1_date <- as.POSIXlt(as.Date(max(as.POSIXlt(daily.df$day)))+1, format = "%m/%d/%Y") # oldest date in the DF.
  rho.df$day =   p1_date
  
  head(rho.df)
  for (i in 1:n.cat1)
    for (j in 1:n.leaf){
      rho.df[,3] = rep(muTotal,1)
      rho.df[,3+i] = rep(mucat1[i],1)
      rho.df[,3+n.cat1+j] = rep(muleaf[j],1)
    }
  head(rho.df)
  return(rho.df)
}