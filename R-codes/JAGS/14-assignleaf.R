
muleaf = muleaf2 
assignleaf <- function(muleaf = muleaf){  # create a datafrmae of mu for each obs at each level muleaf =muleaf1

  muleaf.matrix = matrix(muleaf,nrow = 2, ncol=n.cat1,byrow=F)
  rownames(muleaf.matrix) = levels(rawdata$cat2)
  colnames(muleaf.matrix) = levels(rawdata$cat1)
  muleaf.matrix
  mucat1 = colSums(muleaf.matrix) 
  muTotal = sum(mucat1)
  
  muleaf
  mucat1
  muTotal
  
  rho.df = daily.df
  head(rho.df)
  for (i in 1:n.cat1)
    for (j in 1:n.leaf){
      rho.df[,2] = rep(muTotal,N)
      rho.df[,2+i] = rep(mucat1[i],N)
      rho.df[,2+n.cat1+j] = rep(muleaf[j],N)
    }
  head(rho.df)
  return(rho.df)
}


setwd("H:/Desktop/7.1 No time to waste/Stats 798A Research Master/R-codes/JAGS")

save(rho.df,file = "rdata/sim2/rho1.RData")