posttheta <-function(rho = rho,
                     MCMC = model6.sim,
                     k = c(0.8,0.85,0.9,0.95,0.99)){
  
  allpost = matrix()
  for (j in 1:length(k)){
    
    k2 = k[j]
    posttheta = data.frame()
    
    for (h in 1:3){
      for (i in 1:ncol(MCMC[[h]])){
        posttheta[i,h] = sum(MCMC[[h]][1:1000,i] >= rep(rho[i]*k2,1000))/1000
      }}
    posttheta= round(rowMeans(posttheta2),4)
    
    allpost[[j]] = posttheta2
  }
  allpost

  posttheta = matrix()
  for (j in 1:length(k)){
    k2=k[j]
  for (i in 1:7){
    posttheta[i,j] = sum(MCMC[[1]][,i] >= originalrho[i]*k2)/1000
  }}
    posttheta
    
    
  level = c(0,1,1,2,2,2,2)
  lv1row = c("-","A","B","A","A","B","B")
  lv2row = c("-","-","-","A","A","B","B")
  name = c("Total","A","B","AA","AB","BA","BB")
  colnamez = c("i","j","Name","theta >  threshold")
  posttheta
  
  data2 = cbind(lv1row,lv2row,name,posttheta)
  
  colnames(data2) = c("i","j","Name","$\theta$ > threshold")

  return( data2)
  
}

# ------------------------------------------------------------ #

posttheta2 <-function(originalrho = rho,
                     MCMC = model6.sim,
                     k = 1/1.001){
  posttheta = c()
  for (i in 1:7){
    posttheta[i] = sum(MCMC[[1]][,i] >= originalrho[i]*(1/k))/1000
  }
  

  name = c("Anomaly0","Anomaly10","Anomaly25","Anomaly50","Anomaly100","Anomaly250","Anomaly500")
  colnamez = c("Name","theta >  threshold")
  posttheta
  
  data2 = cbind(name,posttheta)
  
  colnames(data2) = c("Name","$\theta$ > threshold")
  
  return( data2)
  
}

modelnormh.MCMC

# ------------------------------------------------------------ #

postthetam <-function(  rho = rhonorm,
                       rhoh = rhonormh,
                       MCMC = modelnorm.MCMC,
                      MCMCh = modelnormh.MCMC,
                          k = c(0.8,0.85,0.9,0.95,0.99)){
  
  allpost = list()
  for (j in 1:length(k)){
  
  k2 = k[j]
  posttheta = data.frame()
  
  for (h in 1:3){
  for (i in 1:ncol(MCMC[[h]])){
    posttheta[i,h] = sum(MCMC[[h]][1:1000,i] >= rep(rho[i]*k2,1000))/1000
  }}
  posttheta = round(rowMeans(posttheta),4)
  
  allpost[[j]] = posttheta
  }
  
  
  allpost2 = list()
  for (j in 1:length(k)){
    
    k2 = k[j]
    posttheta2 = data.frame()
    
    for (h in 1:3){
      for (i in 1:ncol(MCMCh[[h]])){
        posttheta2[i,h] = sum(MCMCh[[h]][1:1000,i] >= rep(rhoh[i]*k2,1000))/1000
      }}
    posttheta2= round(rowMeans(posttheta2),4)
    
    allpost2[[j]] = posttheta2
  }

  mat1 = matrix(unlist(allpost),nrow=ncol(MCMC[[h]]),byrow = F,dimnames = list(colnames(MCMC[[1]]),paste("k=",k)))
  mat2 = matrix(unlist(allpost2),nrow=ncol(MCMC[[h]]),byrow = F,dimnames = list(colnames(MCMC[[1]]),paste("k=",k,",h")))
  
  finalmat = cbind(mat1,mat2)

  return(finalmat)
  
}

postthetampic <-function(  rho = rhonorm,
                        rhoh = rhonormh,
                        MCMC = modelnorm.MCMC,
                        MCMCh = modelnormh.MCMC,
                        k = c(0.8,0.85,0.9,0.95,0.99),
                        output = "plots/mimic/heatnorm.PDF"){
  
  allpost = list()
  for (j in 1:length(k)){
    
    k2 = 1/k[j]
    posttheta = data.frame()
    
    for (h in 1:3){
      for (i in 1:ncol(MCMC[[h]])){
        posttheta[i,h] = sum(MCMC[[h]][1:1000,i] >= rep(rho[i]*k2,1000))/1000
      }}
    posttheta = round(rowMeans(posttheta),4)
    
    allpost[[j]] = posttheta
  }
  
  
  allpost2 = list()
  for (j in 1:length(k)){
    
    k2 = 1/k[j]
    posttheta2 = data.frame()
    
    for (h in 1:3){
      for (i in 1:ncol(MCMCh[[h]])){
        posttheta2[i,h] = sum(MCMCh[[h]][1:1000,i] >= rep(rhoh[i]*k2,1000))/1000
      }}
    posttheta2= round(rowMeans(posttheta2),4)
    
    allpost2[[j]] = posttheta2
  }
  
  mat1 = matrix(unlist(allpost),nrow=ncol(MCMC[[h]]),byrow = F,dimnames = list(colnames(MCMC[[1]]),paste(k)))
  mat2 = matrix(unlist(allpost2),nrow=ncol(MCMC[[h]]),byrow = F,dimnames = list(colnames(MCMC[[1]]),paste(k)))

  pal <- rev(brewer.pal(n = 10, name = "RdYlBu"))
  plot1 <- levelplot(t(mat1), main="Independent Bayesian Model", xlab="k", ylab="", col.regions=pal, cuts=9)
  plot2 <- levelplot(t(mat2), main="Hierarchical Bayesian Model", xlab="k", ylab="", col.regions=pal, cuts=9)
  
  grid.arrange(plot1,plot2, nrow=2)
  
  png(output,width = 850, height = 1080)
  grid.arrange(plot1,plot2, nrow=2)
  dev.off()
}





#Build the palette and plot it
