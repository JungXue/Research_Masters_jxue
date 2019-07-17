library(parallel)

  ### number of cores

  no_cores <- detectCores()  # 8 core
  
  # Setup cluster
  clust <- makeCluster(no_cores) 
  
  
  base <- 4
  #Note that this line is required so that all cores in cluster have this variable available
  clusterExport(clust, "base")