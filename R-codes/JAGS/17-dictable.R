

dictable<- function(){
  
  M1M2 <- summary(diffdic(dic.mod1, dic.mod2))
  M1M3 <- summary(diffdic(dic.mod1, dic.mod3))
  M1M4 <- summary(diffdic(dic.mod1, dic.mod4))
  M1M5 <- summary(diffdic(dic.mod1, dic.mod5))
  M1M6 <- summary(diffdic(dic.mod1, dic.mod6))
  M2M3 <- summary(diffdic(dic.mod2, dic.mod3))
  M2M4 <- summary(diffdic(dic.mod2, dic.mod4))
  M2M5 <- summary(diffdic(dic.mod2, dic.mod5))
  M2M6 <- summary(diffdic(dic.mod2, dic.mod6))
  M3M4 <- summary(diffdic(dic.mod3, dic.mod4))
  M3M5 <- summary(diffdic(dic.mod3, dic.mod5))
  M3M6 <- summary(diffdic(dic.mod3, dic.mod6))
  M4M5 <- summary(diffdic(dic.mod4, dic.mod5))
  M4M6 <- summary(diffdic(dic.mod4, dic.mod6))
  M5M6 <- summary(diffdic(dic.mod5, dic.mod6))
  
  compare_model = rbind(M1M2,M1M3,M1M4,M1M5,M1M6,
                        M2M3,M2M4,M2M5,M2M6,
                        M3M4,M3M5,M3M6,
                        M4M5,M4M6,
                        M5M6)
  
  row.names(compare_model) = c("M1:M2","M1:M3","M1:M4","M1:M5",'M1:M6',
                               "M2:M3","M2:M4","M2:M5","M2:M6",
                               "M3:M4","M3:M5","M3:M6",
                               "M4:M5","M4:M6",
                               "M5:M6")
  return(compare_model)
  
}

dictablesim1<- function(){
  
  M1M1 <- summary(diffdic(dic.mod1, dic.mod1h))
  M2M2 <- summary(diffdic(dic.mod2, dic.mod2h))
  M3M3 <- summary(diffdic(dic.mod3, dic.mod3h))
  M4M4 <- summary(diffdic(dic.mod4, dic.mod4h))
  M5M5 <- summary(diffdic(dic.mod5, dic.mod5h))
  M6M6 <- summary(diffdic(dic.mod6, dic.mod6h))
  M7M7 <- summary(diffdic(dic.mod7, dic.mod7))
  
  compare_model = rbind(M1M1,M2M2,M3M3,M4M4,M5M5,M6M6,M7M7)
  
  row.names(compare_model) = c("Anomaly0","Anomaly10","Anomaly25","Anomaly50","Anomaly100","Anomaly250","Anomaly500")
  return(compare_model)
  
}