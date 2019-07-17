source("00-permeables.R")
source("01-rand.day.time.R")
source("02-rand.day.R")
source("03-simdata.R")
source("04-cumdata.R")

### Function 5 ###

### codes for debugging ggcumplot 

ggcumplot.test <- function(){
  
  cat2.val = rbind(            
    c(200,200,100),                 
    c(200,200, 50),
    c(100,  0,  0))
  
  cat2.val= cbind(            
    c(200,185,100,  0),                 
    c(150,200, 50, 15),
    c(100,  0,  0,  0))
  
  cat2.val = cbind(            
    c(200,200),                 
    c(100,500))
  
  sample1 = simdata (100000,cat2.val = cat2.val)
  time.start = "2006/01/01 00:00:01"
  time.end = "2018/12/31 23:59:59"
  
  head(sample1)
  str(sample1)
  x = sample1
  
  cumdata.df = cumdata(sample1)
  head(cumdata.df)
  
  data=cumdata.df
  
}



ggcumplot <- function(data=cumdata.df,
                      nameB1= "ggcumplotB1.jpg",
                      nameLeaf= "ggcumplotleaf.jpg"){
  
  library(ggplot2)
  
  nB1 =  length(levels(data$cat1))
  nleaf = ncol(data) - 8 - nB1
  head(data)
  cumLeaf.df = data[,c(8,(8+nB1+1):(8+nB1+nleaf))]
  cumLeaf.df[1:10,]
  cumCat1.df = data[,c(6,9:(9+nB1-1))]
  cumCat1.df [1:10,]
  
  gatherLeaf.df <- gather(cumLeaf.df,key = 'parameterLeaf',value = 'Value',2:length(cumLeaf.df))
  gatherLeaf.df$index = rep(1:nrow(data),nleaf)
  gatherLeaf.df[1:10,]
  
  gatherB1.df <- gather(cumCat1.df,key = 'parameterB1',value = 'Value',2:length(cumCat1.df ))
  gatherB1.df$index = rep(1:nrow(data),nB1)
  gatherB1.df[1:10,]
  
  jpeg(nameLeaf, width = 800, height = nleaf*100+100)
  plot1 = ggplot(gatherLeaf.df,aes(x = index, y = Value, group = parameterLeaf, col = parameterLeaf)) + 
    geom_step() + 
    facet_grid(parameterLeaf ~ .) +       #cat1 for other plot
    ggtitle("Cumulative plot for the leafs") +
    ylab("Cumulative counts") + 
    xlab("index")
  print(plot1)
  dev.off()
  
  jpeg(nameB1, width = 800, height = nB1*200+100)
  plot2 = ggplot(gatherB1.df,aes(x = index, y = Value, group = parameterB1, col = parameterB1)) + 
    geom_step() + 
    facet_grid(parameterB1 ~ .) +       #cat1 for other plot
    ggtitle("Cumulative plot for the first brunchs") +
    ylab("Cumulative counts") + 
    xlab("index") 
  print(plot2)
  dev.off()
}

### Test ggcumplot
ggcumplot.test2 <- function(){
  
  ggcumplot(cumdata.df)
  
}