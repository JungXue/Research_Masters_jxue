source("00-permeables.R")
source("01-rand.day.time.R")
source("02-rand.day.R")
source("03-simdata.R")
source("04-cumdata.R")
source("05-ggcumplot.R")
source("06-tabulatedata.R")

# Function 7

## Function to create daily plot

ggdailyplot.test <- function(){
  cat2.val = rbind(            
    c(200,200,100),                 
    c(200,200, 50),
    c(100,  0,  0))
  
  cat2.val= cbind(            
    c(200,185,100,  0),                 
    c(150,200, 50, 15),
    c(100,  0,  0,  0))
  
  sample1 = simdata (100000,cat2.val = cat2.val)
  time.start = "2006/01/01 00:00:01"
  time.end = "2018/12/31 23:59:59"
  
  head(sample1)
  str(sample1)
  x = sample1
  
  dailycount.df = tabulatedata(sample1)
  data=dailycount.df
  head(data)
  
  data = daily1.S10.df [3000:4000,]
}


ggdailyplot <- function(data = dailycount.df,
                        nameB1= "ggdailyplotB1.jpg",
                        nameLeaf= "ggdailyplotleaf.jpg"){
  library(ggplot2)
  
  namez = table(nchar(names(data)))
  nB1 =  namez[1]
  nleaf = namez[2]
  
  dailycountleaf.df = data [,c(1,(3+nB1):length(data))]
  dailycountleaf.df[1:10,]
  dailycountbr1.df = data [,c(1,3:(3+nB1-1))]
  dailycountbr1.df [1:10,]
  
  gatherdailyLeaf.df <- gather(dailycountleaf.df,key = 'Leaf',value = 'count',2:length(dailycountleaf.df))
  gatherdailyLeaf.df$Leaf = factor(gatherdailyLeaf.df$Leaf)
  gatherdailyLeaf.df[1:10,]

  gatherdailyB1.df <- gather(dailycountbr1.df,key = 'Brunch1',value = 'count',2:length(dailycountbr1.df))
  gatherdailyB1.df$Brunch1 = factor(gatherdailyB1.df$Brunch1)
  gatherdailyB1.df[1:10,]
  
  levels(gatherdailyLeaf.df$Leaf)
  levels(gatherdailyB1.df$Brunch1)
  
  jpeg(nameLeaf, width = 800, height = nleaf*100+100)
  plot1 = ggplot(gatherdailyLeaf.df,aes(x = day, y = count, group = Leaf, col = Leaf)) + 
    geom_step() + 
    facet_grid(Leaf ~ .) +       
    ggtitle("dailycount plot for the leafs") +
    ylab("Daily counts") + 
    xlab("time") 
  print(plot1)
  dev.off()
  
  jpeg(nameB1, width = 800, height = nB1*200+100)
  plot2 = ggplot(gatherdailyB1.df,aes(x = day, y = count, group = Brunch1, col = Brunch1)) + 
    geom_step() + 
    facet_grid(Brunch1 ~ .) +       
    ggtitle("dailycount plot for the first brunchs") +
    ylab("Daily counts") + 
    xlab("time") 
  print(plot2)
  dev.off()
}