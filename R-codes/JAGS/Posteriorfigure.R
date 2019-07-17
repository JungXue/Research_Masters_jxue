# http://www.cookbook-r.com/Graphs/Plotting_means_and_error_bars_(ggplot2)/

library(ggplot2)
set.seed(45)

size = c( 0.1, 0.5, 1,5,10,50)
test <- data.frame(abnsize=rep(size, 3), val=sample(1:100, 18), 
                 variable=rep(paste0("category", 1:3), each=6),
                 U = sample(1:100, 18)+1,
                 L = sample(1:100, 18)-1)
test$abnsize <- log(test$abnsize)
str(test)

pd <- position_dodge(0.1) # move them .05 to the left and right

#log 

# plot
ggplot(data = test, aes(x=abnsize, y=val,colour=variable)) + geom_line() +
  geom_errorbar(aes(ymin=L , ymax=U), colour="black", width=.1, position=pd) +
  ggtitle("Figure 3. \nPosterior value by size of anomaly") +
  xlab("Anomaly size") +
  ylab("Value of theta") +
  geom_point(position=pd, size=3, shape=21, fill="white")+
  theme_bw() +
  theme(legend.justification=c(1,0),
        legend.position=c(1,0))