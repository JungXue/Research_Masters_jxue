
# setwd("H:/Desktop/7.1 No time to waste/Stats 798A Research Master/R-codes/JAGS")

Packages=c("rmutil","ggplot2","pdp")
lapply(Packages, library, character.only = TRUE)


###------------------------create prior with hyper priors-------------------------------#

N<-10000

### Null model--------------------------------

y1 <- rep(1,N)               
y1.df <- data.frame(y1)

any(y1<0)
plot(density(y1))


### normal 1,0.3 prior,truncated at 0-----------

mu2.y2 = rnorm(N,1,0.1)   
sigma2.y2 = rnorm(N,0.3,0.1)
y2 <- rnorm(N, mu2.y2, sqrt(sigma2.y2))
y2 <- y2[-which(y2<0)]
y2.df <- data.frame(y2)

any(y2<0)
plot(density(y2))

### normal 1.5,0.3 prior,truncated at 0-----------

mu2.y3 = rnorm(N,1,0.1)   
sigma2.y3 = rnorm(N,0.1,0.1)
y3 <- rnorm(N, mu2.y3, sqrt(sigma2.y3))
y3 <- y3[-which(y3<0)]
y3.df <- data.frame(y3)

any(y3<0)
plot(density(y3))



### Gamma 4,3 prior ---------------------------

alpha.y4 <- rnorm(N,4,0.1)  
beta.y4 <-rnorm(N,3,0.1)  
y4 <- rgamma(N, alpha.y4,beta.y4)
y4.df <- data.frame(y4)

any(y4<0)
plot(density(y4))


### laplace(1,1) prior-----------------------

mean.y5 <- rnorm(N,1,0.1)
scale.y5 <- rnorm(N,1,0.1)
y5 <- rlaplace(N,mean.y5,scale.y5 )
y5 <- y5[-which(y5<0)]
y5.df <- data.frame(y5)

any(y5<0)
plot(density(y5))


### mixture model----------------------------

spike<-rbinom(N,1,0.9)

mu2.y6 = rnorm(N,1,0.1)   
sigma2.y6 = rnorm(N,1,0.1)
slab <- rnorm(N, mu2.y6, sqrt(sigma2.y6))

y6 <- rnorm(N, spike*1+(1-spike)*slab, .1)
y6 <- y6[-which(y6<0)]
y6.df = data.frame(y6)

any(y6<0)
plot(density(y6))
hist(y6)








###------------------------Assign value for ggplots-------------------------------#

p1 <- ggplot(y1.df, aes(x=y1)) +   
      geom_histogram(binwidth=.1,aes(y=..density..),color="dark grey",fill="light blue") + 
      geom_density(aes(y=..density..),alpha=.2, fill="light blue")+scale_x_continuous(limits = c(0, 5))+
      geom_vline(xintercept = 1, linetype="dashed", color = "red", size=0.5)

p2 <- ggplot(y2.df, aes(x=y2)) +   
      geom_histogram(binwidth=.1,aes(y=..density..),color="dark grey",fill="light blue") + 
      geom_density(aes(y=..density..),alpha=.2, fill="light blue")+scale_x_continuous(limits = c(0, 5))+
      geom_vline(xintercept = 1, linetype="dashed", color = "red", size=0.5)

p3 <- ggplot(y3.df, aes(x=y3)) +   
      geom_histogram(binwidth=.1,aes(y=..density..),color="dark grey",fill="light blue") + 
      geom_density(aes(y=..density..),alpha=.2, fill="light blue")+scale_x_continuous(limits = c(0, 5))+
      geom_vline(xintercept = 1, linetype="dashed", color = "red", size=0.5)

p4 <- ggplot(y4.df, aes(x=y4)) +   
      geom_histogram(binwidth=.1,aes(y=..density..),color="dark grey",fill="light blue") + 
      geom_density(aes(y=..density..),alpha=.2, fill="light blue")+scale_x_continuous(limits = c(0, 5)) +
      geom_vline(xintercept = 1, linetype="dashed", color = "red", size=0.5)

p5 <- ggplot(y5.df, aes(x=y5)) +   
      geom_histogram(binwidth=.1,aes(y=..density..),color="dark grey",fill="light blue") + 
      geom_density(aes(y=..density..),alpha=.2, fill="light blue")+scale_x_continuous(limits = c(0, 5))+
      geom_vline(xintercept = 1, linetype="dashed", color = "red", size=0.5)

p6 <- ggplot(y6.df, aes(x=y6)) +   
      geom_histogram(binwidth=0.1,aes(y=..density..),color="dark grey",fill="light blue") + 
      geom_density(aes(y=..density..),alpha=.2, fill="light blue")+scale_x_continuous(limits = c(0, 5))+
      geom_vline(xintercept = 1, linetype="dashed", color = "red", size=0.5)

# require("gridExtra")



### NA Values??

which(is.na(y1))
which(is.na(y2))
which(is.na(y3))
which(is.na(y4))
which(is.na(y5))
which(is.na(y6))



grid.arrange(p1 + ggtitle("model 1: Null prior"), 
             p2 + ggtitle("model 2: Normal(1,0.3) prior"), 
             p3 + ggtitle("model 3: Normal(1,0.1) prior"),
             p4 + ggtitle("model 4: Gamma(4,3) prior"),
             p5 + ggtitle("model 5: Laplace(1,1) prior"),
             p6 + ggtitle("model 6: 0.1 Mixture prior"),ncol=2)


png("plots/priordist.png")
grid.arrange(p1 + ggtitle("model 1: Null prior"), 
             p2 + ggtitle("model 2: Normal(1,0.3) prior"), 
             p3 + ggtitle("model 3: Normal(1,0.1) prior"),
             p4 + ggtitle("model 4: Gamma(4,3) prior"),
             p5 + ggtitle("model 5: Laplace(1,1) prior"),
             p6 + ggtitle("model 6: 0.5 Mixture prior"),ncol=2)
dev.off()

pdf("plots/priordist.PDF")
grid.arrange(p1 + ggtitle("model 1: Null prior"), 
             p2 + ggtitle("model 2: Normal(1,0.3) prior"), 
             p3 + ggtitle("model 3: Normal(1,0.1) prior"),
             p4 + ggtitle("model 4: Gamma(4,3) prior"),
             p5 + ggtitle("model 5: Laplace(1,1) prior"),
             p6 + ggtitle("model 6: 0.5 Mixture prior"),ncol=2)
dev.off()

