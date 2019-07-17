############---------------drafts--------------------------------------###################



# https://www.stat.auckland.ac.nz/~paul/RGraphics/chapter4.pdf
# https://www.statmethods.net/RiA/lattice.pdf
# http://www.sumsar.net/papers/baath_2015_modeling_match_resluts_in_soccer.pdf?fbclid=IwAR0Q4W-VloPFqGMkWLkkS3UYETpNBkWhXtWFNLOQ8ePxE7ekD3FONIlc-9E


MCMCtrace(model6.sim, param = "all", ind = TRUE, ISB = FALSE,pdf = FALSE)
MCMCtrace(model2.sim, param = "all", ind = TRUE, ISB = FALSE,pdf = FALSE)
#what is prior matrix? ( priors = Y ? DOESNT WORK)
# gvals = Y  ?

MCMCtrace(model3.sim, param = "total", ind = TRUE, ISB = FALSE,pdf = FALSE)
MCMCtrace(model4.sim, param = "total", ind = TRUE, ISB = FALSE,pdf = FALSE)
MCMCtrace(model5.sim, param = "total", ind = TRUE, ISB = FALSE,pdf = FALSE)


MCMCplot(model1.sim)

?MCMCtrace()

xyplot(model1.sim, main="model 1", strip=F, strip.left=strip.custom(factor.levels=MyText,bg="skyblue"))
xyplot(model2.sim, main="model 2", strip=F, strip.left=strip.custom(factor.levels=MyText,bg="skyblue"))
xyplot(model3.sim, main="model 3", strip=F, strip.left=strip.custom(factor.levels=MyText,bg="skyblue"))
xyplot(model4.sim, main="model 4", strip=F, strip.left=strip.custom(factor.levels=MyText,bg="skyblue"))
xyplot(model5.sim, main="model 5", strip=F, strip.left=strip.custom(factor.levels=MyText,bg="skyblue"))

densityplot(model1.sim, main="model 1", strip=F, strip.left=strip.custom(factor.levels=MyText,bg="skyblue"))
densityplot(model2.sim, main="model 2", strip=F, strip.left=strip.custom(factor.levels=MyText,bg="skyblue"))
densityplot(model3.sim, main="model 3", strip=F, strip.left=strip.custom(factor.levels=MyText,bg="skyblue"))
densityplot(model4.sim, main="model 4", strip=F, strip.left=strip.custom(factor.levels=MyText,bg="skyblue"))
densityplot(model5.sim, main="model 5", strip=F, strip.left=strip.custom(factor.levels=MyText,bg="skyblue"))

acfplot(model1.sim, main="model 1", strip=F, strip.left=strip.custom(factor.levels=MyText,bg="skyblue"))  #,aspect = 'fill' 
acfplot(model2.sim, main="model 2", strip=F, strip.left=strip.custom(factor.levels=MyText,bg="skyblue"))
acfplot(model3.sim, main="model 3", strip=F, strip.left=strip.custom(factor.levels=MyText,bg="skyblue"))
acfplot(model4.sim, main="model 4", strip=F, strip.left=strip.custom(factor.levels=MyText,bg="skyblue"))
acfplot(model5.sim, main="model 5", strip=F, strip.left=strip.custom(factor.levels=MyText,bg="skyblue"))

HPDinterval(model1.sim)
HPDinterval(model2.sim)
HPDinterval(model3.sim)
HPDinterval(model4.sim)
HPDinterval(model5.sim)



caterplot(model1.sim, main="model 1", labels.loc = "above",labels = MyText)
caterplot(model2.sim, main="model 1", labels.loc = "above",labels = MyText)
caterplot(model3.sim, main="model 1", labels.loc = "above",labels = MyText)
caterplot(model4.sim, main="model 1", labels.loc = "above",labels = MyText)
caterplot(model5.sim, main="model 1", labels.loc = "above",labels = MyText)
caterplot(model6.sim, main="model 1", labels.loc = "above",labels = MyText)


?caterplot
plotPost(model1.sim[,1],       main="Posterior Est.", xlab="sd" , showMode=T )
plotPost(model1.sim[,"total"], main="Posterior Est.", xlab="sd" , showMode=T )
plotPost(model2.sim, main="Posterior Est.", xlab="sd" , showMode=T )
plotPost(model3.sim, main="Posterior Est.", xlab="sd" , showMode=T )
plotPost(model4.sim, main="Posterior Est.", xlab="sd" , showMode=T )
plotPost(model5.sim, main="Posterior Est.", xlab="sd" , showMode=T )
plotPost(model6.sim, main="Posterior Est.", xlab="sd" , showMode=T )

hist(model1.sim[,1])


#-------------------------------Plots---------------------------------

# 


############################################################

### density of posterior distributions
y1 <- modelall.sim[[1]][,1]
y2 <- modelall.sim[[1]][,2]
y3 <- modelall.sim[[1]][,3]
y4 <- modelall.sim[[1]][,4]
y5 <- modelall.sim[[1]][,5]
y6 <- modelall.sim[[1]][,6]
y1.df <- data.frame(y1)
y2.df <- data.frame(y2)
y3.df <- data.frame(y3)
y4.df <- data.frame(y4)
y5.df <- data.frame(y5)
y6.df <- data.frame(y6)


p1 <- ggplot(y1.df, aes(x=y1)) +   
  geom_histogram(binwidth=1,aes(y=..density..),color="dark grey",fill="light blue") + 
  geom_density(aes(y=..density..),alpha=.2, fill="light blue")+scale_x_continuous(limits = c(5, 40))+
  geom_vline(xintercept = rho[1], linetype="dashed", color = "red", size=0.5)
geom_vline(xintercept = Y[1], linetype="dashed", color = "red", size=0.5)

p2 <- ggplot(y2.df, aes(x=y2)) +   
  geom_histogram(binwidth=0.02,aes(y=..density..),color="dark grey",fill="light blue") + 
  geom_density(aes(y=..density..),alpha=.2, fill="light blue")+scale_x_continuous(limits = c(20.85, 21.25))+
  geom_vline(xintercept = rho[1], linetype="dashed", color = "red", size=0.5)

p3 <- ggplot(y3.df, aes(x=y3)) +   
  geom_histogram(binwidth=0.02,aes(y=..density..),color="dark grey",fill="light blue") + 
  geom_density(aes(y=..density..),alpha=.2, fill="light blue")+scale_x_continuous(limits = c(20.85, 21.25))+
  geom_vline(xintercept = rho[1], linetype="dashed", color = "red", size=0.5)

p4 <- ggplot(y4.df, aes(x=y4)) +   
  geom_histogram(binwidth=0.02,aes(y=..density..),color="dark grey",fill="light blue") + 
  geom_density(aes(y=..density..),alpha=.2, fill="light blue")+scale_x_continuous(limits = c(20.85, 21.25)) +
  geom_vline(xintercept = rho[1], linetype="dashed", color = "red", size=0.5)

p5 <- ggplot(y5.df, aes(x=y5)) +   
  geom_histogram(binwidth=0.02,aes(y=..density..),color="dark grey",fill="light blue") + 
  geom_density(aes(y=..density..),alpha=.2, fill="light blue")+scale_x_continuous(limits = c(20.85, 21.25))+
  geom_vline(xintercept = rho[1], linetype="dashed", color = "red", size=0.5)

p6 <- ggplot(y6.df, aes(x=y6)) +   
  geom_histogram(binwidth=0.02,aes(y=..density..),color="dark grey",fill="light blue") + 
  geom_density(aes(y=..density..),alpha=.2, fill="light blue")+scale_x_continuous(limits = c(20.85, 21.25))+
  geom_vline(xintercept = rho[1], linetype="dashed", color = "red", size=0.5)

grid.arrange(p1 + ggtitle("model 1: Null posterior"), 
             p2 + ggtitle("model 2: Normal(1,0.3) posterior"), 
             p3 + ggtitle("model 3: Normal(1,0.1) posterior"),
             p4 + ggtitle("model 4: Gamma(4,3) posterior"),
             p5 + ggtitle("model 5: Laplace(1,1) posterior"),
             p6 + ggtitle("model 6: 0.1 Mixture posterior"),ncol=2)


pdf("plots/findmodel/Densityall.PDF")
densityplot(modelall.sim, main="Posterior density for Total of all models", 
            strip=F, strip.left=strip.custom(factor.levels=modelnames ,bg="skyblue"))
dev.off()


### https://www.r-bloggers.com/check-your-prior-posterior-overlap-ppo-mcmc-wrangling-in-r-made-easy-with-mcmcvis/

spike<-rbinom(N,1,0.9)
mu2.y6 = rnorm(N,1,0.1)   
sigma2.y6 = rnorm(N,1,0.1)
slab <- rnorm(N, mu2.y6, sqrt(sigma2.y6))

y6 <- rnorm(N, spike*1+(1-spike)*slab, .1)
y6 <- y6[-which(y6<0)]
y6a <- sample(y6,size=nrow(model6.sim[[1]]),replace = T)
y6b <- sample(y6,size=nrow(model6.sim[[2]]),replace = T)
y6c <- sample(y6,size=nrow(model6.sim[[3]]),replace = T)
PR=list()
PR[[1]] <- outer(y6a,as.numeric(as.vector(rho.df[1,3:9])),FUN = "*")
PR[[2]] <- outer(y6b,as.numeric(as.vector(rho.df[1,3:9])),FUN = "*")
PR[[3]] <- outer(y6c,as.numeric(as.vector(rho.df[1,3:9])),FUN = "*")
MCMCtrace(model6.sim, params = 'total', priors = PR, pdf = FALSE)


### error in caterplot intervals

#---------------------------Latex Tables all models------------------------------






### 2. samplers

model1 <-c(" ")
model2 <-c("lambda","mu2.y2","sigma2.y2")
model3 <-c("lambda","mu2.y3","sigma2.y3")
model4 <-c("lambda","alpha.y4","beta.y4")
model5 <-c("lambda","mean.y5","scale.y5")
model6 <-c("mu2.y6","sigma2.y6")

sampler = rbind(model1,model2,model3,model4, model5, model6)

save(sampler,file="rdata/sim1/samplertable.RData")
load("rdata/sim1/samplertable.RData")

# xtable(sampler,digits = 6, type = "latex", file = "plots/samplertable.tex")






#---------------------------Latex Tables------------------------------




###

# https://cran.r-project.org/web/packages/bayesplot/vignettes/plotting-mcmc-draws.html

# https://rstudio-pubs-static.s3.amazonaws.com/90226_cdaaf19bc8b64a19ae54b6d885579c83.html
# http://doingbayesiandataanalysis.blogspot.com/2012/08/gamma-likelihood-parameterized-by-mode.html


# https://cran.r-project.org/web/packages/MCMCvis/vignettes/MCMCvis.html
# mcmc hist https://cran.r-project.org/web/packages/bayesplot/vignettes/plotting-mcmc-draws.html
# https://mc-stan.org/bayesplot/reference/MCMC-overview.html
## bridge sampling 

# understand errors
# https://www4.stat.ncsu.edu/~reich/BSMdata/errors.html