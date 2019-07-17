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