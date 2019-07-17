#
# https://stackoverflow.com/questions/46918215/specifying-a-hierarchical-model-in-jags-for-r
#


#generate covariate data at plot and site scales.
x1 <- runif(100,0,1)  #100 plot level observations of x1
x2 <- runif(10,10,20) #10  site level observations of x2

#generate site values - in this case characters A:J
site_1 <- LETTERS[sort(rep(seq(1,10, by = 1),10))]
site_2 <- LETTERS[sort(seq(1,10, by = 1))]

#put together site level data - 10 observations for 10 sites.
site_data <- data.frame(site_2,x2)
colnames(site_data) <- c('site','x2')
head(site_data)
#put together plot level data - 100 observations across 10 sites
plot_data <- data.frame(site_1,x1)
head(plot_data)
colnames(plot_data) <- c('site','x1')
plot_data <- merge(plot_data,site_data, all.x=T) #merge in site level data.
head(plot_data)

#pick parameter values.
b1 <- 10
b2 <- 0.2

#y is a function of the plot level covariate x1 and the site level covariate x2.
plot_data$y <- b1*plot_data$x1 + b2*plot_data$x2 + rnorm(100)
head(plot_data)
#check that the model fits. it does.
summary(lm(y ~ x1 + x2, data = plot_data))

jags.model = "
model{
# priors
b1 ~ dnorm(0, .001)
b2 ~ dnorm(0, .001)
tau <- pow(sigma, -2)
sigma ~ dunif(0, 100)

# Response:
for (i in 1:N){
y[i] ~ dnorm(y.hat[i], tau)
y.hat[i] <- b1*x1[i] + site_effect[plot_site[i]]
}

# Effect of site:
for (s in 1:S){
site_effect[s] <- b2 * x2_site[site_site[s]]
}

}
"
# Ensure the site is coded as a factor with the same levels in both data frames:
plot_data$site <- factor(plot_data$site)
site_data$site <- factor(site_data$site, levels=levels(plot_data$site))
head(plot_data)
head(site_data)
#setup jags data as a list
jd <- list(        y=plot_data$y, 
                  x1=plot_data$x1, 
           plot_site=plot_data$site, 
           site_site=site_data$site, 
             x2_site=site_data$x2, 
                   N=length(plot_data$y), 
                   S=nrow(site_data))
jd
#install.packages("runjags", dependencies = FALSE)
library(runjags)
#run jags model
jags.out <- run.jags(jags.model,
                     data=jd,
                     adapt = 1000,
                     burnin = 1000,
                     sample = 2000,
                     n.chains=3,
                     monitor=c('b1', 'b2'))
summary(jags.out)
head(jags.out)
