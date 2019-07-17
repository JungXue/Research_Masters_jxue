## check Rtools is included in system PATH.
#If Rtools is installed to c:\\Rtools then you should see something like this:
#[1] "c:\\\\Rtools\\\\bin;...

Sys.getenv("PATH")

##Check that g++ can be called from R.

system('g++ -v')
system('where make')

##Configuration

cat('Sys.setenv(BINPREF = "C:/Rtools/mingw_$(WIN)/bin/")',
    file = file.path(Sys.getenv("HOME"), ".Rprofile"), 
    sep = "\n", append = TRUE)

##Install R stan
install.packages("rstan", repos = "https://cloud.r-project.org/", dependencies=TRUE)

##testing

fx <- inline::cxxfunction( signature(x = "integer", y = "numeric" ) , '
	return ScalarReal( INTEGER(x)[0] * REAL(y)[0] ) ;
' )
fx( 2L, 5 ) # should be 10

################################################################################################################

#   https://github.com/stan-dev/rstan/wiki/RStan-Getting-Started#how-to-use-rstan

## Loading the package

library("rstan") 

## These options respectively allow you to automatically save a bare version of a compiled Stan program 
#to the hard disk so that it does not need to be recompiled and to execute multiple Markov chains in parallel.

rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())


###########################################3
# Example 1: Eight Schools

schools_dat <- list(J = 8, 
                    y = c(28,  8, -3,  7, -1,  1, 18, 12),
                    sigma = c(15, 10, 16, 11,  9, 11, 10, 18))

fit <- stan(file = '8schools.stan', data = schools_dat, 
            iter = 1000, chains = 4)

print(fit)
plot(fit)
pairs(fit, pars = c("mu", "tau", "lp__"))

la <- extract(fit, permuted = TRUE) # return a list of arrays 
mu <- la$mu 

### return an array of three dimensions: iterations, chains, parameters 
a <- extract(fit, permuted = FALSE) 

### use S3 functions on stanfit objects
a2 <- as.array(fit)
m <- as.matrix(fit)
d <- as.data.frame(fit)
print(fit, digits = 1)

################################3

##Rats example

y <- as.matrix(read.table('https://raw.github.com/wiki/stan-dev/rstan/rats.txt', header = TRUE))
x <- c(8, 15, 22, 29, 36)
xbar <- mean(x)
N <- nrow(y)
T <- ncol(y)
#################################

#Demo

model <- stan_demo()

###########################333

##Language meanuel 

http://mc-stan.org/users/documentation/
  
  

