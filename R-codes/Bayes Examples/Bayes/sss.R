lambda <- 0.5
tMax <- 100

## find the number 'n' of exponential r.vs required by imposing that
## Pr{N(t) <= n} <= 1 - eps for a small 'eps'
n <- qpois(1 - 1e-8, lambda = lambda * tMax)

## simulate exponential interarrivals the
X <- rexp(n = n, rate = lambda)
S <- c(0, cumsum(X))
plot(x = S, y = 0:n, type = "s", xlim = c(0, tMax)) 

## several paths?
nSamp <- 50
## simulate exponential interarrivals
X <- matrix(rexp(n * nSamp, rate = lambda), ncol = nSamp,
            dimnames = list(paste("S", 1:n, sep = ""), paste("samp", 1:nSamp)))
## compute arrivals, and add a fictive arrival 'T0' for t = 0
S <- apply(X, 2, cumsum)
S <- rbind("T0" = rep(0, nSamp), S)
head(S)
## plot using steps
matplot(x = S, y = 0:n, type = "s", col = "darkgray",
        xlim = c(0, tMax),
        main = "Homogeneous Poisson Process paths", xlab = "t", ylab = "N(t)")