if(!require(foreach)) install.packages("foreach")
if(!require(doParallel)) install.packages("doParallel")
# require(doSMP) # will no longer work...
library(foreach)
library(doParallel)
library(plyr)

workers <- makeCluster(4) # My computer has 2 cores
registerDoParallel(workers)

x <- seq_len(20)
wait <- function(i) Sys.sleep(0.3)
system.time(llply(x, wait)) # 6 sec
system.time(llply(x, wait, .parallel = TRUE))

library(plyr)

1.81*4

## When you are done:
stopCluster(workers)



# https://topepo.github.io/caret/parallel-processing.html
# https://stats.stackexchange.com/questions/273750/parallel-processing-bayesian-model-with-r

# how to run multiple chains on multiple processor and sum result? 