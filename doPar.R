
library(foreach)
library(doSNOW)

cl <- makeCluster(4, outfile="") # number of cores. Notice 'outfile'
registerDoSNOW(cl)
iterations <- 100
pb <- txtProgressBar(min = 1, max = iterations, style = 3)
result <- foreach(i = 1:iterations, .combine = rbind) %dopar% 
{
  s <- summary(rnorm(1e6))[3]
  setTxtProgressBar(pb, i) 
  return(s)
}
close(pb)
stopCluster(cl) 

library(doParallel)
registerDoParallel(cores=2)
getDoParWorkers()

library(foreach)
library(doParallel)

cl <- makeCluster(4, outfile="") # number of cores. Notice 'outfile'
registerDoSNOW(cl)
iterations <- 100
pb <- txtProgressBar(min = 1, max = iterations, style = 3)
result <- foreach(i = 1:iterations, .combine = rbind) %dopar% 
{
  s <- summary(rnorm(1e6))[3]
  setTxtProgressBar(pb, i) 
  return(s)
}
close(pb)
stopCluster(cl) 


library(doParallel)
cl <- makePSOCKcluster(3, outfile="")
registerDoParallel(cl)
pb <- txtProgressBar(min=1, max=100, style=3)
foreach(i=1:100) %dopar% {
  Sys.sleep(1)
  setTxtProgressBar(pb, i)
  i
}
