rm(list = ls())

source("./gev.imputations.R") ## Load up the code base
library(parallel)
load("out.RData") ## Here is where you load the output from your run.  I've assumed its called "mc"

##------ This is a helper function for the parallelization ------------
helper <- function(k, mc, coord.new, x.new.loc,x.new.scale,x.new.shape,p)
  {
    a <- impute.value(mc, coord.new[k,], x.new.loc[k,], x.new.scale[k,], x.new.shape[k,],p)
    print(paste("Finished with site", k))
    return(a)
  }
##----------------------------------------------------------------------

##------ Make information to do imputation --------------
## You will want to replace this with the real thing
N <- 1e2 ## How many points on the grid
coord.new <- matrix(0,N,2)##my coordinate matrix

##This is just a stupid grid
k <- 1
for(i in 1:10)
  {
    for(j in 1:10)
      {
        coord.new[k,] <- c( (i - 5)/10, (j - 5)/10)
        k <- k + 1
      }
  }

##Load up some covariates
x.new.loc <- matrix(colMeans(mc$loc.dsgn.mat),N,5, byrow=TRUE) ## everyone gets the mean
x.new.scale <- matrix(colMeans(mc$scale.dsgn.mat),N,3,byrow=TRUE)
x.new.shape <- matrix(1,N,1)
##------ Finish making data -------------------------------

p.return <- .1 ## Let's look at the 10 year precipitation event
cores.use <- 10  ## Here you decide how many cores you want to take up on the server

##Rprof()
##a <- helper(1, mc, coord.new, x.new.loc, x.new.scale, x.new.shape, p.return)
##Rprof(NULL)
##summaryRprof()

l <- mclapply(1:N,"helper", mc, coord.new, x.new.loc, x.new.scale, x.new.shape, p.return,mc.silent = FALSE, mc.cores = cores.use)

Z <- matrix(0, dim(mc$chain.loc)[1],N)
for(i in 1:N)
  {
    Z[,i] <- l[[i]]$Z
  }

save(Z, file = "map.out.RData")


##---- Here's me playing around with a plot.  I'm terrible at plots -----
median.Z <- apply(Z,2,"median")
image((1:10 - 5)/10,(1:10 - 5)/10,matrix(median.Z,10,10), xlim = c(min(mc$coord[,1]), max(mc$coord[,2])), ylim = c(min(mc$coord[,1]), max(mc$coord[,2])))
##-------------------------------------------------------------------------

