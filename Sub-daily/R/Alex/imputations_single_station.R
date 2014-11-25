rm(list = ls())

source("./gev.imputations.R") ## Load up the code base
library(parallel)
load("out.RData")
load("cov.RData")
print.figs <- TRUE

coord.new <- cov[,2:1]
if(print.figs){pdf("grid.pdf")}else{X11()}
plot(coord.new,pch=".")
points(mc$coord, col="red")
if(print.figs)dev.off()

N <- dim(coord.new)[1]
##coord.new <- coord.new[sample(1:N,1e4),]
##N <- dim(coord.new)[1] 
##Load up some covariates
x.new.loc <- matrix(colMeans(mc$loc.dsgn.mat),N,5, byrow=TRUE) ## everyone gets the mean
x.new.scale <- matrix(colMeans(mc$scale.dsgn.mat),N,3,byrow=TRUE)
x.new.shape <- matrix(1,N,1)
##------ Finish making data -------------------------------

p.return <- .1 ## Let's look at the 10 year precipitation event
##Z <- impute.value(mc,coord.new, x.new.loc,x.new.scale,x.new.shape,p.return)
S <- dim(mc$chain.loc)[1]
cores.use <- 2

##-------- Here's the new bit, let's look at station 100 -------------
coord.new <- coord.new[100,,drop=FALSE]
x.new.loc <- x.new.loc[100,,drop=FALSE]
x.new.scale <- x.new.scale[100,,drop=FALSE]
x.new.shape <- x.new.shape[100,,drop=FALSE]
##-------- End new bit -----------------------------------------------


l <- mclapply(1:S, "impute.value.single",mc, coord.new, x.new.loc, x.new.scale, x.new.shape, p.return, mc.cores = 2, mc.silent = FALSE)
l <- unlist(l)

