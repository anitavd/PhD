rm(list=ls())

library(SpatialExtremes)
library(extRemes)
library(ismev)
source("alex.gev.code.R")

#Run model, Markov chain. 
load("files.RData")
mc <- latent(data=data.am[,-1], coord.norm, marg.cov = marg.cov, loc.form = loc.form, scale.form = scale.form,shape.form = shape.form, hyper = hyper, prop = prop, start = start, n = 100, burn.in= 30, thin = 1)  
source("alex.gev.code.R")
mc.fix.shape <- latent.fix.shape(data=data.am[,-1], coord.norm, marg.cov = marg.cov, loc.form = loc.form, scale.form = scale.form,shape.form = shape.form, hyper = hyper, prop = prop, start = start, n = 100, burn.in= 30, thin = 1, shape.fix = .15) ##notice that bit on the end


plot(colMeans(mc$chain.shape[,-(1:4)]))
points(colMeans(mc.fix.shape$chain.shape[,-(1:4)]),col="red")
legend("topright", legend = c("Original", "New"), pch = 1, col=c(1,"red"))




