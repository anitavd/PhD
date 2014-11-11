####################################################################
#
# Testscript for å lese og jobbe med EUR-11 netcdf filer
#
# AVD, MET, jun-2014
#
###################################################################


library(ncdf4)
library(extRemes)

#Year and month vectors
source("~/PhD/Future/R/create_time_ncfiles.R")

nc <- nc_open("~/PhD/Future/Data/Eur-11/SMHI/MPI/AM-loc-EUR11pr3hr_MPI_SMHI_rcp85_1970-2013.nc") #variables: lat, lon, pr, time_bnds
pr <- ncvar_get(nc,"pr")
nc_close(nc)

AMmod <- pr*(60*60*3)

AMobs <- read.table("~/PhD/Future/Data/Obs/AM-Obs_pr3h_1970-2013.txt",header=T)

loc.mod <- c()
scale.mod <- c()
shape.mod <- c()
shape.gmle.mod <- c()
shape.lmom.mod <- c()
rl.mod <- c()
for (i in 1:19) {
  gev <- fevd(AMmod[i,],type="GEV",method="MLE")
  gev.gmle <- fevd(AMmod[i,],type="GEV",method="GMLE")
  gev.lmom <- fevd(AMmod[i,],type="GEV",method="Lmoments")
  rlevels <- return.level(gev,return.period=c(5,10,20,50,100,200))
  loc.mod <- c(loc.mod,gev$results$par[1])
  scale.mod <- c(scale.mod,gev$results$par[2])
  shape.mod <- c(shape.mod,gev$results$par[3]) 
  shape.gmle.mod <- c(shape.gmle.mod,gev.gmle$results$par[3]) 
  shape.lmom.mod <- c(shape.lmom.mod,gev.lmom$results[3]) 
  rl.mod <- cbind(rl.mod,rlevels)
}

loc.obs <- c()
scale.obs <- c()
shape.obs <- c()
shape.gmle.obs <- c()
shape.lmom.obs <- c()
rl.obs <- c()
for (i in 2:20) {
  gev <- fevd(AMobs[,i][!is.na(AMobs[,i])],type="GEV",method="MLE")
  gev.gmle <- fevd(AMobs[,i][!is.na(AMobs[,i])],type="GEV",method="GMLE")
  gev.lmom <- fevd(AMobs[,i][!is.na(AMobs[,i])],type="GEV",method="Lmoments")
  rlevels <- return.level(gev,return.period=c(5,10,20,50,100,200))
  loc.obs <- c(loc.obs,gev$results$par[1])
  scale.obs <- c(scale.obs,gev$results$par[2])
  shape.obs <- c(shape.obs,gev$results$par[3])  
  shape.gmle.obs <- c(shape.gmle.obs,gev.gmle$results$par[3]) 
  shape.lmom.obs <- c(shape.lmom.obs,gev.lmom$results[3]) 
  rl.obs <- cbind(rl.obs,rlevels)
}

j=2
plot(log(c(5,10,20,50,100,200)),rl.mod[,j],ylim=c(10,60))
points(log(c(5,10,20,50,100,200)),rl.obs[,j],col="red")

#Gaussian prior: fevd.avd(z,method="GMLE",priorParams=list(m=0.025,s=0.7))  #VERY large variance, allows for ~min value of -0.3 and ~max value of 0.5 for shape

#Test linear trend in location parameter:
#data <- cbind(Obs=z,Year=seq(1901,2000))
#res <- fevd(z,as.data.frame(data),location.fun=~Year)
#res$results$par[2]
