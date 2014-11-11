#Compare extreme 3-hr precipitation from observations and EUR-11 data

####################################################################
#
# Testscript for å lese og jobbe med EUR-11 netcdf filer
#
# AVD, MET, jun-2014
#
###################################################################


library(ncdf4)
library(extRemes)
library(gevXgpd)

institute <- "SMHI"
model <- "CNRM"
scen <- "rcp45"
trend <- FALSE

#Year and month vectors
source("~/PhD/Future/R/create_time_ncfiles.R")

#Modified fevd script, Gaussian prior on shape parameter
source("~/PhD/Future/R/fevd.avd.R")

if(institute=="SMHI") nc <- nc_open(paste("~/PhD/Future/Data/Eur-11/",institute,"/",model,"/AM-loc-EUR11pr3h_",model,"_",institute,"_",scen,"_1970-2013.nc",sep="")) #variables: lat, lon, pr, time_bnds
if(institute=="KNMI") nc <- nc_open(paste("~/PhD/Future/Data/Eur-11/",institute,"/AM-loc-EUR11pr3h_ICHEC_KNMI_",scen,"_1966-2015.nc",sep=""))
pr <- ncvar_get(nc,"pr")
nc_close(nc)

AMmod <- pr*(60*60*3)

AMobs <- read.table("~/PhD/Future/Data/Obs/AM-Obs_pr3h_1970-2013.txt",header=T)

rperiods <- c(5,10,20,50,100,200)

loc.obs <- c()
scale.obs <- c()
shape.obs <- c()
rl.obs <- c()
ci.dn.obs <- c()
ci.up.obs <- c()
trend.obs <- c()
loc.mod <- c()
scale.mod <- c()
shape.mod <- c()
rl.mod <- c()
ci.dn.mod <- c()
ci.up.mod <- c()
trend.mod <- c()

for (i in 1:19) {

  #Use Frei's package to estimate shape according to Martins&Stedinger
  gev.obs <- fitGEV(as.data.frame(AMobs[,i+1][!is.na(AMobs[,i+1])]),estim="pmlik",ret=rperiods)
  loc.obs <- c(loc.obs,gev.obs$chi)
  scale.obs <- c(scale.obs,gev.obs$alpha)
  shape.obs <- c(shape.obs,-gev.obs$k)
  rlevels <- gev.obs$fit[,2]
  rl.obs <- cbind(rl.obs,rlevels)
  #Compute confidence interval for return levels
  alpha <- 0.05  #confidence level
  z.alpha <- qnorm(alpha/2, lower.tail = FALSE)
  cov.theta <- gev.obs$cov
  var.theta <- diag(cov.theta)
  yp <- -log(1 - 1/rperiods)
  p <- c(gev.obs$chi,gev.obs$alpha,-gev.obs$k)
  res <- cbind(1, (-1/p[3]) * (1 - yp^(-p[3])), p[2] * (p[3])^(-2) * (1 - yp^(-p[3])) - (p[2]/p[3]) * yp^(-p[3]) * log(yp))
  grads <- t(res)
  se.theta <- sqrt(diag(t(grads) %*% cov.theta %*% grads))
  ci.dn.obs <- cbind(ci.dn.obs,rlevels - z.alpha * se.theta)
  ci.up.obs <- cbind(ci.up.obs,rlevels + z.alpha * se.theta)
  
  if(institute=="SMHI") pr.mod <- AMmod[i,][!is.na(AMobs[,i+1])]
  if(institute=="KNMI") pr.mod <- AMmod[i,5:48][!is.na(AMobs[,i+1])]

  #Use Frei's package to estimate shape according to Martins&Stedinger
  gev.mod <- fitGEV(as.data.frame(pr.mod),estim="pmlik",ret=rperiods)
  loc.mod <- c(loc.mod,gev.mod$chi)
  scale.mod <- c(scale.mod,gev.mod$alpha)
  shape.mod <- c(shape.mod,-gev.mod$k)
  rlevels <- gev.mod$fit[,2]
  rl.mod <- cbind(rl.mod,rlevels)
  #Compute confidence interval for return levels
  cov.theta <- gev.mod$cov
  var.theta <- diag(cov.theta)
  p <- c(gev.mod$chi,gev.mod$alpha,-gev.mod$k)
  res <- cbind(1, (-1/p[3]) * (1 - yp^(-p[3])), p[2] * (p[3])^(-2) * (1 - yp^(-p[3])) - (p[2]/p[3]) * yp^(-p[3]) * log(yp))
  grads <- t(res)
  se.theta <- sqrt(diag(t(grads) %*% cov.theta %*% grads))
  ci.dn.mod <- cbind(ci.dn.mod,rlevels - z.alpha * se.theta)
  ci.up.mod <- cbind(ci.up.mod,rlevels + z.alpha * se.theta)
  
  #Trend
  if(trend) {
    years <- AMobs$Year[!is.na(AMobs[,i+1])]
    data <- as.data.frame(cbind(AM=AMobs[,i+1][!is.na(AMobs[,i+1])],Year=years))
    res <- fevd(data$AM,as.data.frame(data),location.fun=~Year)
    trend.obs <- c(trend.obs,res$results$par[2])
    data <- as.data.frame(cbind(AM=pr.mod,Year=years))
    res <- fevd(data$AM,as.data.frame(data),location.fun=~Year)
    trend.mod <- c(trend.mod,res$results$par[2])
  }
}

#Plot return levels (Also plot in map!)
ymax <- max(rl.mod,rl.obs)
for(j in 1:19) {
  if(institute=="SMHI") png(paste("~/PhD/Future/Figs/",institute,"/",model,"/",scen,"/compareRL_site",j,"_",scen,".png",sep=""))
  if(institute=="KNMI") png(paste("~/PhD/Future/Figs/",institute,"/",scen,"/compareRL_site",j,"_",scen,".png",sep=""))
  plot(log(rperiods),rl.obs[,j],ylim=c(0,ymax),pch=21,bg="blue",axes=F,ylab="Precipitation [mm]",xlab="Return period [years]")
  points(log(rperiods),rl.mod[,j],bg="red",pch=22)
  lines(log(rperiods),ci.dn.obs[,j],lty="dashed")
  lines(log(rperiods),ci.up.obs[,j],lty="dashed")
  #lines(log(rperiods),ci.dn.mod[,j],lty="dashed",col="red")
  #lines(log(rperiods),ci.up.mod[,j],lty="dashed",col="red")
  points(log(rperiods),rl.obs[,j],pch=21,bg="blue")
  points(log(rperiods),rl.mod[,j],bg="red",pch=22)
  axis(1,at=log(rperiods),labels=rperiods)
  axis(2)

  dev.off()
}

#Plot location parameter (with linear slope) 
min.y <- min(min(loc.obs),min(loc.mod))
max.y <- max(max(loc.obs),max(loc.mod))
if(institute=="SMHI") png(paste("~/PhD/Future/Figs/",institute,"/",model,"/",scen,"/compareLoc_all.png",sep=""),width=580,height=480,units="px")
if(institute=="KNMI") png(paste("~/PhD/Future/Figs/",institute,"/",scen,"/compareLoc_all.png",sep=""),width=580,height=480,units="px")
plot(loc.obs,pch=21,bg="blue",ylim=c(min.y,max.y),axes=F,ylab="Location parameter",xlab="Site")
points(loc.mod,pch=22,bg="red")
if(trend) {
  for(j in 1:length(loc.mod)) {
    lines(c(j-0.3,j+0.3),c(loc.obs[j]+((trend.obs[j]/0.6)/2),loc.obs[j]-((trend.obs[j]/0.6)/2)),lwd=3)
    lines(c(j-0.3,j+0.3),c(loc.mod[j]+((trend.mod[j]/0.6)/2),loc.mod[j]-((trend.mod[j]/0.6)/2)),col="red",lwd=3)
    if(j<length(loc.mod)) abline(v=j+0.5,lty="dashed")
  }
} else {
  for(j in 1:length(loc.mod)) {
    if(j<length(loc.mod)) abline(v=j+0.5,lty="dashed")
  }
}
axis(2)
mtext(seq(1,19),side=1,line=1,at=seq(1,19))

dev.off()


#Plot scale parameter
min.y <- min(min(scale.obs),min(scale.mod))
max.y <- max(max(scale.obs),max(scale.mod))
if(institute=="SMHI") png(paste("~/PhD/Future/Figs/",institute,"/",model,"/",scen,"/compareScale_all.png",sep=""),width=580,height=480,units="px")
if(institute=="KNMI") png(paste("~/PhD/Future/Figs/",institute,"/",scen,"/compareScale_all.png",sep=""),width=580,height=480,units="px")
plot(scale.obs,pch=21,bg="blue",ylim=c(min.y,max.y),axes=F,ylab="Scale parameter",xlab="Site")
points(scale.mod,pch=22,bg="red")
if(trend) {
  for(j in 1:length(loc.mod)) {
    lines(c(j-0.3,j+0.3),c(scale.obs[j]+((trend.obs[j]/0.6)/2),loc.obs[j]-((trend.obs[j]/0.6)/2)),lwd=3)
    lines(c(j-0.3,j+0.3),c(scale.mod[j]+((trend.mod[j]/0.6)/2),loc.mod[j]-((trend.mod[j]/0.6)/2)),col="red",lwd=3)
    if(j<length(loc.mod)) abline(v=j+0.5,lty="dashed")
  }
} else {
  for(j in 1:length(scale.mod)) {
    if(j<length(scale.mod)) abline(v=j+0.5,lty="dashed")
  }
}
axis(2)
mtext(seq(1,19),side=1,line=1,at=seq(1,19))

dev.off()

#Scale with wet event statistics
#source("compareWetEvents.R")
#plot(100*loc.obs/colMeans(wt.obs[,2:20],na.rm=T),pch=19,ylim=c(3,7))
#points(100*loc.mod/colMeans(wt.mod[,2:20],na.rm=T),pch=19,col="red")


