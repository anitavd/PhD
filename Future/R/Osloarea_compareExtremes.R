rm(list=ls())
library(gevXgpd)
library(ncdf4)
rperiods <- c(5,10,20,50,100,200)

obs <- read.table("~/PhD//Future//Data//Obs/Osloarea_AM_pr3h_19712014.txt",header=T)

#6 stations and 6 grid cells for analysis.
nc <- nc_open("~/PhD//Future//Data//Eur-11//DMI//locOslo-AM-EUR11pr3h_ICHEC_DMI_rcp45_1971-2014.nc")
am.3h.dmi <- ncvar_get(nc,"pr")*60*60
nc_close(nc)

nc <- nc_open("~/PhD//Future//Data//Eur-11/IPSL/locOslo-AM-EUR11pr3h_WRF_IPSL_rcp45_1971-2014.nc")
am.3h.ipsl <- ncvar_get(nc,"pr")*60*60*3
nc_close(nc)

nc <- nc_open("~/PhD//Future//Data//Eur-11/KNMI/locOslo-AM-EUR11pr3h_ICHEC_KNMI_rcp45_1971-2014.nc")
am.3h.knmi <- ncvar_get(nc,"pr")*60*60*3
nc_close(nc)

nc <- nc_open("~/PhD//Future//Data//Eur-11/SMHI/ICHEC/locOslo-AM-EUR11pr3h_ICHEC_SMHI_rcp45_1971-2014.nc")
am.3h.smhi.i <- ncvar_get(nc,"pr")*60*60*3
nc_close(nc)

nc <- nc_open("~/PhD//Future//Data//Eur-11/SMHI/CNRM/locOslo-AM-EUR11pr3h_CNRM_SMHI_rcp45_1971-2014.nc")
am.3h.smhi.c <- ncvar_get(nc,"pr")*60*60*3
nc_close(nc)

#Compute GEV from combines series
gev.3h.dmi <- fitGEV(as.data.frame(c(am.3h.dmi[1,],am.3h.dmi[,2],am.3h.dmi[3,],am.3h.dmi[4,],am.3h.dmi[5,],am.3h.dmi[6])),estim="pmlik",ret=rperiods)
gev.3h.ipsl <- fitGEV(as.data.frame(c(am.3h.ipsl[1,],am.3h.ipsl[,2],am.3h.ipsl[3,],am.3h.ipsl[4,],am.3h.ipsl[5,],am.3h.ipsl[6])),estim="pmlik",ret=rperiods)
gev.3h.knmi <- fitGEV(as.data.frame(c(am.3h.knmi[1,],am.3h.knmi[,2],am.3h.knmi[3,],am.3h.knmi[4,],am.3h.knmi[5,],am.3h.knmi[6])),estim="pmlik",ret=rperiods)
gev.3h.smhi.i <- fitGEV(as.data.frame(c(am.3h.smhi.i[1,],am.3h.smhi.i[,2],am.3h.smhi.i[3,],am.3h.smhi.i[4,],am.3h.smhi.i[5,],am.3h.smhi.i[6])),estim="pmlik",ret=rperiods)
gev.3h.smhi.c <- fitGEV(as.data.frame(c(am.3h.smhi.c[1,],am.3h.smhi.c[,2],am.3h.smhi.c[3,],am.3h.smhi.c[4,],am.3h.smhi.c[5,],am.3h.smhi.c[6])),estim="pmlik",ret=rperiods)

gev.obs <- fitGEV(as.data.frame(c(obs[,2],obs[,3],obs[,4],obs[,5],obs[,6],obs[,7])),estim="pmlik",ret=rperiods)

par(bg="gray90")

plot(log(rperiods),gev.obs$fit[,2],pch=21,bg="blue",ylim=range(0,70),xlab="Return period [year]",ylab="Return level [mm]",axes=F)
axis(1,at=log(rperiods),labels=rperiods)
axis(2)
points(log(rperiods),gev.s.i45$fit[,2],pch=22,bg="red")
#points(log(rperiods),gev.s.i85$fit[,2],pch=19,col="pink")
points(log(rperiods),gev.s.c45$fit[,2],pch=22,bg="orange")
#points(log(rperiods),gev.s.c85$fit[,2],pch=19,col="yellow")
points(log(rperiods),gev.k.45$fit[,2],pch=22,bg="green")
#points(log(rperiods),gev.k.85$fit[,2],pch=19,col="green")
cov.theta<-gev.obs$cov
var.theta <- diag(cov.theta)
p <- c(gev.obs$chi,gev.obs$alpha,-gev.obs$k)
yp <- -log(1 - 1/rperiods)
res <- cbind(1, (-1/p[3]) * (1 - yp^(-p[3])), p[2] * (p[3])^(-2) * (1 - yp^(-p[3])) - (p[2]/p[3]) * yp^(-p[3]) * log(yp))
grads <- t(res)
se.theta <- sqrt(diag(t(grads) %*% cov.theta %*% grads))
rlevels <- gev.obs$fit[,2]
alpha <- 0.05  #confidence level
z.alpha <- qnorm(alpha/2, lower.tail = FALSE)
ci.dn.obs <- rlevels - z.alpha * se.theta
ci.up.obs <- rlevels + z.alpha * se.theta
lines(log(rperiods),ci.dn.obs,lty="dashed")
lines(log(rperiods),ci.up.obs,lty="dashed")

legend(1.5,70,c("Obs","SMHI-ICHEC","SMHI-CNRM","KNMI-ICHEC"),pch=c(21,22,22,22),pt.bg=c("blue","red","orange","green"),bg="white")

#Comments to plot: RCMs generally too low, especially KNMI. Only SMHI CNRM gives values within the CI of the observations. 

############################################################################################################
#AM-plot


#x11(width=16,height=10)


#j=2
#plot(rep(1,6),obs$pr[which(obs$Year==1970)],xlim=range(1,44),pch=21,bg="blue",ylim=range(0,50),xlab="Year",ylab="Annual maximum precipitation [mm]",axes=F)
#for(i in 1971:2013) {
#  points(rep(j,6),obs$pr[which(obs$Year==i)],pch=21,bg="blue")
#  j=j+1
#}
#axis(1,at=seq(1,44),labels=seq(1970,2013))
#axis(2)

#j=1
#for(i in 1970:2013) {
#  points(rep(j,6)+0.2,smhi.i45$pr[which(smhi.i45$Year==i)],pch=22,bg="red")
#  points(rep(j,6)+0.2,smhi.c45$pr[which(smhi.c45$Year==i)],pch=22,bg="orange")
#  points(rep(j,6)+0.2,knmi.45$pr[which(knmi.45$Year==i)],pch=22,bg="green")   #does not reach the extremest AMs in obs, SMHI does more or less.
#  j=j+1
#}

#Create list of all data
AM <- list(Obs=as.numeric(as.matrix(obs[,-1])),DMI=as.vector(am.3h.dmi),IPSL=as.vector(am.3h.ipsl),KNMI=as.vector(am.3h.knmi),SMHI.I=as.vector(am.3h.smhi.i),SMHI.C=as.vector(am.3h.smhi.c))

col <- c("slategrey","chartreuse3","plum4","chocolate2","deepskyblue3","deepskyblue4")

png("~/PhD//Future//Figs/Osloarea_AM_comparison.png",width=820,height=700)
boxplot(AM,col=col)

#legend(3.5,60,c("Obs","DMI","IPSL","KNMI","SMHI-I","SMHI-C"),pch=c(22,22,22,22,22,22),pt.bg=col,bg="gray90")

dev.off()

#Comments to plot: RCMs does well on the moderate extremes, but can't reach the highest values. KNMI gives lower values. DMI and IPSL do a good job!

##############################################################################################################
#Create AM series from the max AM among the stations/grid cells.

rm(list=ls())
library(gevXgpd)
rperiods <- c(5,10,20,50,100,200)


nc <- nc_open("~/PhD/Future//Data//Eur-11//SMHI//ICHEC/AM-Osloloc-EUR11pr3h_ICHEC_SMHI_rcp45_1970-2013.nc")
pr <- ncvar_get(nc,"pr")
nc_close(nc)
smhi.i45 <- as.data.frame(rbind(cbind(seq(1970,2013),pr[1,]),cbind(seq(1970,2013),pr[2,]),cbind(seq(1970,2013),pr[5,]),cbind(seq(1970,2013),pr[6,]),cbind(seq(1970,2013),pr[9,]),cbind(seq(1970,2013),pr[10,])))
colnames(smhi.i45) <- c("Year","pr")
smhi.i45$pr <- smhi.i45$pr*(60*60*3) 
nc <- nc_open("~/PhD/Future//Data//Eur-11//SMHI//CNRM/AM-Osloloc-EUR11pr3h_CNRM_SMHI_rcp45_1970-2013.nc")
pr <- ncvar_get(nc,"pr")
nc_close(nc)
smhi.c45 <- as.data.frame(rbind(cbind(seq(1970,2013),pr[1,]),cbind(seq(1970,2013),pr[2,]),cbind(seq(1970,2013),pr[5,]),cbind(seq(1970,2013),pr[6,]),cbind(seq(1970,2013),pr[9,]),cbind(seq(1970,2013),pr[10,])))
colnames(smhi.c45) <- c("Year","pr")
smhi.c45$pr <- smhi.c45$pr*(60*60*3) 
nc <- nc_open("~/PhD/Future//Data//Eur-11/KNMI//AM-Osloloc-EUR11pr3h_ICHEC_KNMI_rcp45_19502015.nc")
pr <- ncvar_get(nc,"pr")
nc_close(nc)
knmi.45 <- as.data.frame(rbind(cbind(seq(1970,2013),pr[1,21:64]),cbind(seq(1970,2013),pr[2,21:64]),cbind(seq(1970,2013),pr[5,21:64]),cbind(seq(1970,2013),pr[6,21:64]),cbind(seq(1970,2013),pr[9,21:64]),cbind(seq(1970,2013),pr[10,21:64])))
colnames(knmi.45) <- c("Year","pr")
knmi.45$pr <- knmi.45$pr*(60*60*3) 

obs.max <- c()
smhi.i45.max <- c()
smhi.c45.max <- c()
knmi.45.max <- c()
for (year in 1970:2013) {
  obs.max <- c(obs.max,max(obs$pr[which(obs$Year==year)],na.rm=T))
  smhi.i45.max <- c(smhi.i45.max,max(smhi.i45$pr[which(smhi.i45$Year==year)]))
  smhi.c45.max <- c(smhi.c45.max,max(smhi.c45$pr[which(smhi.c45$Year==year)]))
  knmi.45.max <- c(knmi.45.max,max(knmi.45$pr[which(knmi.45$Year==year)]))
}

gev.obs.max <- fitGEV(as.data.frame(obs.max[which(!is.na(obs.max))]),estim="pmlik",ret=rperiods)
gev.s.i45.max <- fitGEV(as.data.frame(smhi.i45.max),estim="pmlik",ret=rperiods)
gev.s.c45.max <- fitGEV(as.data.frame(smhi.c45.max),estim="pmlik",ret=rperiods)
gev.k.45.max <- fitGEV(as.data.frame(knmi.45.max),estim="pmlik",ret=rperiods)

plot(log(rperiods),gev.obs.max$fit[,2],pch=19,ylim=range(10,90))
points(log(rperiods),gev.s.i45.max$fit[,2],pch=19,col="red")
points(log(rperiods),gev.s.c45.max$fit[,2],pch=19,col="orange")
points(log(rperiods),gev.k.45.max$fit[,2],pch=19,col="blue")

cov.theta<-gev.obs.max$cov
var.theta <- diag(cov.theta)
p <- c(gev.obs.max$chi,gev.obs.max$alpha,-gev.obs.max$k)
yp <- -log(1 - 1/rperiods)
res <- cbind(1, (-1/p[3]) * (1 - yp^(-p[3])), p[2] * (p[3])^(-2) * (1 - yp^(-p[3])) - (p[2]/p[3]) * yp^(-p[3]) * log(yp))
grads <- t(res)
se.theta <- sqrt(diag(t(grads) %*% cov.theta %*% grads))
rlevels <- gev.obs.max$fit[,2]
alpha <- 0.05  #confidence level
z.alpha <- qnorm(alpha/2, lower.tail = FALSE)
ci.dn.obs <- rlevels - z.alpha * se.theta
ci.up.obs <- rlevels + z.alpha * se.theta
lines(log(rperiods),ci.dn.obs,lty="dashed")
lines(log(rperiods),ci.up.obs,lty="dashed")

#Comments to plot: Wider CI due to fewer values --> SMHI is within the CI, KNMI not. All too low.
