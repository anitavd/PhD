rm(list=ls())
library(gevXgpd)
library(ncdf4)
rperiods <- c(5,10,20,50,100,200)

#Read AM
am.3h.obs <- read.table("~/PhD//Future//Data//Obs/Osloarea_AM_pr3h_19712014.txt",header=T)

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

#Read WF
wf.3h.obs <- read.table("~/PhD//Future//Data//Obs/Osloarea_WF_pr3h_19712014.txt",header=T)

nc <- nc_open("~/PhD//Future//Data//Eur-11//DMI//locOslo-WF-EUR11pr3h_ICHEC_DMI_rcp45_1971-2014.nc")
wf.3h.dmi <- ncvar_get(nc,"pr")
nc_close(nc)

nc <- nc_open("~/PhD//Future//Data//Eur-11/IPSL/locOslo-WF-EUR11pr3h_WRF_IPSL_rcp45_1971-2014.nc")
wf.3h.ipsl <- ncvar_get(nc,"pr")
nc_close(nc)

nc <- nc_open("~/PhD//Future//Data//Eur-11/KNMI/locOslo-WF-EUR11pr3h_ICHEC_KNMI_rcp45_1971-2014.nc")
wf.3h.knmi <- ncvar_get(nc,"pr")
nc_close(nc)

nc <- nc_open("~/PhD//Future//Data//Eur-11/SMHI/ICHEC/locOslo-WF-EUR11pr3h_ICHEC_SMHI_rcp45_1971-2014.nc")
wf.3h.smhi.i <- ncvar_get(nc,"pr")
nc_close(nc)

nc <- nc_open("~/PhD//Future//Data//Eur-11/SMHI/CNRM/locOslo-WF-EUR11pr3h_CNRM_SMHI_rcp45_1971-2014.nc")
wf.3h.smhi.c <- ncvar_get(nc,"pr")
nc_close(nc)

#Read WT
wi.3h.obs <- read.table("~/PhD//Future//Data//Obs/Osloarea_WI_pr3h_19712014.txt",header=T)
wt.3h.obs <- wi.3h.obs*wf.3h.obs
wt.3h.obs[,1] <- wi.3h.obs[,1]

nc <- nc_open("~/PhD//Future//Data//Eur-11//DMI//locOslo-WT-EUR11pr3h_ICHEC_DMI_rcp45_1971-2014.nc")
wt.3h.dmi <- ncvar_get(nc,"pr")*60*60
nc_close(nc)

nc <- nc_open("~/PhD//Future//Data//Eur-11/IPSL/locOslo-WT-EUR11pr3h_WRF_IPSL_rcp45_1971-2014.nc")
wt.3h.ipsl <- ncvar_get(nc,"pr")*60*60*3
nc_close(nc)

nc <- nc_open("~/PhD//Future//Data//Eur-11/KNMI/locOslo-WT-EUR11pr3h_ICHEC_KNMI_rcp45_1971-2014.nc")
wt.3h.knmi <- ncvar_get(nc,"pr")*60*60*3
nc_close(nc)

nc <- nc_open("~/PhD//Future//Data//Eur-11/SMHI/ICHEC/locOslo-WT-EUR11pr3h_ICHEC_SMHI_rcp45_1971-2014.nc")
wt.3h.smhi.i <- ncvar_get(nc,"pr")*60*60*3
nc_close(nc)

nc <- nc_open("~/PhD//Future//Data//Eur-11/SMHI/CNRM/locOslo-WT-EUR11pr3h_CNRM_SMHI_rcp45_1971-2014.nc")
wt.3h.smhi.c <- ncvar_get(nc,"pr")*60*60*3
nc_close(nc)

#Compute GEV from combines series
gev.3h.dmi <- fitGEV(as.data.frame(c(am.3h.dmi[1,],am.3h.dmi[,2],am.3h.dmi[3,],am.3h.dmi[4,],am.3h.dmi[5,],am.3h.dmi[6])),estim="pmlik",ret=rperiods)
gev.3h.ipsl <- fitGEV(as.data.frame(c(am.3h.ipsl[1,],am.3h.ipsl[,2],am.3h.ipsl[3,],am.3h.ipsl[4,],am.3h.ipsl[5,],am.3h.ipsl[6])),estim="pmlik",ret=rperiods)
gev.3h.knmi <- fitGEV(as.data.frame(c(am.3h.knmi[1,],am.3h.knmi[,2],am.3h.knmi[3,],am.3h.knmi[4,],am.3h.knmi[5,],am.3h.knmi[6])),estim="pmlik",ret=rperiods)
gev.3h.smhi.i <- fitGEV(as.data.frame(c(am.3h.smhi.i[1,],am.3h.smhi.i[,2],am.3h.smhi.i[3,],am.3h.smhi.i[4,],am.3h.smhi.i[5,],am.3h.smhi.i[6])),estim="pmlik",ret=rperiods)
gev.3h.smhi.c <- fitGEV(as.data.frame(c(am.3h.smhi.c[1,],am.3h.smhi.c[,2],am.3h.smhi.c[3,],am.3h.smhi.c[4,],am.3h.smhi.c[5,],am.3h.smhi.c[6])),estim="pmlik",ret=rperiods)

gev.obs <- fitGEV(as.data.frame(c(am.3h.obs[,2],am.3h.obs[,3],am.3h.obs[,4],am.3h.obs[,5],am.3h.obs[,6],am.3h.obs[,7])),estim="pmlik",ret=rperiods)


#Plot return levels from combines series UPDATE!!!!!
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
#AM, WF, WI, WT, -plots

#Create list of all data
AM <- list(Obs=as.numeric(as.matrix(am.3h.obs[,-1])),DMI=as.vector(am.3h.dmi),IPSL=as.vector(am.3h.ipsl),KNMI=as.vector(am.3h.knmi),SMHI.I=as.vector(am.3h.smhi.i),SMHI.C=as.vector(am.3h.smhi.c))
WF <- list(Obs=as.numeric(as.matrix(wf.3h.obs[,-1])),DMI=as.vector(wf.3h.dmi),IPSL=as.vector(wf.3h.ipsl),KNMI=as.vector(wf.3h.knmi),SMHI.I=as.vector(wf.3h.smhi.i),SMHI.C=as.vector(wf.3h.smhi.c))
WT <- list(Obs=as.numeric(as.matrix(wt.3h.obs[,-1])),DMI=as.vector(wt.3h.dmi),IPSL=as.vector(wt.3h.ipsl),KNMI=as.vector(wt.3h.knmi),SMHI.I=as.vector(wt.3h.smhi.i),SMHI.C=as.vector(wt.3h.smhi.c))
WI <- list(Obs=as.numeric(as.matrix(wi.3h.obs[,-1])),DMI=as.vector(wt.3h.dmi/wf.3h.dmi),IPSL=as.vector(wt.3h.ipsl/wf.3h.ipsl),KNMI=as.vector(wt.3h.knmi/wf.3h.knmi),SMHI.I=as.vector(wt.3h.smhi.i/wf.3h.smhi.i),SMHI.C=as.vector(wt.3h.smhi.c/wf.3h.smhi.c))

col <- c("slategrey","chartreuse3","plum4","chocolate2","deepskyblue3","deepskyblue4")

png("~/PhD//Future//Figs/Osloarea_AM_comparison.png",width=820,height=700)
boxplot(AM,col=col,ylab="Annual maximum 3-hr precipitation [mm]")
dev.off()

png("~/PhD//Future//Figs/Osloarea_WF_comparison.png",width=820,height=700)
boxplot(WF,col=col,ylab="Frequency of 3-hr precipitation events > 0.5 mm, MJJAS")
dev.off()

png("~/PhD//Future//Figs/Osloarea_WT_comparison.png",width=820,height=700)
boxplot(WT,col=col,ylab="Total precipitation for 3-hr events > 0.5 mm, MJJAS")
dev.off()

png("~/PhD//Future//Figs/Osloarea_WI_comparison.png",width=820,height=700)
boxplot(WI,col=col,ylab="Intensity in 3-hr precipitation events > 0.5 mm, MJJAS")
dev.off()

#Observations probably give too low values of WF and WT, because there are many missing values, and periods of down time at stations, which affect WF and WT
#because these depend on several values, while AM only depends on one value and is thus not as vulnerable.
