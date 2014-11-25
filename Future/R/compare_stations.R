#######################################################################
#
# Compare obs and eur-11 at 66 stations
#
#
#
#########################################################################

#Colors: obs, DMI, IPSL, KNMI, SMHI.I, SMHI.C
col <- c("slategrey","chartreuse3","plum4","chocolate2","deepskyblue3","deepskyblue4")

am.obs <- read.table("~/PhD//Future//Data//Obs//stations10yrs_AM_pr3h_19712014.txt",header=T)
wi.obs <- read.table("~/PhD//Future//Data//Obs//stations10yrs_WI_pr3h_19712014.txt",header=T)
wf.obs <- read.table("~/PhD//Future//Data//Obs//stations10yrs_WF_pr3h_19712014.txt",header=T)
wt.obs <- as.data.frame(matrix(NA,45,67))
wt.obs[1,] <- wf.obs[1,]
wt.obs[,1] <- wf.obs[,1]
wt.obs[2:45,2:67] <- wf.obs[2:45,2:67] * wi.obs[2:45,2:67]

nc <- nc_open("~/eur11/DMI//AM//stations-AM-EUR11pr3h_ICHEC_DMI_rcp45_1971-2014.nc")           
am.dmi <- ncvar_get(nc,"pr")*(60*60)
nc_close(nc)
nc <- nc_open("~/eur11/DMI/WT//stations-WT-EUR11pr3h_ICHEC_DMI_rcp45_1971-2014.nc")           
wt.dmi <- ncvar_get(nc,"pr")*(60*60)
nc_close(nc)
nc <- nc_open("~/eur11/DMI/WF/stations-WF-EUR11pr3h_ICHEC_DMI_rcp45_1971-2014.nc")           
wf.dmi <- ncvar_get(nc,"pr")
nc_close(nc)
wi.dmi <- wt.dmi / wf.dmi

nc <- nc_open("~/eur11/IPSL/AM//stations-AM-EUR11pr3h_WRF_IPSL_rcp45_1971-2014.nc")           
am.ipsl <- ncvar_get(nc,"pr")*(60*60*3)
nc_close(nc)
nc <- nc_open("~/eur11/IPSL/WT//stations-WT-EUR11pr3h_WRF_IPSL_rcp45_1971-2014.nc")           
wt.ipsl <- ncvar_get(nc,"pr")*(60*60*3)
nc_close(nc)
nc <- nc_open("~/eur11/IPSL/WF/stations-WF-EUR11pr3h_WRF_IPSL_rcp45_1971-2014.nc")           
wf.ipsl <- ncvar_get(nc,"pr")
nc_close(nc)
wi.ipsl <- wt.ipsl / wf.ipsl

nc <- nc_open("~/eur11/KNMI//AM//stations-AM-EUR11pr3h_ICHEC_KNMI_rcp45_1971-2014.nc")           
am.knmi <- ncvar_get(nc,"pr")*(60*60*3)
nc_close(nc)
nc <- nc_open("~/eur11/KNMI/WT/stations-WT-EUR11pr3h_ICHEC_KNMI_rcp45_1971-2014.nc")           
wt.knmi <- ncvar_get(nc,"pr")*(60*60*3)
nc_close(nc)
nc <- nc_open("~/eur11/KNMI/WF/stations-WF-EUR11pr3h_ICHEC_KNMI_rcp45_1971-2014.nc")           
wf.knmi <- ncvar_get(nc,"pr")
nc_close(nc)
wi.knmi <- wt.knmi / wf.knmi

nc <- nc_open("~/eur11/SMHI//EUR-11//ICHEC-EC-EARTH//AM//stations-AM-EUR11pr3h_ICHEC_SMHI_rcp45_1971-2014.nc")           
am.smhi.i <- ncvar_get(nc,"pr")*(60*60*3)
nc_close(nc)
nc <- nc_open("~/eur11/SMHI//EUR-11//ICHEC-EC-EARTH/WT/stations-WT-EUR11pr3h_ICHEC_SMHI_rcp45_1971-2014.nc")           
wt.smhi.i <- ncvar_get(nc,"pr")*(60*60*3)
nc_close(nc)
nc <- nc_open("~/eur11/SMHI//EUR-11//ICHEC-EC-EARTH/WF/stations-WF-EUR11pr3h_ICHEC_SMHI_rcp45_1971-2014.nc")           
wf.smhi.i <- ncvar_get(nc,"pr")
nc_close(nc)
wi.smhi.i <- wt.smhi.i / wf.smhi.i

nc <- nc_open("~/eur11/SMHI//EUR-11//CNRM-CERFACS-CNRM-CM5//AM//stations-AM-EUR11pr3h_CNRM_SMHI_rcp45_1971-2014.nc")           
am.smhi.c <- ncvar_get(nc,"pr")*(60*60*3)
nc_close(nc)
nc <- nc_open("~/eur11/SMHI//EUR-11//CNRM-CERFACS-CNRM-CM5/WT//stations-WT-EUR11pr3h_CNRM_SMHI_rcp45_1971-2014.nc")           
wt.smhi.c <- ncvar_get(nc,"pr")*(60*60*3)
nc_close(nc)
nc <- nc_open("~/eur11/SMHI//EUR-11//CNRM-CERFACS-CNRM-CM5/WF/stations-WF-EUR11pr3h_CNRM_SMHI_rcp45_1971-2014.nc")           
wf.smhi.c <- ncvar_get(nc,"pr")
nc_close(nc)
wi.smhi.c <- wt.smhi.c / wf.smhi.c

#Scores
#AM
R.dmi <- c()
R.ipsl <- c()
R.knmi <- c()
R.smhi.i <- c()
R.smhi.c <-c()
RMSE.dmi <- c()
RMSE.knmi <- c()
RMSE.ipsl <- c()
RMSE.smhi.i <- c()
RMSE.smhi.c <- c()
MAE.dmi <- c()
MAE.ipsl <- c()
MAE.knmi <- c()
MAE.smhi.i <- c()
MAE.smhi.c <- c()

for (i in 1:66) {
  R.dmi<-c(R.dmi,cor(am.dmi[i,],am.obs[2:45,(i+1)],use="pairwise.complete.obs"))
  R.ipsl<-c(R.ipsl,cor(am.ipsl[i,],am.obs[2:45,(i+1)],use="pairwise.complete.obs"))
  R.knmi<-c(R.knmi,cor(am.knmi[i,],am.obs[2:45,(i+1)],use="pairwise.complete.obs"))
  R.smhi.i<-c(R.smhi.i,cor(am.smhi.i[i,],am.obs[2:45,(i+1)],use="pairwise.complete.obs"))
  R.smhi.c<-c(R.smhi.c,cor(am.smhi.c[i,],am.obs[2:45,(i+1)],use="pairwise.complete.obs"))
  
  RMSE.dmi<-c(RMSE.dmi,sqrt(mean((am.dmi[i,]-am.obs[2:45,(i+1)])^2,na.rm=T)))
  RMSE.ipsl<-c(RMSE.ipsl,sqrt(mean((am.ipsl[i,]-am.obs[2:45,(i+1)])^2,na.rm=T)))
  RMSE.knmi<-c(RMSE.knmi,sqrt(mean((am.knmi[i,]-am.obs[2:45,(i+1)])^2,na.rm=T)))
  RMSE.smhi.i<-c(RMSE.smhi.i,sqrt(mean((am.smhi.i[i,]-am.obs[2:45,(i+1)])^2,na.rm=T)))
  RMSE.smhi.c<-c(RMSE.smhi.c,sqrt(mean((am.smhi.c[i,]-am.obs[2:45,(i+1)])^2,na.rm=T)))
  
  MAE.dmi<-c(MAE.dmi,mean(abs(am.dmi[i,]-am.obs[2:45,(i+1)]),na.rm=T))
  MAE.ipsl<-c(MAE.ipsl,mean(abs(am.ipsl[i,]-am.obs[2:45,(i+1)]),na.rm=T))
  MAE.knmi<-c(MAE.knmi,mean(abs(am.knmi[i,]-am.obs[2:45,(i+1)]),na.rm=T))
  MAE.smhi.i<-c(MAE.smhi.i,mean(abs(am.smhi.i[i,]-am.obs[2:45,(i+1)]),na.rm=T))
  MAE.smhi.c<-c(MAE.smhi.c,mean(abs(am.smhi.c[i,]-am.obs[2:45,(i+1)]),na.rm=T))
}

plot(R.dmi,col=col[2],pch=19,ylim=c(-0.8,0.8))
points(R.ipsl,col=col[3],pch=19)
points(R.knmi,col=col[4],pch=19)
points(R.smhi.i,col=col[5],pch=19)
points(R.smhi.c,col=col[6],pch=19)
#abline(0,0,lty="dashed")
abline(mean(R.dmi),0,col=col[2],lwd=2)
abline(mean(R.ipsl),0,col=col[3],lwd=2)
abline(mean(R.knmi),0,col=col[4],lwd=2)
abline(mean(R.smhi.i),0,col=col[5],lwd=2)
abline(mean(R.smhi.c),0,col=col[6],lwd=2)

plot(RMSE.dmi,col=col[2],pch=19,ylim=c(0,25))
points(RMSE.ipsl,col=col[3],pch=19)
points(RMSE.knmi,col=col[4],pch=19)
points(RMSE.smhi.i,col=col[5],pch=19)
points(RMSE.smhi.c,col=col[6],pch=19)
#abline(0,0,lty="dashed")
abline(mean(RMSE.dmi),0,col=col[2],lwd=2)
abline(mean(RMSE.ipsl),0,col=col[3],lwd=2)
abline(mean(RMSE.knmi),0,col=col[4],lwd=2)
abline(mean(RMSE.smhi.i),0,col=col[5],lwd=2)
abline(mean(RMSE.smhi.c),0,col=col[6],lwd=2)

plot(MAE.dmi,col=col[2],pch=19)
points(MAE.ipsl,col=col[3],pch=19)
points(MAE.knmi,col=col[4],pch=19)
points(MAE.smhi.i,col=col[5],pch=19)
points(MAE.smhi.c,col=col[6],pch=19)
#abline(0,0,lty="dashed")
abline(mean(MAE.dmi),0,col=col[2],lwd=2)
abline(mean(MAE.ipsl),0,col=col[3],lwd=2)
abline(mean(MAE.knmi),0,col=col[4],lwd=2)
abline(mean(MAE.smhi.i),0,col=col[5],lwd=2)
abline(mean(MAE.smhi.c),0,col=col[6],lwd=2)

#R uinteressant?

#############################################################################
#WF

R.dmi <- c()
R.ipsl <- c()
R.knmi <- c()
R.smhi.i <- c()
R.smhi.c <-c()
RMSE.dmi <- c()
RMSE.knmi <- c()
RMSE.ipsl <- c()
RMSE.smhi.i <- c()
RMSE.smhi.c <- c()
MAE.dmi <- c()
MAE.ipsl <- c()
MAE.knmi <- c()
MAE.smhi.i <- c()
MAE.smhi.c <- c()

for (i in 1:66) {
  R.dmi<-c(R.dmi,cor(wf.dmi[i,],wf.obs[2:45,(i+1)],use="pairwise.complete.obs"))
  R.ipsl<-c(R.ipsl,cor(wf.ipsl[i,],wf.obs[2:45,(i+1)],use="pairwise.complete.obs"))
  R.knmi<-c(R.knmi,cor(wf.knmi[i,],wf.obs[2:45,(i+1)],use="pairwise.complete.obs"))
  R.smhi.i<-c(R.smhi.i,cor(wf.smhi.i[i,],wf.obs[2:45,(i+1)],use="pairwise.complete.obs"))
  R.smhi.c<-c(R.smhi.c,cor(wf.smhi.c[i,],wf.obs[2:45,(i+1)],use="pairwise.complete.obs"))
  
  RMSE.dmi<-c(RMSE.dmi,sqrt(mean((wf.dmi[i,]-wf.obs[2:45,(i+1)])^2,na.rm=T)))
  RMSE.ipsl<-c(RMSE.ipsl,sqrt(mean((wf.ipsl[i,]-wf.obs[2:45,(i+1)])^2,na.rm=T)))
  RMSE.knmi<-c(RMSE.knmi,sqrt(mean((wf.knmi[i,]-wf.obs[2:45,(i+1)])^2,na.rm=T)))
  RMSE.smhi.i<-c(RMSE.smhi.i,sqrt(mean((wf.smhi.i[i,]-wf.obs[2:45,(i+1)])^2,na.rm=T)))
  RMSE.smhi.c<-c(RMSE.smhi.c,sqrt(mean((wf.smhi.c[i,]-wf.obs[2:45,(i+1)])^2,na.rm=T)))
  
  MAE.dmi<-c(MAE.dmi,mean(abs(wf.dmi[i,]-wf.obs[2:45,(i+1)]),na.rm=T))
  MAE.ipsl<-c(MAE.ipsl,mean(abs(wf.ipsl[i,]-wf.obs[2:45,(i+1)]),na.rm=T))
  MAE.knmi<-c(MAE.knmi,mean(abs(wf.knmi[i,]-wf.obs[2:45,(i+1)]),na.rm=T))
  MAE.smhi.i<-c(MAE.smhi.i,mean(abs(wf.smhi.i[i,]-wf.obs[2:45,(i+1)]),na.rm=T))
  MAE.smhi.c<-c(MAE.smhi.c,mean(abs(wf.smhi.c[i,]-wf.obs[2:45,(i+1)]),na.rm=T))
}

plot(RMSE.dmi,col=col[2],pch=19)
points(RMSE.ipsl,col=col[3],pch=19)
points(RMSE.knmi,col=col[4],pch=19)
points(RMSE.smhi.i,col=col[5],pch=19)
points(RMSE.smhi.c,col=col[6],pch=19)
#abline(0,0,lty="dashed")
abline(mean(RMSE.dmi),0,col=col[2],lwd=2)
abline(mean(RMSE.ipsl),0,col=col[3],lwd=2)
abline(mean(RMSE.knmi),0,col=col[4],lwd=2)
abline(mean(RMSE.smhi.i),0,col=col[5],lwd=2)
abline(mean(RMSE.smhi.c),0,col=col[6],lwd=2)

plot(MAE.dmi,col=col[2],pch=19)
points(MAE.ipsl,col=col[3],pch=19)
points(MAE.knmi,col=col[4],pch=19)
points(MAE.smhi.i,col=col[5],pch=19)
points(MAE.smhi.c,col=col[6],pch=19)
#abline(0,0,lty="dashed")
abline(mean(MAE.dmi),0,col=col[2],lwd=2)
abline(mean(MAE.ipsl),0,col=col[3],lwd=2)
abline(mean(MAE.knmi),0,col=col[4],lwd=2)
abline(mean(MAE.smhi.i),0,col=col[5],lwd=2)
abline(mean(MAE.smhi.c),0,col=col[6],lwd=2)

#############################################################################
#WI

R.dmi <- c()
R.ipsl <- c()
R.knmi <- c()
R.smhi.i <- c()
R.smhi.c <-c()
RMSE.dmi <- c()
RMSE.knmi <- c()
RMSE.ipsl <- c()
RMSE.smhi.i <- c()
RMSE.smhi.c <- c()
MAE.dmi <- c()
MAE.ipsl <- c()
MAE.knmi <- c()
MAE.smhi.i <- c()
MAE.smhi.c <- c()

for (i in 1:66) {
  R.dmi<-c(R.dmi,cor(wi.dmi[i,],wi.obs[2:45,(i+1)],use="pairwise.complete.obs"))
  R.ipsl<-c(R.ipsl,cor(wi.ipsl[i,],wi.obs[2:45,(i+1)],use="pairwise.complete.obs"))
  R.knmi<-c(R.knmi,cor(wi.knmi[i,],wi.obs[2:45,(i+1)],use="pairwise.complete.obs"))
  R.smhi.i<-c(R.smhi.i,cor(wi.smhi.i[i,],wi.obs[2:45,(i+1)],use="pairwise.complete.obs"))
  R.smhi.c<-c(R.smhi.c,cor(wi.smhi.c[i,],wi.obs[2:45,(i+1)],use="pairwise.complete.obs"))
  
  RMSE.dmi<-c(RMSE.dmi,sqrt(mean((wi.dmi[i,]-wi.obs[2:45,(i+1)])^2,na.rm=T)))
  RMSE.ipsl<-c(RMSE.ipsl,sqrt(mean((wi.ipsl[i,]-wi.obs[2:45,(i+1)])^2,na.rm=T)))
  RMSE.knmi<-c(RMSE.knmi,sqrt(mean((wi.knmi[i,]-wi.obs[2:45,(i+1)])^2,na.rm=T)))
  RMSE.smhi.i<-c(RMSE.smhi.i,sqrt(mean((wi.smhi.i[i,]-wi.obs[2:45,(i+1)])^2,na.rm=T)))
  RMSE.smhi.c<-c(RMSE.smhi.c,sqrt(mean((wi.smhi.c[i,]-wi.obs[2:45,(i+1)])^2,na.rm=T)))
  
  MAE.dmi<-c(MAE.dmi,mean(abs(wi.dmi[i,]-wi.obs[2:45,(i+1)]),na.rm=T))
  MAE.ipsl<-c(MAE.ipsl,mean(abs(wi.ipsl[i,]-wi.obs[2:45,(i+1)]),na.rm=T))
  MAE.knmi<-c(MAE.knmi,mean(abs(wi.knmi[i,]-wi.obs[2:45,(i+1)]),na.rm=T))
  MAE.smhi.i<-c(MAE.smhi.i,mean(abs(wi.smhi.i[i,]-wi.obs[2:45,(i+1)]),na.rm=T))
  MAE.smhi.c<-c(MAE.smhi.c,mean(abs(wi.smhi.c[i,]-wi.obs[2:45,(i+1)]),na.rm=T))
}

plot(RMSE.dmi,col=col[2],pch=19)
points(RMSE.ipsl,col=col[3],pch=19)
points(RMSE.knmi,col=col[4],pch=19)
points(RMSE.smhi.i,col=col[5],pch=19)
points(RMSE.smhi.c,col=col[6],pch=19)
#abline(0,0,lty="dashed")
abline(mean(RMSE.dmi),0,col=col[2],lwd=2)
abline(mean(RMSE.ipsl),0,col=col[3],lwd=2)
abline(mean(RMSE.knmi),0,col=col[4],lwd=2)
abline(mean(RMSE.smhi.i),0,col=col[5],lwd=2)
abline(mean(RMSE.smhi.c),0,col=col[6],lwd=2)

plot(MAE.dmi,col=col[2],pch=19)
points(MAE.ipsl,col=col[3],pch=19)
points(MAE.knmi,col=col[4],pch=19)
points(MAE.smhi.i,col=col[5],pch=19)
points(MAE.smhi.c,col=col[6],pch=19)
#abline(0,0,lty="dashed")
abline(mean(MAE.dmi),0,col=col[2],lwd=2)
abline(mean(MAE.ipsl),0,col=col[3],lwd=2)
abline(mean(MAE.knmi),0,col=col[4],lwd=2)
abline(mean(MAE.smhi.i),0,col=col[5],lwd=2)
abline(mean(MAE.smhi.c),0,col=col[6],lwd=2)

#############################################################################
#WT

R.dmi <- c()
R.ipsl <- c()
R.knmi <- c()
R.smhi.i <- c()
R.smhi.c <-c()
RMSE.dmi <- c()
RMSE.knmi <- c()
RMSE.ipsl <- c()
RMSE.smhi.i <- c()
RMSE.smhi.c <- c()
MAE.dmi <- c()
MAE.ipsl <- c()
MAE.knmi <- c()
MAE.smhi.i <- c()
MAE.smhi.c <- c()

for (i in 1:66) {
  R.dmi<-c(R.dmi,cor(wt.dmi[i,],wt.obs[2:45,(i+1)],use="pairwise.complete.obs"))
  R.ipsl<-c(R.ipsl,cor(wt.ipsl[i,],wt.obs[2:45,(i+1)],use="pairwise.complete.obs"))
  R.knmi<-c(R.knmi,cor(wt.knmi[i,],wt.obs[2:45,(i+1)],use="pairwise.complete.obs"))
  R.smhi.i<-c(R.smhi.i,cor(wt.smhi.i[i,],wt.obs[2:45,(i+1)],use="pairwise.complete.obs"))
  R.smhi.c<-c(R.smhi.c,cor(wt.smhi.c[i,],wt.obs[2:45,(i+1)],use="pairwise.complete.obs"))
  
  RMSE.dmi<-c(RMSE.dmi,sqrt(mean((wt.dmi[i,]-wt.obs[2:45,(i+1)])^2,na.rm=T)))
  RMSE.ipsl<-c(RMSE.ipsl,sqrt(mean((wt.ipsl[i,]-wt.obs[2:45,(i+1)])^2,na.rm=T)))
  RMSE.knmi<-c(RMSE.knmi,sqrt(mean((wt.knmi[i,]-wt.obs[2:45,(i+1)])^2,na.rm=T)))
  RMSE.smhi.i<-c(RMSE.smhi.i,sqrt(mean((wt.smhi.i[i,]-wt.obs[2:45,(i+1)])^2,na.rm=T)))
  RMSE.smhi.c<-c(RMSE.smhi.c,sqrt(mean((wt.smhi.c[i,]-wt.obs[2:45,(i+1)])^2,na.rm=T)))
  
  MAE.dmi<-c(MAE.dmi,mean(abs(wt.dmi[i,]-wt.obs[2:45,(i+1)]),na.rm=T))
  MAE.ipsl<-c(MAE.ipsl,mean(abs(wt.ipsl[i,]-wt.obs[2:45,(i+1)]),na.rm=T))
  MAE.knmi<-c(MAE.knmi,mean(abs(wt.knmi[i,]-wt.obs[2:45,(i+1)]),na.rm=T))
  MAE.smhi.i<-c(MAE.smhi.i,mean(abs(wt.smhi.i[i,]-wt.obs[2:45,(i+1)]),na.rm=T))
  MAE.smhi.c<-c(MAE.smhi.c,mean(abs(wt.smhi.c[i,]-wt.obs[2:45,(i+1)]),na.rm=T))
}

plot(RMSE.dmi,col=col[2],pch=19)
points(RMSE.ipsl,col=col[3],pch=19)
points(RMSE.knmi,col=col[4],pch=19)
points(RMSE.smhi.i,col=col[5],pch=19)
points(RMSE.smhi.c,col=col[6],pch=19)
#abline(0,0,lty="dashed")
abline(mean(RMSE.dmi),0,col=col[2],lwd=2)
abline(mean(RMSE.ipsl),0,col=col[3],lwd=2)
abline(mean(RMSE.knmi),0,col=col[4],lwd=2)
abline(mean(RMSE.smhi.i),0,col=col[5],lwd=2)
abline(mean(RMSE.smhi.c),0,col=col[6],lwd=2)

plot(MAE.dmi,col=col[2],pch=19)
points(MAE.ipsl,col=col[3],pch=19)
points(MAE.knmi,col=col[4],pch=19)
points(MAE.smhi.i,col=col[5],pch=19)
points(MAE.smhi.c,col=col[6],pch=19)
#abline(0,0,lty="dashed")
abline(mean(MAE.dmi),0,col=col[2],lwd=2)
abline(mean(MAE.ipsl),0,col=col[3],lwd=2)
abline(mean(MAE.knmi),0,col=col[4],lwd=2)
abline(mean(MAE.smhi.i),0,col=col[5],lwd=2)
abline(mean(MAE.smhi.c),0,col=col[6],lwd=2)