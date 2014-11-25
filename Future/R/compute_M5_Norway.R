######################################################################################
#
# Compute M5 for EUR-11 data
#
#
#
#
######################################################################################
rm(list=ls())
library(gevXgpd)
library(ncdf4)

dur <- "day"

#Read AM for entire Norway-grid
nc <- nc_open(paste("~/eur11//DMI//AM//AM-EUR11pr",dur,"_ICHEC_DMI_rcp45_1971-2014.nc",sep=""))
am.dmi <- ncvar_get(nc,"pr")*60*60
nc_close(nc)

nc <- nc_open(paste("~/eur11/IPSL//AM//AM-EUR11pr",dur,"_WRF_IPSL_rcp45_1971-2014.nc",sep=""))
am.ipsl <- ncvar_get(nc,"pr")*60*60*3
nc_close(nc)

nc <- nc_open(paste("~/eur11//KNMI//AM//AM-EUR11pr",dur,"_ICHEC_KNMI_rcp45_1971-2014.nc",sep=""))
am.knmi <- ncvar_get(nc,"pr")*60*60*3
nc_close(nc)

nc <- nc_open(paste("~/eur11//SMHI//EUR-11//ICHEC-EC-EARTH//AM//AM-EUR11pr",dur,"_ICHEC_SMHI_rcp45_1971-2014.nc",sep=""))
am.smhi.i <- ncvar_get(nc,"pr")*60*60*3
nc_close(nc)

nc <- nc_open(paste("~/eur11/SMHI//EUR-11//CNRM-CERFACS-CNRM-CM5//AM//AM-EUR11pr",dur,"_CNRM_SMHI_rcp45_1971-2014.nc",sep=""))
am.smhi.c <- ncvar_get(nc,"pr")*60*60*3
nc_close(nc)

rperiods <- c(5)
M5.dmi <- matrix(NA,143,143)
M5.ipsl <- matrix(NA,143,143)
M5.knmi <- matrix(NA,143,143)
M5.smhi.i <- matrix(NA,143,143)
M5.smhi.c <- matrix(NA,143,143)

for (i in 1:143) {
  for (j in 1:143) {
    M5.dmi[i,j] <- fitGEV(as.data.frame(am.dmi[i,j,]),estim="pmlik",ret=rperiods)$fit[1,2]
    M5.ipsl[i,j] <- fitGEV(as.data.frame(am.ipsl[i,j,]),estim="pmlik",ret=rperiods)$fit[1,2]
    M5.knmi[i,j] <- fitGEV(as.data.frame(am.knmi[i,j,]),estim="pmlik",ret=rperiods)$fit[1,2]
    M5.smhi.i[i,j] <- fitGEV(as.data.frame(am.smhi.i[i,j,]),estim="pmlik",ret=rperiods)$fit[1,2]
    M5.smhi.c[i,j] <- fitGEV(as.data.frame(am.smhi.c[i,j,]),estim="pmlik",ret=rperiods)$fit[1,2]
  }
}

##################################

filename <- paste("~/PhD//Future//Results/M5-EUR11pr",dur,"_ICHEC_DMI_rcp45_1971-2014.nc",sep="")
data <- M5.dmi

rlondim<- ncdim_def("rlon","degrees",1:143,longname="longitude in rotated pole grid")
rlatdim  <- ncdim_def("rlat","degrees",1:143,longname="latitude in rotated pole grid")
lon<-ncvar_def("lon","degrees_east",list(rlondim,rlatdim),NULL,longname="longitude",prec="double")
lat<-ncvar_def("lat","degrees_north",list(rlondim,rlatdim),NULL,longname="latitude",prec="double")
M5<-ncvar_def("M5","mm",list(rlondim,rlatdim),NULL,longname="Estimated 5-year return level",prec="float")
ncM5<-nc_create(filename,list(lat,lon,M5))
nc <-nc_open("/vol/mis//dmf2//midlertidig//hindcast//EUR-11/DMI/Norway-pr_EUR-11_ICHEC-EC-EARTH_historical_r3i1p1_DMI-HIRHAM5_v1_3hr_1970.nc")
lonold <- ncvar_get(nc,"lon")   
latold <- ncvar_get(nc,"lat")
nc_close(nc)
ncvar_put(ncM5,M5,data)
ncvar_put(ncM5,lon,lonold)
ncvar_put(ncM5,lat,latold)
nc_close(ncM5)

#Comment to maps: DMI has a few grid cells with very large values, which affects the maps.

#####################################################################
#Observations

am.obs <- read.table("~/PhD//Future//Data/Obs//stations10yrs_AM_pr3h_19712014.txt",header=F)
n<-nrow(am.obs)
rperiods <- 5
M5.obs <- c()
for (i in 2:ncol(am.obs)) {
  M5.obs <- rbind(M5.obs,c(am.obs[1,i],fitGEV(as.data.frame(am.obs[2:n,i][which(!is.na(am.obs[2:n,i]))]),estim="pmlik",ret=rperiods)$fit[1,2]))
}

#Coordinates
lon<-c()
lat<-c()
for (i in 1:nrow(M5.obs)) {
  a<-try(select.station(stid=stations[i],cntr="NORWAY"))
  if(is.null(a)) {
    lon<-c(lon,NA)
    lat<-c(lat,NA)
  } else{
    lon <- c(lon,a$longitude[1])
    lat <- c(lat,a$latitude[1])
  }
}

res <- cbind(M5.obs[,1],round(lon,5),round(lat,5),round(M5.obs[,2],2))
colnames(res) <- c("Stnr","lon","lat","M5")
