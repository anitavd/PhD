rm(list=ls())
library(gevXgpd)
library(ncdf4)

nc<-nc_open("/vol/mis//dmf2//midlertidig//hindcast//EUR-11/KNMI/AM//AM-pr_EUR-11_ICHEC-EC-EARTH_historical_r1i1p1_KNMI-RACMO22E_v1_3hr_1950-2005.nc")
pr <- ncvar_get(nc,"pr")   #dim = 143,143,56
nc_close(nc)
pr.ctrl.knmi <- pr[,,22:51]*(60*60*3) #1971-2000
nc<-nc_open("/vol/mis//dmf2//midlertidig//hindcast//EUR-11/KNMI/AM//AM-pr_EUR-11_ICHEC-EC-EARTH_rcp45_r1i1p1_KNMI-RACMO22E_v1_3hr_2071-2100.nc")
pr.rcp45.knmi <- ncvar_get(nc,"pr")*(60*60*3)   #dim = 143,143,30
nc_close(nc)
nc<-nc_open("/vol/mis//dmf2//midlertidig//hindcast//EUR-11/KNMI/AM//AM-pr_EUR-11_ICHEC-EC-EARTH_rcp85_r1i1p1_KNMI-RACMO22E_v1_3hr_2071-2100.nc")
pr.rcp85.knmi <- ncvar_get(nc,"pr")*(60*60*3)   #dim = 143,143,30
nc_close(nc)

nc<-nc_open("/vol/mis//dmf2//midlertidig//hindcast//EUR-11/SMHI//EUR-11//ICHEC-EC-EARTH//AM//AM-Norway-pr_EUR-11_ICHEC-EC-EARTH_historical_r12i1p1_SMHI-RCA4_v1_3hr_1971-2000.nc")
pr.ctrl.smhi.i <- ncvar_get(nc,"pr")*(60*60*3)   #dim = 143,143,30
lon <- ncvar_get(nc,"lon")
lat <- ncvar_get(nc,"lat")
nc_close(nc)
nc<-nc_open("/vol/mis//dmf2//midlertidig//hindcast//EUR-11/SMHI//EUR-11//ICHEC-EC-EARTH//AM//AM-Norway-pr_EUR-11_ICHEC-EC-EARTH_rcp45_r12i1p1_SMHI-RCA4_v1_3hr_2071-2100.nc")
pr.rcp45.smhi.i <- ncvar_get(nc,"pr")*(60*60*3)   #dim = 143,143,30
nc_close(nc)
nc<-nc_open("/vol/mis//dmf2//midlertidig//hindcast//EUR-11/SMHI//EUR-11//ICHEC-EC-EARTH//AM//AM-Norway-pr_EUR-11_ICHEC-EC-EARTH_rcp85_r12i1p1_SMHI-RCA4_v1_3hr_2071-2100.nc")
pr.rcp85.smhi.i <- ncvar_get(nc,"pr")*(60*60*3)   #dim = 143,143,30
nc_close(nc)

nc<-nc_open("/vol/mis//dmf2//midlertidig//hindcast//EUR-11/SMHI//EUR-11//CNRM-CERFACS-CNRM-CM5//AM//AM-Norway-pr_EUR-11_CNRM-CERFACS-CNRM-CM5_historical_r1i1p1_SMHI-RCA4_v1_3hr_1971-2000.nc")
pr.ctrl.smhi.c <- ncvar_get(nc,"pr")*(60*60*3)   #dim = 143,143,30
nc_close(nc)
nc<-nc_open("/vol/mis//dmf2//midlertidig//hindcast//EUR-11/SMHI//EUR-11//CNRM-CERFACS-CNRM-CM5//AM//AM-Norway-pr_EUR-11_CNRM-CERFACS-CNRM-CM5_rcp45_r1i1p1_SMHI-RCA4_v1_3hr_2071-2100.nc")
pr.rcp45.smhi.c <- ncvar_get(nc,"pr")*(60*60*3)   #dim = 143,143,30
nc_close(nc)
nc<-nc_open("/vol/mis//dmf2//midlertidig//hindcast//EUR-11/SMHI//EUR-11//CNRM-CERFACS-CNRM-CM5//AM//AM-Norway-pr_EUR-11_CNRM-CERFACS-CNRM-CM5_rcp85_r1i1p1_SMHI-RCA4_v1_3hr_2071-2100.nc")
pr.rcp85.smhi.c <- ncvar_get(nc,"pr")*(60*60*3)   #dim = 143,143,30
nc_close(nc)


rperiods <- c(5,10,20,50,100,200)
M5.ctrl.knmi <- matrix(NA,143,143)
M5.rcp45.knmi <- matrix(NA,143,143)
M5.rcp85.knmi <- matrix(NA,143,143)
M5.ctrl.smhi.i <- matrix(NA,143,143)
M5.rcp45.smhi.i <- matrix(NA,143,143)
M5.rcp85.smhi.i <- matrix(NA,143,143)
M5.ctrl.smhi.c <- matrix(NA,143,143)
M5.rcp45.smhi.c <- matrix(NA,143,143)
M5.rcp85.smhi.c <- matrix(NA,143,143)

for (i in 1:143) {
  for (j in 1:143) {
    M5.ctrl.knmi[i,j] <- fitGEV(as.data.frame(pr.ctrl.knmi[i,j,]),estim="pmlik",ret=rperiods)$fit[1,2]
    M5.rcp45.knmi[i,j] <- fitGEV(as.data.frame(pr.rcp45.knmi[i,j,]),estim="pmlik",ret=rperiods)$fit[1,2]
    M5.rcp85.knmi[i,j] <- fitGEV(as.data.frame(pr.rcp85.knmi[i,j,]),estim="pmlik",ret=rperiods)$fit[1,2]
    M5.ctrl.smhi.i[i,j] <- fitGEV(as.data.frame(pr.ctrl.smhi.i[i,j,]),estim="pmlik",ret=rperiods)$fit[1,2]
    M5.rcp45.smhi.i[i,j] <- fitGEV(as.data.frame(pr.rcp45.smhi.i[i,j,]),estim="pmlik",ret=rperiods)$fit[1,2]
    M5.rcp85.smhi.i[i,j] <- fitGEV(as.data.frame(pr.rcp85.smhi.i[i,j,]),estim="pmlik",ret=rperiods)$fit[1,2]
    M5.ctrl.smhi.c[i,j] <- fitGEV(as.data.frame(pr.ctrl.smhi.c[i,j,]),estim="pmlik",ret=rperiods)$fit[1,2]
    M5.rcp45.smhi.c[i,j] <- fitGEV(as.data.frame(pr.rcp45.smhi.c[i,j,]),estim="pmlik",ret=rperiods)$fit[1,2]
    M5.rcp85.smhi.c[i,j] <- fitGEV(as.data.frame(pr.rcp85.smhi.c[i,j,]),estim="pmlik",ret=rperiods)$fit[1,2]
  }
}

ratio.rcp45.knmi <- M5.rcp45.knmi/M5.ctrl.knmi
ratio.rcp85.knmi <- M5.rcp85.knmi/M5.ctrl.knmi
ratio.rcp45.smhi.i <- M5.rcp45.smhi.i/M5.ctrl.smhi.i
ratio.rcp85.smhi.i <- M5.rcp85.smhi.i/M5.ctrl.smhi.i
ratio.rcp45.smhi.c <- M5.rcp45.smhi.c/M5.ctrl.smhi.c
ratio.rcp85.smhi.c <- M5.rcp85.smhi.c/M5.ctrl.smhi.c

change.rcp45.knmi <- M5.rcp45.knmi - M5.ctrl.knmi
change.rcp85.knmi <- M5.rcp85.knmi - M5.ctrl.knmi
change.rcp45.smhi.i <- M5.rcp45.smhi.i - M5.ctrl.smhi.i
change.rcp85.smhi.i <- M5.rcp85.smhi.i - M5.ctrl.smhi.i
change.rcp45.smhi.c <- M5.rcp45.smhi.c - M5.ctrl.smhi.c
change.rcp85.smhi.c <- M5.rcp85.smhi.c - M5.ctrl.smhi.c

rm(M5.ctrl.knmi)
rm(M5.rcp45.knmi)
rm(M5.rcp85.knmi)
rm(M5.ctrl.smhi.i)
rm(M5.rcp45.smhi.i)
rm(M5.rcp85.smhi.i)
rm(M5.ctrl.smhi.c)
rm(M5.rcp45.smhi.c)
rm(M5.rcp85.smhi.c)
gc(reset=T)


#############################################################
# Create netcdf file of results
institute <- "SMHI"    #Change!!!!!!!!!!!!!!!!
model="CNRM"
scen="rcp45"
RP = "M5"


xdim<- ncdim_def("x","degrees",1:143)
ydim  <- ncdim_def("y","degrees",1:143)
#londim<- ncdim_def("lon","degreesE",lon)
#latdim  <- ncdim_def("lat","degreesN",lat)
#prdim <- ncdim_def("pr","mm",change.rcp45.knmi)
prchange<-ncvar_def("prchange","mm",list(xdim,ydim),NULL,longname="Change in M5 for 3-hour precipitation",prec="float")
prratio<-ncvar_def("prratio","mm",list(xdim,ydim),NULL,longname="Ratio of M5 for 3-hour precipitation",prec="float")
rlon<-ncvar_def("lon","degrees",list(xdim,ydim),NULL,longname="Rotated degrees east",prec="double")
rlat<-ncvar_def("lat","degrees",list(xdim,ydim),NULL,longname="Rotated degrees north",prec="double")
ncchange<-nc_create(paste("~/PhD/Future/Results/",institute,"/change.",RP,".",scen,".",institute,".",model,".nc",sep=""),list(prchange,rlon,rlat))
ncratio<-nc_create(paste("~/PhD/Future/Results/",institute,"/ratio.",RP,".",scen,".",institute,".",model,".nc",sep=""),list(prratio,rlon,rlat))
nc <-nc_open("/vol/mis//dmf2//midlertidig//hindcast//EUR-11/KNMI/AM//AM-pr_EUR-11_ICHEC-EC-EARTH_historical_r1i1p1_KNMI-RACMO22E_v1_3hr_1950-2005.nc")
lon <- ncvar_get(nc,"lon")   
lat <- ncvar_get(nc,"lat")
nc_close(nc)
#ncvar_put(ncnew,prchange,change.rcp45.knmi)
ncvar_put(ncratio,prratio,ratio.rcp45.smhi.c)            #Change!!!!!!!!!!!!!!!
ncvar_put(ncchange,prchange,change.rcp45.smhi.c)
ncvar_put(ncchange,rlon,lon)
ncvar_put(ncratio,rlon,lon)
ncvar_put(ncchange,rlat,lat)
ncvar_put(ncratio,rlat,lat)
nc_close(ncratio)
nc_close(ncchange)