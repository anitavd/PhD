#Create wet event mean and wet event frequency for EUR-11 data (May - September)

rm(list=ls())
library(ncdf4)
institute <- "SMHI"
model <- "MPI"
scen <- "rcp45"
#Year and month vectors
source("~/PhD/Future/R/create_time_ncfiles.R")

if(institute=="SMHI") nc <- nc_open("/vol/mis//dmf2//midlertidig/hindcast//EUR-11/SMHI/EUR-11/CNRM-CERFACS-CNRM-CM5/loc-EUR11pr3h_CNRM_SMHI_rcp45_19702013.nc")
if(institute=="KNMI") nc <- nc_open("/vol//mis//dmf2//midlertidig//hindcast//EUR-11//KNMI/loc-EUR11pr3h_ICHEC_KNMI_rcp45_19662015.nc")
pr <- ncvar_get(nc,"pr")
nc_close(nc)
pr <- pr*(60*60*3)

if(institute=="SMHI") {
  wf<-seq(1970,2013)
  wi<-seq(1970,2013)
  start <- 1970
  end <- 2013
}
if(institute=="KNMI") {
  wf<-seq(1966,2015)
  wi<-seq(1966,2015)
  start <- 1966
  end <- 2015
}
for (i in 1:19) {
  wf.i <- c()
  ws.i <- c()
  for (Year in start:end) {
    wf.i <- c(wf.i,length(which(pr[i,][year==Year & month>4 & month<10] > 1)))
    ws.i <- c(ws.i,sum(pr[i,][year==Year & month>4 & month<10][which(pr[i,][year==Year & month>4 & month<10] > 1)]))
  }
  wf <- cbind(wf,wf.i)
  wi <- cbind(wi,round(ws.i/wf.i,digits=2))
}
