##################################################################
#
# Plots GEV parameters on a 1x1km grid, based on regression output
# from the Bayesian Hierarchical model (testModel.R) and explanatory variables (covariates) 
#
# 
# Anita Verpe Dyrrdal, met.no, Nov.2012
#
###################################################################

rm(list=ls())

library(ncdf)
source("/vol/klimadata/applikasjon/gridding/src/iobin.R")
source("/vol/klimadata/applikasjon/gridding/src/met_stat.R")

#Read file with indexes for non-NA values
#filename <- "/home/anitavd/PhD/Sub-daily/RR3/snowmask.bil"
#con=file(filename,open="rb")
#snowmask=readBin(con,integer(),size=1,n=1550*1195)
#close(con)
#TabID <- which(snowmask==1)
TabID <- as.numeric(as.matrix(read.table("/home/anitavd/scripts/TabID.txt",header=TRUE)))

#Read result from model
con <- file("/home/anitavd/PhD/Sub-daily/mc2.txt")   #mc2, 4-covariate model with n=300.000
lm <- readLines(con,n=20)
close(con)

#Estimated regression parameters for location
loc.lm1 <- as.numeric(strsplit(lm[11]," ")[[1]][3])
if(is.na(loc.lm1)) loc.lm1 <- as.numeric(strsplit(lm[11]," ")[[1]][4])

loc.lm2 <- as.numeric(strsplit(lm[11]," ")[[1]][5])
if(is.na(loc.lm2)) loc.lm2 <- as.numeric(strsplit(lm[11]," ")[[1]][6])
if(is.na(loc.lm2)) loc.lm2 <- as.numeric(strsplit(lm[11]," ")[[1]][7])

loc.lm3 <- as.numeric(strsplit(lm[11]," ")[[1]][8])
if(is.na(loc.lm3)) loc.lm3 <- as.numeric(strsplit(lm[11]," ")[[1]][9])
if(is.na(loc.lm3)) loc.lm3 <- as.numeric(strsplit(lm[11]," ")[[1]][10])
if(is.na(loc.lm3)) loc.lm3 <- as.numeric(strsplit(lm[11]," ")[[1]][7])

loc.lm4 <- as.numeric(strsplit(lm[11]," ")[[1]][11])
if(is.na(loc.lm4)) loc.lm4 <- as.numeric(strsplit(lm[11]," ")[[1]][12])
if(is.na(loc.lm4)) loc.lm4 <- as.numeric(strsplit(lm[11]," ")[[1]][13])
if(is.na(loc.lm4)) loc.lm4 <- as.numeric(strsplit(lm[11]," ")[[1]][10])
if(is.na(loc.lm4)) loc.lm4 <- as.numeric(strsplit(lm[11]," ")[[1]][9])

loc.lm5 <- as.numeric(strsplit(lm[11]," ")[[1]][14])
if(is.na(loc.lm5)) loc.lm5 <- as.numeric(strsplit(lm[11]," ")[[1]][15])
if(is.na(loc.lm5)) loc.lm5 <- as.numeric(strsplit(lm[11]," ")[[1]][16])
if(is.na(loc.lm5)) loc.lm5 <- as.numeric(strsplit(lm[11]," ")[[1]][13])
if(is.na(loc.lm5)) loc.lm5 <- as.numeric(strsplit(lm[11]," ")[[1]][12])
if(is.na(loc.lm5)) loc.lm5 <- as.numeric(strsplit(lm[11]," ")[[1]][11])

##############################

loc.lm1.dn <- as.numeric(strsplit(lm[10]," ")[[1]][3])
if(is.na(loc.lm1.dn)) loc.lm1.dn <- as.numeric(strsplit(lm[11]," ")[[1]][4])

loc.lm2.dn <- as.numeric(strsplit(lm[10]," ")[[1]][5])
if(is.na(loc.lm2.dn)) loc.lm2.dn <- as.numeric(strsplit(lm[11]," ")[[1]][6])
if(is.na(loc.lm2.dn)) loc.lm2.dn <- as.numeric(strsplit(lm[11]," ")[[1]][7])

loc.lm3.dn <- as.numeric(strsplit(lm[10]," ")[[1]][8])
if(is.na(loc.lm3.dn)) loc.lm3.dn <- as.numeric(strsplit(lm[11]," ")[[1]][9])
if(is.na(loc.lm3.dn)) loc.lm3.dn <- as.numeric(strsplit(lm[11]," ")[[1]][10])
if(is.na(loc.lm3.dn)) loc.lm3.dn <- as.numeric(strsplit(lm[11]," ")[[1]][7])

loc.lm4.dn <- as.numeric(strsplit(lm[10]," ")[[1]][11])
if(is.na(loc.lm4.dn)) loc.lm4.dn <- as.numeric(strsplit(lm[11]," ")[[1]][12])
if(is.na(loc.lm4.dn)) loc.lm4.dn <- as.numeric(strsplit(lm[11]," ")[[1]][13])
if(is.na(loc.lm4.dn)) loc.lm4.dn <- as.numeric(strsplit(lm[11]," ")[[1]][10])
if(is.na(loc.lm4.dn)) loc.lm4.dn <- as.numeric(strsplit(lm[11]," ")[[1]][9])

loc.lm5.dn <- as.numeric(strsplit(lm[10]," ")[[1]][14])
if(is.na(loc.lm5.dn)) loc.lm5.dn <- as.numeric(strsplit(lm[11]," ")[[1]][15])
if(is.na(loc.lm5.dn)) loc.lm5.dn <- as.numeric(strsplit(lm[11]," ")[[1]][16])
if(is.na(loc.lm5.dn)) loc.lm5.dn <- as.numeric(strsplit(lm[11]," ")[[1]][13])
if(is.na(loc.lm5.dn)) loc.lm5.dn <- as.numeric(strsplit(lm[11]," ")[[1]][12])
if(is.na(loc.lm5.dn)) loc.lm5.dn <- as.numeric(strsplit(lm[11]," ")[[1]][11])

############################################

loc.lm1.up <- as.numeric(strsplit(lm[12]," ")[[1]][3])
if(is.na(loc.lm1.up)) loc.lm1.up <- as.numeric(strsplit(lm[11]," ")[[1]][4])

loc.lm2.up <- as.numeric(strsplit(lm[12]," ")[[1]][5])
if(is.na(loc.lm2.up)) loc.lm2.up <- as.numeric(strsplit(lm[11]," ")[[1]][6])
if(is.na(loc.lm2.up)) loc.lm2.up <- as.numeric(strsplit(lm[11]," ")[[1]][7])

loc.lm3.up <- as.numeric(strsplit(lm[12]," ")[[1]][8])
if(is.na(loc.lm3.up)) loc.lm3.up <- as.numeric(strsplit(lm[11]," ")[[1]][9])
if(is.na(loc.lm3.up)) loc.lm3.up <- as.numeric(strsplit(lm[11]," ")[[1]][10])
if(is.na(loc.lm3.up)) loc.lm3.up <- as.numeric(strsplit(lm[11]," ")[[1]][7])

loc.lm4.up <- as.numeric(strsplit(lm[12]," ")[[1]][11])
if(is.na(loc.lm4.up)) loc.lm4.up <- as.numeric(strsplit(lm[11]," ")[[1]][12])
if(is.na(loc.lm4.up)) loc.lm4.up <- as.numeric(strsplit(lm[11]," ")[[1]][13])
if(is.na(loc.lm4.up)) loc.lm4.up <- as.numeric(strsplit(lm[11]," ")[[1]][10])
if(is.na(loc.lm4.up)) loc.lm4.up <- as.numeric(strsplit(lm[11]," ")[[1]][9])

loc.lm5.up <- as.numeric(strsplit(lm[12]," ")[[1]][14])
if(is.na(loc.lm5.up)) loc.lm5.up <- as.numeric(strsplit(lm[11]," ")[[1]][15])
if(is.na(loc.lm5.up)) loc.lm5.up <- as.numeric(strsplit(lm[11]," ")[[1]][16])
if(is.na(loc.lm5.up)) loc.lm5.up <- as.numeric(strsplit(lm[11]," ")[[1]][13])
if(is.na(loc.lm5.up)) loc.lm5.up <- as.numeric(strsplit(lm[11]," ")[[1]][12])
if(is.na(loc.lm5.up)) loc.lm5.up <- as.numeric(strsplit(lm[11]," ")[[1]][11])


#Estimated regression parameters for scale
scale.lm1 <- as.numeric(strsplit(lm[17]," ")[[1]][3])
if(is.na(scale.lm1)) scale.lm1 <- as.numeric(strsplit(lm[17]," ")[[1]][4])

scale.lm2 <- as.numeric(strsplit(lm[17]," ")[[1]][5])
if(is.na(scale.lm2)) scale.lm2 <- as.numeric(strsplit(lm[17]," ")[[1]][6])
if(is.na(scale.lm2)) scale.lm2 <- as.numeric(strsplit(lm[17]," ")[[1]][7])

scale.lm3 <- as.numeric(strsplit(lm[17]," ")[[1]][8])
if(is.na(scale.lm3)) scale.lm3 <- as.numeric(strsplit(lm[17]," ")[[1]][9])
if(is.na(scale.lm3)) scale.lm3 <- as.numeric(strsplit(lm[17]," ")[[1]][10])
if(is.na(scale.lm3)) scale.lm3 <- as.numeric(strsplit(lm[17]," ")[[1]][7])

scale.lm4 <- as.numeric(strsplit(lm[17]," ")[[1]][11])
if(is.na(scale.lm4)) scale.lm4 <- as.numeric(strsplit(lm[17]," ")[[1]][12])
if(is.na(scale.lm4)) scale.lm4 <- as.numeric(strsplit(lm[17]," ")[[1]][13])
if(is.na(scale.lm4)) scale.lm4 <- as.numeric(strsplit(lm[17]," ")[[1]][10])
if(is.na(scale.lm4)) scale.lm4 <- as.numeric(strsplit(lm[17]," ")[[1]][9])

######################################

scale.lm1.dn <- as.numeric(strsplit(lm[16]," ")[[1]][3])
if(is.na(scale.lm1.dn)) scale.lm1.dn <- as.numeric(strsplit(lm[17]," ")[[1]][4])

scale.lm2.dn <- as.numeric(strsplit(lm[16]," ")[[1]][5])
if(is.na(scale.lm2.dn)) scale.lm2.dn <- as.numeric(strsplit(lm[17]," ")[[1]][6])
if(is.na(scale.lm2.dn)) scale.lm2.dn <- as.numeric(strsplit(lm[17]," ")[[1]][7])

scale.lm3.dn <- as.numeric(strsplit(lm[16]," ")[[1]][8])
if(is.na(scale.lm3.dn)) scale.lm3.dn <- as.numeric(strsplit(lm[17]," ")[[1]][9])
if(is.na(scale.lm3.dn)) scale.lm3.dn <- as.numeric(strsplit(lm[17]," ")[[1]][10])
if(is.na(scale.lm3.dn)) scale.lm3.dn <- as.numeric(strsplit(lm[17]," ")[[1]][7])

scale.lm4.dn <- as.numeric(strsplit(lm[16]," ")[[1]][11])
if(is.na(scale.lm4.dn)) scale.lm4.dn <- as.numeric(strsplit(lm[17]," ")[[1]][12])
if(is.na(scale.lm4.dn)) scale.lm4.dn <- as.numeric(strsplit(lm[17]," ")[[1]][13])
if(is.na(scale.lm4.dn)) scale.lm4.dn <- as.numeric(strsplit(lm[17]," ")[[1]][10])
if(is.na(scale.lm4.dn)) scale.lm4.dn <- as.numeric(strsplit(lm[17]," ")[[1]][9])

##########################################

scale.lm1.up <- as.numeric(strsplit(lm[18]," ")[[1]][3])
if(is.na(scale.lm1.up)) scale.lm1.up <- as.numeric(strsplit(lm[17]," ")[[1]][4])

scale.lm2.up <- as.numeric(strsplit(lm[18]," ")[[1]][5])
if(is.na(scale.lm2.up)) scale.lm2.up <- as.numeric(strsplit(lm[17]," ")[[1]][6])
if(is.na(scale.lm2.up)) scale.lm2.up <- as.numeric(strsplit(lm[17]," ")[[1]][7])

scale.lm3.up <- as.numeric(strsplit(lm[18]," ")[[1]][8])
if(is.na(scale.lm3.up)) scale.lm3.up <- as.numeric(strsplit(lm[17]," ")[[1]][9])
if(is.na(scale.lm3.up)) scale.lm3.up <- as.numeric(strsplit(lm[17]," ")[[1]][10])
if(is.na(scale.lm3.up)) scale.lm3.up <- as.numeric(strsplit(lm[17]," ")[[1]][7])

scale.lm4.up <- as.numeric(strsplit(lm[18]," ")[[1]][11])
if(is.na(scale.lm4.up)) scale.lm4.up <- as.numeric(strsplit(lm[17]," ")[[1]][12])
if(is.na(scale.lm4.up)) scale.lm4.up <- as.numeric(strsplit(lm[17]," ")[[1]][13])
if(is.na(scale.lm4.up)) scale.lm4.up <- as.numeric(strsplit(lm[17]," ")[[1]][10])
if(is.na(scale.lm4.up)) scale.lm4.up <- as.numeric(strsplit(lm[17]," ")[[1]][9])


filename=sprintf("/home/anitavd/data/dem1.bil")
con=file(filename,open="rb")
elev <- readBin(con,integer(),size=2,n=1550*1195)
close(con)
elev <- elev[TabID]
elev[elev==-1] = NA
dim(elev) <- c(length(elev),1)

filename<-"/home/anitavd/PhD/Sub-daily/Covariates/M5_rr3.bil"
con=file(filename,open="rb")
M5.3h <- readBin(con,integer(),size=2,n=1550*1195)
close(con)
M5.3h <- M5.3h[TabID]
M5.3h[M5.3h==-1] = NA
M5.3h[which(is.na(M5.3h))] = 0
dim(M5.3h) <- c(length(M5.3h),1)

filename<-"/home/anitavd/PhD/Sub-daily/Covariates/tam_jja.bil"
con=file(filename,open="rb")
JJAtemp <- readBin(con,integer(),size=2,n=1550*1195)
close(con)
JJAtemp[JJAtemp==-1] = NA
JJAtemp <- (JJAtemp[TabID]-2730)/10
dim(JJAtemp) <- c(length(JJAtemp),1)

con <- open.ncdf("/home/anitavd/PhD/Sub-daily/Covariates/north.nc")
north <- get.var.ncdf(con,"north_raster")
close(con)
north <- north[TabID]   
dim(north) <- c(length(north),1) 

cov1 <- (north-mean(north,na.rm=T))/apply(north,2,sd)
cov2 <- (JJAtemp-mean(JJAtemp,na.rm=T))/apply(JJAtemp,2,sd)
cov3 <- (elev-mean(elev,na.rm=T))/apply(elev,2,sd)
cov8 <- (M5.3h-mean(M5.3h,na.rm=T))/apply(M5.3h,2,sd)

loc.grid <- array(NA,dim=c(1195,1550))     
loc.grid[TabID] <- (rep(loc.lm1,length(TabID)) + rep(loc.lm2,length(TabID))*cov1 + rep(loc.lm3,length(TabID))*cov2 + rep(loc.lm4,length(TabID))*cov3 + rep(loc.lm5,length(TabID))*cov8)*100   #Two decimals
#Add extra columns with NA's to get a square of 1550*1550
# for (i in 1:355) loc.grid <- rbind(loc.grid,rep(NA,1550))
#Square map of coordinates
#x.grid <- y.grid <- seq(1,1550)

filename <- "/home/anitavd/PhD/Sub-daily/Results/loc.BHMest"

writebinfile(loc.grid,paste(filename,".bil",sep=""))

hdrfile <- paste(filename,".hdr",sep="")
blwfile <- paste(filename,".blw",sep="")

system("cp /home/anitavd/GISfile.hdr /home/anitavd/PhD/Sub-daily/Results/")
system("cp /home/anitavd/GISfile.blw /home/anitavd/PhD/Sub-daily/Results/")
system(paste("mv /home/anitavd/PhD/Sub-daily/Results/GISfile.hdr ",hdrfile,sep=""))
system(paste("mv /home/anitavd/PhD/Sub-daily/Results/GISfile.blw ",blwfile,sep="")) 

#Need to maximize and minimize loc and scale by trying different combinations of min/max regression parameters
#Spatially maximize/minimize in GIS

loc.grid.dn <- array(NA,dim=c(1195,1550))
loc.grid.dn[TabID] <- (rep(loc.lm1.dn,length(TabID)) + rep(loc.lm2.dn,length(TabID))*cov1 + rep(loc.lm3.dn,length(TabID))*cov2 + rep(loc.lm4.dn,length(TabID))*cov3 + rep(loc.lm5.dn,length(TabID))*cov8)*100   #Two decimals

filename <- "/home/anitavd/PhD/Sub-daily/Results/loc.dn.BHMest"

writebinfile(loc.grid.dn,paste(filename,".bil",sep=""))

loc.grid.up <- array(NA,dim=c(1195,1550))
loc.grid.up[TabID] <- (rep(loc.lm1.up,length(TabID)) + rep(loc.lm2.up,length(TabID))*cov1 + rep(loc.lm3.up,length(TabID))*cov2 + rep(loc.lm4.up,length(TabID))*cov3 + rep(loc.lm5.up,length(TabID))*cov8)*100   #Two decimals

filename <- "/home/anitavd/PhD/Sub-daily/Results/loc.up.BHMest"

writebinfile(loc.grid.up,paste(filename,".bil",sep=""))

scale.grid <- array(NA,dim=c(1195,1550))
scale.grid[TabID] <- (rep(scale.lm1,length(TabID)) + rep(scale.lm2,length(TabID))*cov2 + rep(scale.lm3,length(TabID))*cov3 + rep(scale.lm4,length(TabID))*cov8)*100   #Two decimals

filename <- "/home/anitavd/PhD/Sub-daily/Results/scale.BHMest"

writebinfile(scale.grid,paste(filename,".bil",sep=""))

hdrfile <- paste(filename,".hdr",sep="")
blwfile <- paste(filename,".blw",sep="")

system("cp /home/anitavd/GISfile.hdr /home/anitavd/PhD/Sub-daily/Results/")
system("cp /home/anitavd/GISfile.blw /home/anitavd/PhD/Sub-daily/Results/")
system(paste("mv /home/anitavd/PhD/Sub-daily/Results/GISfile.hdr ",hdrfile,sep=""))
system(paste("mv /home/anitavd/PhD/Sub-daily/Results/GISfile.blw ",blwfile,sep="")) 

scale.grid.dn <- array(NA,dim=c(1195,1550))
scale.grid.dn[TabID] <- (rep(scale.lm1.dn,length(TabID)) + rep(scale.lm2.dn,length(TabID))*cov2 + rep(scale.lm3.dn,length(TabID))*cov3 + rep(scale.lm4.dn,length(TabID))*cov8)*100   #Two decimals

filename <- "/home/anitavd/PhD/Sub-daily/Results/scale.dn.BHMest"

writebinfile(scale.grid.dn,paste(filename,".bil",sep=""))

scale.grid.up <- array(NA,dim=c(1195,1550))
scale.grid.up[TabID] <- (rep(scale.lm1.up,length(TabID)) + rep(scale.lm2.up,length(TabID))*cov2 + rep(scale.lm3.up,length(TabID))*cov3 + rep(scale.lm4.up,length(TabID))*cov8)*100   #Two decimals

filename <- "/home/anitavd/PhD/Sub-daily/Results/scale.up.BHMest"

writebinfile(scale.grid.up,paste(filename,".bil",sep=""))






                             

