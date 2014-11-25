##################################################################
#
# Plots return levels for hourly precipitation according to a given return period,
# and taking the estimated GEV parameters from the Bayesian Hierarchical model 
#
# 
# Anita Verpe Dyrrdal, met.no, Apr.2013
#
###################################################################

rm(list=ls())

source("/vol/klimadata/applikasjon/gridding/src/iobin.R")
source("/vol/klimadata/applikasjon/gridding/src/met_stat.R")

map.returnValues <- function(returnPeriod) {

TabID <- as.numeric(as.matrix(read.table("/home/anitavd/scripts/TabID.txt",header=TRUE)))

filename <- "/home/anitavd/PhD/Sub-daily/Results/loc.BHMest.bil"
con=file(filename,open="rb")
loc <- readBin(con,integer(),size=2,n=1550*1195)
close(con)
loc <- loc[TabID]/100
loc[loc==-1] = NA

filename <- "/home/anitavd/PhD/Sub-daily/Results/scale.BHMest.bil"
con=file(filename,open="rb")
scale <- readBin(con,integer(),size=2,n=1550*1195)
close(con)
scale <- scale[TabID]/100
scale[scale==-1] = NA

shape <- 0.15

#Compute return levels according to formula in Coles (2001), p.56

p <- 1/returnPeriod
y <- -log(1-p)
z <- loc - ((scale/shape) * (1 - y^(-shape)))
returnLevel <- array(NA,dim=c(1195,1550))
returnLevel[TabID] <- z

filename <- paste("/home/anitavd/PhD/Sub-daily/Results/M",returnPeriod,".BHMest",sep="")

writebinfile(returnLevel,paste(filename,".bil",sep=""))

hdrfile <- paste(filename,".hdr",sep="")
blwfile <- paste(filename,".blw",sep="")

system("cp /home/anitavd/GISfile.hdr /home/anitavd/PhD/Sub-daily/Results/")
system("cp /home/anitavd/GISfile.blw /home/anitavd/PhD/Sub-daily/Results/")
system(paste("mv /home/anitavd/PhD/Sub-daily/Results/GISfile.hdr ",hdrfile,sep=""))
system(paste("mv /home/anitavd/PhD/Sub-daily/Results/GISfile.blw ",blwfile,sep=""))

#Create confidence interval
#Lower
filename <- "/home/anitavd/PhD/Sub-daily/Results/loc.dn.BHMest.bil"
con=file(filename,open="rb")
loc <- readBin(con,integer(),size=2,n=1550*1195)
close(con)
loc <- loc[TabID]/100
loc[loc==-1] = NA

filename <- "/home/anitavd/PhD/Sub-daily/Results/scale.dn.BHMest.bil"
con=file(filename,open="rb")
scale <- readBin(con,integer(),size=2,n=1550*1195)
close(con)
scale <- scale[TabID]/100
scale[scale==-1] = NA

p <- 1/returnPeriod
y <- -log(1-p)
z <- loc - ((scale/shape) * (1 - y^(-shape)))
returnLevel <- array(NA,dim=c(1195,1550))
returnLevel[TabID] <- z

filename <- paste("/home/anitavd/PhD/Sub-daily/Results/M",returnPeriod,".dn.BHMest",sep="")

writebinfile(returnLevel,paste(filename,".bil",sep=""))

hdrfile <- paste(filename,".hdr",sep="")
blwfile <- paste(filename,".blw",sep="")

system("cp /home/anitavd/GISfile.hdr /home/anitavd/PhD/Sub-daily/Results/")
system("cp /home/anitavd/GISfile.blw /home/anitavd/PhD/Sub-daily/Results/")
system(paste("mv /home/anitavd/PhD/Sub-daily/Results/GISfile.hdr ",hdrfile,sep=""))
system(paste("mv /home/anitavd/PhD/Sub-daily/Results/GISfile.blw ",blwfile,sep=""))

#Upper
filename <- "/home/anitavd/PhD/Sub-daily/Results/loc.up.BHMest.bil"
con=file(filename,open="rb")
loc <- readBin(con,integer(),size=2,n=1550*1195)
close(con)
loc <- loc[TabID]/100
loc[loc==-1] = NA

filename <- "/home/anitavd/PhD/Sub-daily/Results/scale.up.BHMest.bil"
con=file(filename,open="rb")
scale <- readBin(con,integer(),size=2,n=1550*1195)
close(con)
scale <- scale[TabID]/100
scale[scale==-1] = NA

p <- 1/returnPeriod
y <- -log(1-p)
z <- loc - ((scale/shape) * (1 - y^(-shape)))
returnLevel <- array(NA,dim=c(1195,1550))
returnLevel[TabID] <- z

filename <- paste("/home/anitavd/PhD/Sub-daily/Results/M",returnPeriod,".up.BHMest",sep="")

writebinfile(returnLevel,paste(filename,".bil",sep=""))

hdrfile <- paste(filename,".hdr",sep="")
blwfile <- paste(filename,".blw",sep="")

system("cp /home/anitavd/GISfile.hdr /home/anitavd/PhD/Sub-daily/Results/")
system("cp /home/anitavd/GISfile.blw /home/anitavd/PhD/Sub-daily/Results/")
system(paste("mv /home/anitavd/PhD/Sub-daily/Results/GISfile.hdr ",hdrfile,sep=""))
system(paste("mv /home/anitavd/PhD/Sub-daily/Results/GISfile.blw ",blwfile,sep=""))

} 


