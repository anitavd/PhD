######################################################################################
#
# Bayesian hierarchical model for extreme hourly precipitation
# Uses observed hourly precipitation from met.no stations, and gridded covariates to
# distribute GEV parameters in space. 
# The R-package SpatialExtremes (latent) is used to run a Markov chain. 
#
#
# Anita Verpe Dyrrdal, MET, Aug.2013
#
######################################################################################

rm(list=ls())
library(SpatialExtremes)
library(extRemes)
library(ismev)
library(ncdf)
library(evd)

loc <- c()
scale <- c()
east <- c()
north <- c()
lon <- c()
lat <- c()
stnr <- c()
elev <- c()
M5 <- c()
M5.3h <- c()
loc.3h <- c()
JJAtemp <- c()
summerRR <- c()
distSea <- c()
data.am <- matrix(NA,46,80)
data.am[,1] <- seq(1967,2012)
j <- 1
no.obs <- 0   #length of the longest series
t<-2

#Read files with observations
tipping.stations <- list.files("/home/anitavd/PhD/Sub-daily/data/tipping/",pattern = "hourly_AM.txt", full.names=F)
geonor.stations <- list.files("/home/anitavd/PhD/Sub-daily/data/geonor/",pattern = "hourly_AM.txt", full.names=F)
all.stations <- c(tipping.stations,geonor.stations)
no.stations <- length(all.stations)

#TabID <- as.numeric(as.matrix(read.table("/home/anitavd/scripts/TabID.txt",header=TRUE)))
filename <- "/home/anitavd/PhD/Sub-daily/RR3/snowmask.bil"
con=file(filename,open="rb")
snowmask=readBin(con,integer(),size=1,n=1550*1195)
close(con)
TabID <- which(snowmask==1)

for (i in 1:no.stations) {
print(i)
	file <- all.stations[i]
	station <- try(read.table(paste("/home/anitavd/PhD/Sub-daily/data/tipping/",file,sep=""),header=F,skip=3),silent=T)
	if(class(station)=="try-error") station <- try(read.table(paste("/home/anitavd/PhD/Sub-daily/data/geonor/",file,sep=""),header=F,skip=3),silent=T)
	colnames(station) <- c("STNR","EAST","NORTH","LON","LAT","YEAR","RR_1")
	station$RR_1[which(station$RR_1 >= 50)] = NA
	data <- station$RR_1[!is.na(station$RR_1)]

	if(length(data) > 9) {

		first.year <- station$YEAR[1]
		last.year <- tail(station$YEAR,1)
		idx1 <- which(data.am[,1]==first.year)
		idx2 <- which(data.am[,1]==last.year)
		for (l in idx1:idx2) {
			try(data.am[l,t] <- station$RR_1[which(station$YEAR==data.am[l,1])],silent=T)
		}
		t <- t+1


		loc <- rbind(loc,fgev(data,shape=0.15)$estimate[1])
		scale <- rbind(scale,fgev(data,shape=0.15)$estimate[2]) 

		stnr <- rbind(stnr, station$STNR[1])
		east <- rbind(east, station$EAST[1])
		north <- rbind(north, station$NORTH[1])
		lon <- rbind(lon, station$LON[1])
		lat <- rbind(lat, station$LAT[1])
		#data.am[1:length(data),j] <- data
		no.obs <- max(no.obs,length(which(!is.na(data))))

		#Extract elevation from climate grid
		x = round((station$EAST - (-75000 + 500)) / 1000, digits = 0) + 1
		y = round((station$NORTH - (6450000 + 500)) / 1000, digits = 0) + 1
		point = (1550 - y) * 1195 + (x - 1) + 1
		filename=sprintf("/vol/klimagrid/senorge/dem1.bil")
		con=file(filename,open="rb")
		seek(con, where = ((point - 1) * 2), origin = "start")   # Set position
		elev.point <- readBin(con, integer(), size=2, n=1)	# [m] 
		close(con)

		#If grid cell is outside the grid, find a grid cell nearby that is within the grid
		if(elev.point==0) {
			offset <- c(1, -1, 1195, -1195, 1196, -1196, 1194, -1194)
			for (k in 1:8) {
				con=file(filename,open="rb")
				seek(con, where = ((point + offset[k] - 1) * 2), origin = "start")   # Set position
				elev.point <- readBin(con, integer(), size=2, n=1)	# [m] 
				close(con)
				if(elev.point>0) {
					point <- point + offset[k]				
					break()
				}
			}
		}
		elev <- rbind(elev,as.numeric(elev.point))			

		#Extract M5(24h) computed from the climate grid
		filename=sprintf("/vol/klimadata/archive/anitavd/M200/ny/M5_RRuncor.bil")
		con=file(filename,open="rb")
		seek(con, where = ((point - 1) * 2), origin = "start")   # Set position
		M5 <- rbind(M5,readBin(con, integer(), size=2, n=1))	# [mm] 
		close(con)

		#M5 for 3-hour precipitation
		filename<-"/home/anitavd/PhD/Sub-daily/Covariates/M5_rr3.bil"
		con=file(filename,open="rb")
		seek(con, where = ((point - 1) * 2), origin = "start")   # Set position
		M5.3h <- rbind(M5.3h,readBin(con,integer(),size=2,n=1))
		close(con)

		#M5 for 3-hour precipitation
		filename<-"/home/anitavd/PhD/Sub-daily/Covariates/loc.RR3.bil"
		con=file(filename,open="rb")
		seek(con, where = ((point - 1) * 2), origin = "start")   # Set position
		loc.3h <- rbind(loc.3h,readBin(con,integer(),size=2,n=1))
		close(con)

		#Summer temperature
		filename<-"/home/anitavd/PhD/Sub-daily/Covariates/tam_jja.bil"
		con=file(filename,open="rb")
		seek(con, where = ((point - 1) * 2), origin = "start")   # Set position
		JJAtemp <- rbind(JJAtemp,(readBin(con,integer(),size=2,n=1)-2730)/10)
		close(con)

		#Area dominated by summer precipitation (1)
		filename<-"/home/anitavd/PhD/Sub-daily/Covariates/area_summerPrecip.bil"
		con=file(filename,open="rb")
		seek(con, where = ((point - 1) * 2), origin = "start")   # Set position
		summerRR <- rbind(summerRR,readBin(con,integer(),size=2,n=1))
		close(con)

		#Distance from coast
		filename<-"/home/anitavd/PhD/Sub-daily/Covariates/distsea_km.bil"
		con=file(filename,open="rb")
		seek(con, where = ((point - 1) * 2), origin = "start")   # Set position
		distSea <- rbind(distSea,readBin(con,integer(),size=2,n=1))
		close(con)

		j <- j+1
		
	}

}

no.sites <- length(loc)

data.am <- data.am[,1:(no.sites+1)]

shape <- rep(0.15,no.sites)

elev[which(elev == -1)] = NA
M5[which(M5 == -1)] = NA
M5.3h[which(M5.3h == -1)] = NA
loc.3h[which(loc.3h == -1)] = NA
loc.3h <- loc.3h/100
JJAtemp[which(JJAtemp == -1)] = NA

#Read entire grid to compute mean and sd for the normalization
con <- open.ncdf("~/PhD/Sub-daily/Covariates/north.nc")
lat.grid <- get.var.ncdf(con,"north_raster")
close(con)
cov1.mean <- mean(lat.grid[TabID],na.rm=T)
cov1.sd <- sd(lat.grid[TabID],na.rm=T)

con <- open.ncdf("~/PhD/Sub-daily/Covariates/east.nc")
lon.grid <- get.var.ncdf(con,"east_raster")
close(con)
cov2.mean <- mean(lon.grid[TabID],na.rm=T)
cov2.sd <- sd(lon.grid[TabID],na.rm=T)

filename<-"~/PhD/Sub-daily/Covariates/tam_jja.bil"
con=file(filename,open="rb")
JJAtemp.grid <- readBin(con,integer(),size=2,n=1550*1195)
close(con)
JJAtemp.grid[which(JJAtemp.grid ==-1)] = NA
JJAtemp.grid <- (JJAtemp.grid - 2731)/10   #celsius
cov3.mean <- mean(JJAtemp.grid[TabID],na.rm=T)
cov3.sd <- sd(JJAtemp.grid[TabID],na.rm=T)

filename=sprintf("/vol/klimagrid/senorge/dem1.bil")
con=file(filename,open="rb")
elev.grid <- readBin(con,integer(),size=2,n=1550*1195)
close(con)
elev.grid[which(elev.grid ==-1)] = NA
cov4.mean <- mean(elev.grid[TabID],na.rm=T)
cov4.sd <- sd(elev.grid[TabID],na.rm=T)

filename<-"~/PhD/Sub-daily/Covariates/M5_rr3.bil"
con=file(filename,open="rb")
M5.3h.grid <- readBin(con,integer(),size=2,n=1550*1195)
close(con)
M5.3h.grid[which(M5.3h.grid ==-1)] = NA
cov7.mean <- mean(M5.3h.grid[TabID],na.rm=T)
cov7.sd <- sd(M5.3h.grid[TabID],na.rm=T)

#Normalize coordinates
coord <- cbind(east,north)
colnames(coord) <- c("east","north")
east.norm <- (east-cov2.mean)/cov2.sd
north.norm <- (north-cov1.mean)/cov1.sd
coord.norm <- cbind(east.norm, north.norm)
colnames(coord.norm) <- c("cov2","cov1")


###########################################################################################################################
#2nd layer

#Spatial linear model for the mean of the latent process, link to covariates
#Normalize covariates
cov1 <- north.norm
cov2 <- east.norm
cov3 <- (JJAtemp-cov3.mean)/cov3.sd
cov4 <- (elev-cov4.mean)/cov4.sd
#cov5 <- (distSea-mean(distSea,na.rm=T))/apply(distSea,2,sd)
#cov6 <- (M5-mean(M5,na.rm=T))/apply(M5,2,sd)
cov7 <- (M5.3h-cov7.mean)/cov7.sd
#cov8 <- (loc.3h-mean(loc.3h,na.rm=T))/apply(loc.3h,2,sd)
#cov9 <- (MAP-mean(MAP,na.rm=T))/apply(MAP,2,sd)
#cov10 <- (MSP-mean(MSP,na.rm=T))/apply(MSP,2,sd)
#cov11 <- (wetDays-mean(wetDays,na.rm=T))/apply(wetDays,2,sd)
#cov12 <- cov2*summerRR

loc.form <- y ~ cov2 + cov3 + cov4 + cov7
scale.form <- y ~ cov3 + cov4 + cov7
shape.form <- y ~ 1    #constant

#range in meters
#s11 <- 2
#s12 <- 20
#s21 <- 2
#s22 <- 4
#r11 <- 2
#r12 <- 8000
#r21 <- 2
#r22 <- 8000

#Normalized
s11 <- 2
s12 <- 6
s21 <- 2
s22 <- 2
r11 <- 2
r12 <- 2
r21 <- 1.5
r22 <- 1.5

#####################################################################################################################################################
#3rd layer

hyper <- list()
#hyper$sills <- list(loc = c(s11,s12), scale = c(s21,s22), shape = c(2,1))  #inverse gamma prior
hyper$sills <- list(loc = c(0,0), scale = c(0,0), shape = c(0,0))  #inverse gamma prior
hyper$ranges <- list(loc = c(r11,r12), scale = c(r21,r22), shape = c(2,1))  #gamma prior, uninformative	
hyper$smooths <- list(loc = c(1,1/3), scale = c(1,1/3), shape = c(2,1))	 #gamma prior, uninformative
hyper$betaMeans <- list(loc = c(5,0,0,0,0), scale = c(2,0,0,0), shape = 0.15)	 #multivariate normal distribution, uninformative 
hyper$betaIcov <- list(loc = solve(diag(c(100,100,100,100,100))),scale=solve(diag(c(100,100,100,100))),shape=solve(diag(c(0.0000000001), 1, 1)))  #covariance matrix

prop <- list(gev = c(1.2, 0.08, 0), ranges = c(0.7, 0.8, 0), smooths = c(0,0,0))
 
#start <- list(sills = c(5,2,2), ranges = c(1,1,10), smooths= c(1,1,1), beta = list(loc = c(5,1,1,1,1), scale = c(2,1,1,1),shape = c(0.15)))
start <- list(sills = c(0.000001,0.000001,0.000001), ranges = c(1,1,10), smooths= c(1,1,1), beta = list(loc = c(5,1,1,1,1), scale = c(2,1,1,1),shape = c(0.15)))  

marg.cov <- cbind(cov2,cov3,cov4,cov7)
colnames(marg.cov) <- c("cov2","cov3","cov4","cov7")
print(marg.cov)

#Run model, Markov chain. 
mc <- latent.fix.shape(data=data.am[,-1], coord.norm, marg.cov = marg.cov, loc.form = loc.form, scale.form = scale.form,shape.form = shape.form, hyper = hyper, prop = prop, start = start, n = 10000, burn.in= 300, thin = 15)  

sink(paste("/home/anitavd/PhD/Sub-daily/Test_models/mc_mod6.txt",sep=""))
print(mc)
sink()




