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

loc <- c()
scale <- c()
shape.est <- c()
east <- c()
north <- c()
lon <- c()
lat <- c()
stnr <- c()
elev <- c()
M5 <- c()
M5.3h <- c()
loc.3h <- c()
tam.jja <- c()
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


		loc <- rbind(loc,gev.fit(data,show=F)$mle[1])
		scale <- rbind(scale,gev.fit(data,show=F)$mle[2]) 
		shape.est <- rbind(shape.est,gev.fit(data,show=F)$mle[3])  

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
		tam.jja <- rbind(tam.jja,(readBin(con,integer(),size=2,n=1)-2730)/10)
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
