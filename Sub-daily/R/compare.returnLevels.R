#Compare "observed" and modelled return levels at observational sites

rm(list=ls())
gc(reset=TRUE)
library(SpatialExtremes)
library(extRemes)
library(ismev)
library(ncdf)

model <- 1

loc <- c()
scale <- c()
shape.est <- c()
east <- c()
north <- c()
lon <- c()
lat <- c()
stnr <- c()
elev <- c()
M5.3h <- c()
tam.jja <- c()
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


		loc <- rbind(loc,gev.fit(data)$mle[1])
		scale <- rbind(scale,gev.fit(data)$mle[2]) 
		shape.est <- rbind(shape.est,gev.fit(data)$mle[3])  

		stnr <- rbind(stnr, station$STNR[1])
		east <- rbind(east, station$EAST[1])
		north <- rbind(north, station$NORTH[1])
		lon <- rbind(lon, station$LON[1])
		lat <- rbind(lat, station$LAT[1])
		#data.am[1:length(data),j] <- data
		no.obs <- max(no.obs,length(which(!is.na(data))))


		#IMPORT ALL COVARIATES
		#Extract elevation from climate grid
		x = round((station$EAST - (-75000 + 500)) / 1000, digits = 0) + 1
		y = round((station$NORTH - (6450000 + 500)) / 1000, digits = 0) + 1
		point = (1550 - y) * 1195 + (x - 1) + 1
		filename=sprintf("/home/anitavd/data/dem1.bil")
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

		#M5 for 3-hour precipitation
		filename<-"/home/anitavd/PhD/Sub-daily/Covariates/M5_rr3.bil"
		con=file(filename,open="rb")
		seek(con, where = ((point - 1) * 2), origin = "start")   # Set position
		M5.3h <- rbind(M5.3h,readBin(con,integer(),size=2,n=1))
		close(con)

		#Summer temperature
		filename<-"/home/anitavd/PhD/Sub-daily/Covariates/tam_jja.bil"
		con=file(filename,open="rb")
		seek(con, where = ((point - 1) * 2), origin = "start")   # Set position
		tam.jja <- rbind(tam.jja,(readBin(con,integer(),size=2,n=1)-2730)/10)
		close(con)

		j <- j+1
		
	}

}

no.sites <- length(loc)

data.am <- data.am[,1:(no.sites+1)]

data.am.in <- data.am[,-c(1,3,5,7,9,11,13,15,17,19,21,23,25,27,29,31,33,35,37,39,41,43,45,47,49,51,53,55,57,59,61,63,65,67,69)]
loc.check <- loc[-c(1,3,5,7,9,11,13,15,17,19,21,23,25,27,29,31,33,35,37,39,41,43,45,47,49,51,53,55,57,59,61,63,65,67,69),]
scale.check <- scale[-c(1,3,5,7,9,11,13,15,17,19,21,23,25,27,29,31,33,35,37,39,41,43,45,47,49,51,53,55,57,59,61,63,65,67,69),]
shape <- rep(0.10,35)
y <- -log(1-(1/5))
est.M5 <- loc.check - ((scale.check/shape)*(1-(y^(-shape))))
mod.shape <- rep(NA,35)
#wait for estimation of M5 and separation into classes
#mod.shape[which(est.M5<40)] = 0.10
mod.shape <- shape
obs.M5 <- loc.check - ((scale.check/mod.shape)*(1-(y^(-mod.shape))))
y <- -log(1-(1/10))
obs.M10 <- loc.check - ((scale.check/mod.shape)*(1-(y^(-mod.shape))))
y <- -log(1-(1/50))
obs.M50 <- loc.check - ((scale.check/mod.shape)*(1-(y^(-mod.shape))))
y <- -log(1-(1/100))
obs.M100 <- loc.check - ((scale.check/mod.shape)*(1-(y^(-mod.shape))))
y <- -log(1-(1/200))
obs.M200 <- loc.check - ((scale.check/mod.shape)*(1-(y^(-mod.shape))))

north.check <- north[-c(1,3,5,7,9,11,13,15,17,19,21,23,25,27,29,31,33,35,37,39,41,43,45,47,49,51,53,55,57,59,61,63,65,67,69),]
east.check <- east[-c(1,3,5,7,9,11,13,15,17,19,21,23,25,27,29,31,33,35,37,39,41,43,45,47,49,51,53,55,57,59,61,63,65,67,69),]

coord <- cbind(east,north)
colnames(coord) <- c("east","north")

east.norm <- (east-mean(east,na.rm=T))/apply(east,2,sd)
north.norm <- (north-mean(north,na.rm=T))/apply(north,2,sd)

coord.norm <- cbind(east.norm, north.norm)
coord.norm <- coord.norm[-c(2,4,6,8,10,12,14,16,18,20,22,24,26,28,30,32,34,36,38,40,42,44,46,48,50,52,54,56,58,60,62,64,66,68),]
colnames(coord.norm) <- c("x","y")

elev[which(elev == -1)] = NA
M5.3h[which(M5.3h == -1)] = NA

##################################################################################################
#Specify covariates

#Normalize covariates
cov1 <- (north-mean(north,na.rm=T))/apply(north,2,sd)
cov2 <- (east-mean(east,na.rm=T))/apply(east,2,sd)
cov3 <- (tam.jja-mean(tam.jja,na.rm=T))/apply(tam.jja,2,sd)
cov4 <- (elev-mean(elev,na.rm=T))/apply(elev,2,sd)
cov7 <- (M5.3h-mean(M5.3h,na.rm=T))/apply(M5.3h,2,sd)

shape.form <- y ~ 1 
loc.form <- y ~ cov1 + cov3 + cov4 + cov7
scale.form <- y ~ cov3 + cov7

s11 <- 2
s12 <- 6
s21 <- 2
s22 <- 2
r11 <- 2
r12 <- 2
r21 <- 1.5
r22 <- 1.5

#Hyperparameters
hyper <- list()
hyper$sills <- list(loc = c(s11,s12), scale = c(s21,s22), shape = c(2,1))  #inverse gamma prior
hyper$ranges <- list(loc = c(r11,r12), scale = c(r21,r22), shape = c(2,1))  #gamma prior, uninformative	
hyper$smooths <- list(loc = c(1,1/3), scale = c(1,1/3), shape = c(2,1))	 #gamma prior, uninformative
hyper$betaMeans <- list(loc = c(5,0,0,0,0), scale = c(2,0,0), shape = 0.1)	 #multivariate normal distribution, uninformative 
 
hyper$betaIcov <- list(loc = solve(diag(c(100,100,100,100,100))),scale=solve(diag(c(100,100,100))),shape=solve(diag(c(0.0000000001), 1, 1)))  #covariance matrix

prop <- list(gev = c(1.2, 0.08, 0.1), ranges = c(0.7, 0.8, 0.7), smooths = c(0,0,0))

start <- list(sills = c(5,2,2), ranges = c(1,1,10), smooths= c(1,1,1), beta = list(loc = c(5,1,1,1,1), scale = c(2,1,1),shape = c(0.1)))   

marg.cov <- cbind(cov1[-c(2,4,6,8,10,12,14,16,18,20,22,24,26,28,30,32,34,36,38,40,42,44,46,48,50,52,54,56,58,60,62,64,66,68),],cov3[-c(2,4,6,8,10,12,14,16,18,20,22,24,26,28,30,32,34,36,38,40,42,44,46,48,50,52,54,56,58,60,62,64,66,68),],cov4[-c(2,4,6,8,10,12,14,16,18,20,22,24,26,28,30,32,34,36,38,40,42,44,46,48,50,52,54,56,58,60,62,64,66,68),],cov7[-c(2,4,6,8,10,12,14,16,18,20,22,24,26,28,30,32,34,36,38,40,42,44,46,48,50,52,54,56,58,60,62,64,66,68),])
colnames(marg.cov) <- c("cov1","cov3","cov4","cov7")

#Run model, Markov chain. data should be a matrix with no NA's. marg.cov???
mc <- latent(data=data.am.in, coord.norm, marg.cov = marg.cov, loc.form = loc.form, scale.form = scale.form,shape.form = shape.form, hyper = hyper, prop = prop, start = start, n = 100, burn.in= 30, thin = 5)   #RETURN THIS LIST! n=300000, burn.in=5000, thin=30 (Davison...)

sink(paste("~/PhD/Sub-daily/Results/mc_compMod.txt",sep=""))
print(mc)
sink()

no.cov <- 4

con <- open.ncdf("~/PhD/Sub-daily/Covariates/north.nc")
lat <- get.var.ncdf(con,"north_raster")
close(con)
cov1 <- (lat-mean(lat,na.rm=T))/apply(lat,2,sd)

con <- open.ncdf("~/PhD/Sub-daily/Covariates/east.nc")
lon <- get.var.ncdf(con,"east_raster")
close(con)
cov2 <- (lat-mean(lon,na.rm=T))/apply(lon,2,sd)

filename<-"~/PhD/Sub-daily/Covariates/tam_jja.bil"
con=file(filename,open="rb")
JJAtemp <- readBin(con,integer(),size=2,n=1550*1195)
close(con)
JJAtemp[JJAtemp==-1] = NA
JJAtemp <- (JJAtemp-2730)/10
dim(JJAtemp) <- c(1195,1550)
cov3 <- (JJAtemp-mean(JJAtemp,na.rm=T))/apply(JJAtemp,2,sd)

filename=sprintf("/vol/klimagrid/senorge/dem1.bil")
con=file(filename,open="rb")
elev <- readBin(con,integer(),size=2,n=1550*1195)
close(con)
elev[elev==-1] = NA
dim(elev) <- c(1195,1550)
cov4 <- (elev-mean(elev,na.rm=T))/apply(elev,2,sd)

filename<-"~/PhD/Sub-daily/Covariates/M5_rr3.bil"
con=file(filename,open="rb")
M5.3h <- readBin(con,integer(),size=2,n=1550*1195)
close(con)
M5.3h[M5.3h==-1] = NA
dim(M5.3h) <- c(1195,1550)
cov7 <- (M5.3h-mean(M5.3h,na.rm=T))/apply(M5.3h,2,sd)

for (i in 1:355) {
	cov1 <- rbind(cov1,rep(NA,1550))
	cov3 <- rbind(cov3,rep(NA,1550))
	cov4 <- rbind(cov4,rep(NA,1550))
	cov7 <- rbind(cov7,rep(NA,1550))
}

cov <- array(NA,c(1550,1550,no.cov))
cov[,,1] <- cov1
cov[,,2] <- cov3
cov[,,3] <- cov4
cov[,,4] <- cov7

dimnames(cov) <- list(NULL,NULL,c("cov1","cov3","cov4","cov7"))

rm(cov1)
rm(cov2)
rm(cov3)
rm(cov4)
rm(cov7)
gc(reset=TRUE)

x.grid <- y.grid <- seq(1,1550)
mod.loc <- map.latent(mc,x.grid,y.grid,covariates=cov,param="loc")
mod.loc <- mod.loc$post.sum
mod.loc <- mod.loc[1:1195,]
mod.scale <- map.latent(mc,x.grid,y.grid,covariates=cov,param="scale")
mod.scale <- mod.scale$post.sum
mod.scale <- mod.scale[1:1195,]

#Pick out the correct pixel and compute return level -> compare to observed
MAE.M5 <- 0
RMSE.M5 <- 0
MAE.M10 <- 0
RMSE.M10 <- 0
MAE.M50 <- 0
RMSE.M50 <- 0
MAE.M100 <- 0
RMSE.M100 <- 0
MAE.M200 <- 0
RMSE.M200 <- 0

for(i in 1:35) {

	x = round((east[i] - (-75000 + 500)) / 1000, digits = 0) + 1
	y = round((north[i] - (6450000 + 500)) / 1000, digits = 0) + 1
	point = (1550 - y) * 1195 + (x - 1) + 1  
	mod.loc.point <- mod.loc[point]
	mod.scale.point <- mod.scale[point]
	y <- -log(1-(1/5))
	mod.M5 <- mod.loc.point - ((mod.scale.point/0.1)*(1-(y^(-0.1))))

	MAE.M5 <- MAE.M5 + abs(obs.M5[i] - mod.M5)
	RMSE.M5 <- RMSE.M5 + (obs.M5[i] - mod.M5)^2

}

print(MAE.M5)
print(RMSE.M5)
	





