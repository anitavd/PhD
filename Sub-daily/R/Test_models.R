library(evd)
library(ncdf)
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
cov7 <- (M5.3h-cov7.mean)/cov7.sd

#####################################################################################
#UPDATE

mod1.loc <- 7.4931 + 1.6343*cov3 + 1.3089*cov4 + 1.6766*cov7
mod1.scale <- 2.65362 + 0.29998*cov3 + 0.33832*cov7 

mod2.loc <- 7.5408 + 1.6747*cov3 + 1.4164*cov4 + 1.6875*cov7
mod2.scale <- 2.82815 + 0.61537*cov3 + 0.43035*cov4 + 0.40009*cov7

mod3.loc <- 7.3965 + 0.3846*cov1 + 1.8204*cov3 + 1.4529*cov4 + 1.7182*cov7
mod3.scale <- 2.65084 + 0.31123*cov3 + 0.33633*cov7

mod4.loc <- 7.4908 + 0.4239*cov1 + 1.8547*cov3 + 1.5726*cov4 + 1.7508*cov7
mod4.scale <- 2.84957 + 0.60122*cov3 + 0.42716*cov4 + 0.39884*cov7

mod5.loc <- 7.3840 + 0.3739*cov2 + 1.7316*cov3 + 1.3946*cov4 + 1.7269*cov7 
mod5.scale <- 2.66292 + 0.30592*cov3 + 0.33776*cov7

mod6.loc <- 7.4766 + 0.3813*cov2 + 1.7627*cov3 + 1.5067*cov4 + 1.7477*cov7
mod6.scale <- 2.8691 + 0.6056*cov3  +  0.4485*cov4 + 0.4026*cov7

shape <- 0.15

#Mean and variance
obs.mean <- loc + scale*(gamma(1-shape)-1)/shape
mod1.mean <- mod1.loc + mod1.scale*(gamma(1-shape)-1)/shape
mod2.mean <- mod2.loc + mod2.scale*(gamma(1-shape)-1)/shape
mod3.mean <- mod3.loc + mod3.scale*(gamma(1-shape)-1)/shape
mod4.mean <- mod4.loc + mod4.scale*(gamma(1-shape)-1)/shape
mod5.mean <- mod5.loc + mod5.scale*(gamma(1-shape)-1)/shape
mod6.mean <- mod6.loc + mod6.scale*(gamma(1-shape)-1)/shape

obs.var <- (scale^2)*(gamma(1-(2*shape))-(gamma(1-shape))^2)/(shape^2)
mod1.var <- (mod1.scale^2)*(gamma(1-(2*shape))-(gamma(1-shape))^2)/(shape^2)
mod2.var <- (mod2.scale^2)*(gamma(1-(2*shape))-(gamma(1-shape))^2)/(shape^2)
mod3.var <- (mod3.scale^2)*(gamma(1-(2*shape))-(gamma(1-shape))^2)/(shape^2)
mod4.var <- (mod4.scale^2)*(gamma(1-(2*shape))-(gamma(1-shape))^2)/(shape^2)
mod5.var <- (mod5.scale^2)*(gamma(1-(2*shape))-(gamma(1-shape))^2)/(shape^2)
mod6.var <- (mod6.scale^2)*(gamma(1-(2*shape))-(gamma(1-shape))^2)/(shape^2)

obs.2mom <- obs.var + (obs.mean)^2
mod1.2mom <- mod1.var + (mod1.mean)^2
mod2.2mom <- mod2.var + (mod2.mean)^2
mod3.2mom <- mod3.var + (mod3.mean)^2
mod4.2mom <- mod4.var + (mod4.mean)^2
mod5.2mom <- mod5.var + (mod5.mean)^2
mod6.2mom <- mod6.var + (mod6.mean)^2

#Quantiles
obs.med <- loc + (scale/shape)*((log(2))^(-shape)-1)
mod1.med <- mod1.loc + (mod1.scale/shape)*((log(2))^(-shape)-1)
mod2.med <- mod2.loc + (mod2.scale/shape)*((log(2))^(-shape)-1)
mod3.med <- mod3.loc + (mod3.scale/shape)*((log(2))^(-shape)-1)
mod4.med <- mod4.loc + (mod4.scale/shape)*((log(2))^(-shape)-1)
mod5.med <- mod5.loc + (mod5.scale/shape)*((log(2))^(-shape)-1)
mod6.med <- mod6.loc + (mod6.scale/shape)*((log(2))^(-shape)-1)

obs.q75 <- loc + (scale/shape)*((log(1/0.75))^(-shape)-1)
mod1.q75 <- mod1.loc + (mod1.scale/shape)*((log(1/0.75))^(-shape)-1)
mod2.q75 <- mod2.loc + (mod2.scale/shape)*((log(1/0.75))^(-shape)-1)
mod3.q75 <- mod3.loc + (mod3.scale/shape)*((log(1/0.75))^(-shape)-1)
mod4.q75 <- mod4.loc + (mod4.scale/shape)*((log(1/0.75))^(-shape)-1)
mod5.q75 <- mod5.loc + (mod5.scale/shape)*((log(1/0.75))^(-shape)-1)
mod6.q75 <- mod6.loc + (mod6.scale/shape)*((log(1/0.75))^(-shape)-1)

obs.q90 <- loc + (scale/shape)*((log(1/0.9))^(-shape)-1)
mod1.q90 <- mod1.loc + (mod1.scale/shape)*((log(1/0.9))^(-shape)-1)
mod2.q90 <- mod2.loc + (mod2.scale/shape)*((log(1/0.9))^(-shape)-1)
mod3.q90 <- mod3.loc + (mod3.scale/shape)*((log(1/0.9))^(-shape)-1)
mod4.q90 <- mod4.loc + (mod4.scale/shape)*((log(1/0.9))^(-shape)-1)
mod5.q90 <- mod5.loc + (mod5.scale/shape)*((log(1/0.9))^(-shape)-1)
mod6.q90 <- mod6.loc + (mod6.scale/shape)*((log(1/0.9))^(-shape)-1)

obs.q99 <- loc + (scale/shape)*((log(1/0.99))^(-shape)-1)
mod1.q99 <- mod1.loc + (mod1.scale/shape)*((log(1/0.99))^(-shape)-1)
mod2.q99 <- mod2.loc + (mod2.scale/shape)*((log(1/0.99))^(-shape)-1)
mod3.q99 <- mod3.loc + (mod3.scale/shape)*((log(1/0.99))^(-shape)-1)
mod4.q99 <- mod4.loc + (mod4.scale/shape)*((log(1/0.99))^(-shape)-1)
mod5.q99 <- mod5.loc + (mod5.scale/shape)*((log(1/0.99))^(-shape)-1)
mod6.q99 <- mod6.loc + (mod6.scale/shape)*((log(1/0.99))^(-shape)-1)

i=40   #location

#Plot distribution
#ymax <- max(dgev(seq(0,25,0.1),loc[i],scale[i],shape)) + 0.05

#plot(seq(0,25,0.1),dgev(seq(0,25,0.1),loc[i],scale[i],shape),"l",ylim=c(0,ymax))
#lines(seq(0,25,0.1),dgev(seq(0,25,0.1),mod1.loc[i],mod1.scale[i],shape),col="red")
#lines(seq(0,25,0.1),dgev(seq(0,25,0.1),mod2.loc[i],mod2.scale[i],shape),col="blue")
#lines(seq(0,25,0.1),dgev(seq(0,25,0.1),mod3.loc[i],mod3.scale[i],shape),col="yellow")
#lines(seq(0,25,0.1),dgev(seq(0,25,0.1),mod4.loc[i],mod4.scale[i],shape),col="green")
#lines(seq(0,25,0.1),dgev(seq(0,25,0.1),mod5.loc[i],mod5.scale[i],shape),col="orange")
#lines(seq(0,25,0.1),dgev(seq(0,25,0.1),mod6.loc[i],mod6.scale[i],shape),col="pink")

#Median and quantiles
#abline(v=obs.med[i])
#abline(v=mod1.med[i],col="red")
#abline(v=mod2.med[i],col="blue")
#abline(v=mod3.med[i],col="yellow")
#abline(v=mod4.med[i],col="green")
#abline(v=mod5.med[i],col="orange")
#abline(v=mod6.med[i],col="pink")

#abline(v=obs.q75[i])
#abline(v=mod1.q75[i],col="red")
#abline(v=mod2.q75[i],col="blue")
#abline(v=mod3.q75[i],col="yellow")
#abline(v=mod4.q75[i],col="green")
#abline(v=mod5.q75[i],col="orange")
#abline(v=mod6.q75[i],col="pink")

#abline(v=obs.q90[i])
#abline(v=mod1.q90[i],col="red")
#abline(v=mod2.q90[i],col="blue")
#abline(v=mod3.q90[i],col="yellow")
#abline(v=mod4.q90[i],col="green")
#abline(v=mod5.q90[i],col="orange")
#abline(v=mod6.q90[i],col="pink")

#########################################################################

#Return levels
y <- -log(1-(1/5))
obs.M5 <- loc - ((scale/shape)*(1-(y^(-shape))))
mod1.M5 <- mod1.loc - ((mod1.scale/shape)*(1-(y^(-shape))))
mod2.M5 <- mod2.loc - ((mod2.scale/shape)*(1-(y^(-shape))))
mod3.M5 <- mod3.loc - ((mod3.scale/shape)*(1-(y^(-shape))))
mod4.M5 <- mod4.loc - ((mod4.scale/shape)*(1-(y^(-shape))))
mod5.M5 <- mod5.loc - ((mod5.scale/shape)*(1-(y^(-shape))))
mod6.M5 <- mod6.loc - ((mod6.scale/shape)*(1-(y^(-shape))))

y <- -log(1-(1/10))
obs.M10 <- loc - ((scale/shape)*(1-(y^(-shape))))
mod1.M10 <- mod1.loc - ((mod1.scale/shape)*(1-(y^(-shape))))
mod2.M10 <- mod2.loc - ((mod2.scale/shape)*(1-(y^(-shape))))
mod3.M10 <- mod3.loc - ((mod3.scale/shape)*(1-(y^(-shape))))
mod4.M10 <- mod4.loc - ((mod4.scale/shape)*(1-(y^(-shape))))
mod5.M10 <- mod5.loc - ((mod5.scale/shape)*(1-(y^(-shape))))
mod6.M10 <- mod6.loc - ((mod6.scale/shape)*(1-(y^(-shape))))
y <- -log(1-(1/25))
obs.M25 <- loc - ((scale/shape)*(1-(y^(-shape))))
mod1.M25 <- mod1.loc - ((mod1.scale/shape)*(1-(y^(-shape))))
mod2.M25 <- mod2.loc - ((mod2.scale/shape)*(1-(y^(-shape))))
mod3.M25 <- mod3.loc - ((mod3.scale/shape)*(1-(y^(-shape))))
mod4.M25 <- mod4.loc - ((mod4.scale/shape)*(1-(y^(-shape))))
mod5.M25 <- mod5.loc - ((mod5.scale/shape)*(1-(y^(-shape))))
mod6.M25 <- mod6.loc - ((mod6.scale/shape)*(1-(y^(-shape))))
y <- -log(1-(1/50))
obs.M50 <- loc - ((scale/shape)*(1-(y^(-shape))))
mod1.M50 <- mod1.loc - ((mod1.scale/shape)*(1-(y^(-shape))))
mod2.M50 <- mod2.loc - ((mod2.scale/shape)*(1-(y^(-shape))))
mod3.M50 <- mod3.loc - ((mod3.scale/shape)*(1-(y^(-shape))))
mod4.M50 <- mod4.loc - ((mod4.scale/shape)*(1-(y^(-shape))))
mod5.M50 <- mod5.loc - ((mod5.scale/shape)*(1-(y^(-shape))))
mod6.M50 <- mod6.loc - ((mod6.scale/shape)*(1-(y^(-shape))))


#retper <- c(5,10,25,50)
#i=32
#Plot return levels
#plot(log(retper), c(mod6.M5[i],mod6.M20[i],mod6.M50[i],mod6.M100[i],mod6.M200[i]))
#points(log(retper),c(obs.M5[i],obs.M20[i],obs.M50[i],obs.M100[i],obs.M200[i]),col="red")
#points(log(-1/log((1:length(data.am[which(!is.na(data.am[,i+1])),i+1]))/(length(data.am[which(!is.na(data.am[,i+1])),i+1])+1))),sort(data.am[which(!is.na(data.am[,i+1])),i+1]),col="gray60",pch=19)

##################################################################################
#Compute MAE for quantiles and return levels + IQd
##################################################################################

MSE.mean.mod1 <- 0
MSE.mean.mod2 <- 0
MSE.mean.mod3 <- 0
MSE.mean.mod4 <- 0
MSE.mean.mod5 <- 0
MSE.mean.mod6 <- 0
MSE.var.mod1 <- 0
MSE.var.mod2 <- 0
MSE.var.mod3 <- 0
MSE.var.mod4 <- 0
MSE.var.mod5 <- 0
MSE.var.mod6 <- 0
MSE.2mom.mod1 <- 0
MSE.2mom.mod2 <- 0
MSE.2mom.mod3 <- 0
MSE.2mom.mod4 <- 0
MSE.2mom.mod5 <- 0
MSE.2mom.mod6 <- 0

MAE.med.mod1 <- 0
MAE.med.mod2 <- 0
MAE.med.mod3 <- 0
MAE.med.mod4 <- 0
MAE.med.mod5 <- 0
MAE.med.mod6 <- 0
MAE.q75.mod1 <- 0
MAE.q75.mod2 <- 0
MAE.q75.mod3 <- 0
MAE.q75.mod4 <- 0
MAE.q75.mod5 <- 0
MAE.q75.mod6 <- 0
MAE.q90.mod1 <- 0
MAE.q90.mod2 <- 0
MAE.q90.mod3 <- 0
MAE.q90.mod4 <- 0
MAE.q90.mod5 <- 0
MAE.q90.mod6 <- 0

MAE.M5.mod1 <- 0
MAE.M10.mod1 <- 0
MAE.M25.mod1 <- 0
MAE.M50.mod1 <- 0
MAE.M5.mod2 <- 0
MAE.M10.mod2 <- 0
MAE.M25.mod2 <- 0
MAE.M50.mod2 <- 0
MAE.M5.mod3 <- 0
MAE.M10.mod3 <- 0
MAE.M25.mod3 <- 0
MAE.M50.mod3 <- 0
MAE.M5.mod4 <- 0
MAE.M10.mod4 <- 0
MAE.M25.mod4 <- 0
MAE.M50.mod4 <- 0
MAE.M5.mod5 <- 0
MAE.M10.mod5 <- 0
MAE.M25.mod5 <- 0
MAE.M50.mod5 <- 0
MAE.M5.mod6 <- 0
MAE.M10.mod6 <- 0
MAE.M25.mod6 <- 0
MAE.M50.mod6 <- 0

#D.med.mod1 <- (mean(mod1.med) - mean(obs.med))^2
#D.med.mod2 <- (mean(mod2.med) - mean(obs.med))^2
#D.med.mod3 <- (mean(mod3.med) - mean(obs.med))^2
#D.med.mod4 <- (mean(mod4.med) - mean(obs.med))^2
#D.med.mod5 <- (mean(mod5.med) - mean(obs.med))^2
#D.med.mod6 <- (mean(mod6.med) - mean(obs.med))^2
#D.q75.mod1 <- (mean(mod1.q75) - mean(obs.q75))^2
#D.q75.mod2 <- (mean(mod2.q75) - mean(obs.q75))^2
#D.q75.mod3 <- (mean(mod3.q75) - mean(obs.q75))^2
#D.q75.mod4 <- (mean(mod4.q75) - mean(obs.q75))^2
#D.q75.mod5 <- (mean(mod5.q75) - mean(obs.q75))^2
#D.q75.mod6 <- (mean(mod6.q75) - mean(obs.q75))^2
#D.q90.mod1 <- (mean(mod1.q90) - mean(obs.q90))^2
#D.q90.mod2 <- (mean(mod2.q90) - mean(obs.q90))^2
#D.q90.mod3 <- (mean(mod3.q90) - mean(obs.q90))^2
#D.q90.mod4 <- (mean(mod4.q90) - mean(obs.q90))^2
#D.q90.mod5 <- (mean(mod5.q90) - mean(obs.q90))^2
#D.q90.mod6 <- (mean(mod6.q90) - mean(obs.q90))^2

#D2.med.mod1 <- ((mean(mod1.med))^2 - (mean(obs.med))^2)^2
#D2.med.mod2 <- ((mean(mod2.med))^2 - (mean(obs.med))^2)^2
#D2.med.mod3 <- ((mean(mod3.med))^2 - (mean(obs.med))^2)^2
#D2.med.mod4 <- ((mean(mod4.med))^2 - (mean(obs.med))^2)^2
#D2.med.mod5 <- ((mean(mod5.med))^2 - (mean(obs.med))^2)^2
#D2.med.mod6 <- ((mean(mod6.med))^2 - (mean(obs.med))^2)^2
#D2.q75.mod1 <- ((mean(mod1.q75))^2 - (mean(obs.q75))^2)^2
#D2.q75.mod2 <- ((mean(mod2.q75))^2 - (mean(obs.q75))^2)^2
#D2.q75.mod3 <- ((mean(mod3.q75))^2 - (mean(obs.q75))^2)^2
#D2.q75.mod4 <- ((mean(mod4.q75))^2 - (mean(obs.q75))^2)^2
#D2.q75.mod5 <- ((mean(mod5.q75))^2 - (mean(obs.q75))^2)^2
#D2.q75.mod6 <- ((mean(mod6.q75))^2 - (mean(obs.q75))^2)^2
#D2.q90.mod1 <- ((mean(mod1.q90))^2 - (mean(obs.q90))^2)^2
#D2.q90.mod2 <- ((mean(mod2.q90))^2 - (mean(obs.q90))^2)^2
#D2.q90.mod3 <- ((mean(mod3.q90))^2 - (mean(obs.q90))^2)^2
#D2.q90.mod4 <- ((mean(mod4.q90))^2 - (mean(obs.q90))^2)^2
#D2.q90.mod5 <- ((mean(mod5.q90))^2 - (mean(obs.q90))^2)^2
#D2.q90.mod6 <- ((mean(mod6.q90))^2 - (mean(obs.q90))^2)^2

#D.M5.mod1 <- (mean(mod1.M5) - mean(obs.M5))^2
#D.M5.mod2 <- (mean(mod2.M5) - mean(obs.M5))^2
#D.M5.mod3 <- (mean(mod3.M5) - mean(obs.M5))^2
#D.M5.mod4 <- (mean(mod4.M5) - mean(obs.M5))^2
#D.M5.mod5 <- (mean(mod5.M5) - mean(obs.M5))^2
#D.M5.mod6 <- (mean(mod6.M5) - mean(obs.M5))^2
#D.M10.mod1 <- (mean(mod1.M10) - mean(obs.M10))^2
#D.M10.mod2 <- (mean(mod2.M10) - mean(obs.M10))^2
#D.M10.mod3 <- (mean(mod3.M10) - mean(obs.M10))^2
#D.M10.mod4 <- (mean(mod4.M10) - mean(obs.M10))^2
#D.M10.mod5 <- (mean(mod5.M10) - mean(obs.M10))^2
#D.M10.mod6 <- (mean(mod6.M10) - mean(obs.M10))^2
#D.M25.mod1 <- (mean(mod1.M25) - mean(obs.M25))^2
#D.M25.mod2 <- (mean(mod2.M25) - mean(obs.M25))^2
#D.M25.mod3 <- (mean(mod3.M25) - mean(obs.M25))^2
#D.M25.mod4 <- (mean(mod4.M25) - mean(obs.M25))^2
#D.M25.mod5 <- (mean(mod5.M25) - mean(obs.M25))^2
#D.M25.mod6 <- (mean(mod6.M25) - mean(obs.M25))^2
#D.M50.mod1 <- (mean(mod1.M50) - mean(obs.M50))^2
#D.M50.mod2 <- (mean(mod2.M50) - mean(obs.M50))^2
#D.M50.mod3 <- (mean(mod3.M50) - mean(obs.M50))^2
#D.M50.mod4 <- (mean(mod4.M50) - mean(obs.M50))^2
#D.M50.mod5 <- (mean(mod5.M50) - mean(obs.M50))^2
#D.M50.mod6 <- (mean(mod6.M50) - mean(obs.M50))^2

#D2.M5.mod1 <- ((mean(mod1.M5))^2 - (mean(obs.M5))^2)^2
#D2.M5.mod2 <- ((mean(mod2.M5))^2 - (mean(obs.M5))^2)^2
#D2.M5.mod3 <- ((mean(mod3.M5))^2 - (mean(obs.M5))^2)^2
#D2.M5.mod4 <- ((mean(mod4.M5))^2 - (mean(obs.M5))^2)^2
#D2.M5.mod5 <- ((mean(mod5.M5))^2 - (mean(obs.M5))^2)^2
#D2.M5.mod6 <- ((mean(mod6.M5))^2 - (mean(obs.M5))^2)^2
#D2.M10.mod1 <- ((mean(mod1.M10))^2 - (mean(obs.M10))^2)^2
#D2.M10.mod2 <- ((mean(mod2.M10))^2 - (mean(obs.M10))^2)^2
#D2.M10.mod3 <- ((mean(mod3.M10))^2 - (mean(obs.M10))^2)^2
#D2.M10.mod4 <- ((mean(mod4.M10))^2 - (mean(obs.M10))^2)^2
#D2.M10.mod5 <- ((mean(mod5.M10))^2 - (mean(obs.M10))^2)^2
#D2.M10.mod6 <- ((mean(mod6.M10))^2 - (mean(obs.M10))^2)^2
#D2.M25.mod1 <- ((mean(mod1.M25))^2 - (mean(obs.M25))^2)^2
#D2.M25.mod2 <- ((mean(mod2.M25))^2 - (mean(obs.M25))^2)^2
#D2.M25.mod3 <- ((mean(mod3.M25))^2 - (mean(obs.M25))^2)^2
#D2.M25.mod4 <- ((mean(mod4.M25))^2 - (mean(obs.M25))^2)^2
#D2.M25.mod5 <- ((mean(mod5.M25))^2 - (mean(obs.M25))^2)^2
#D2.M25.mod6 <- ((mean(mod6.M25))^2 - (mean(obs.M25))^2)^2
#D2.M50.mod1 <- ((mean(mod1.M50))^2 - (mean(obs.M50))^2)^2
#D2.M50.mod2 <- ((mean(mod2.M50))^2 - (mean(obs.M50))^2)^2
#D2.M50.mod3 <- ((mean(mod3.M50))^2 - (mean(obs.M50))^2)^2
#D2.M50.mod4 <- ((mean(mod4.M50))^2 - (mean(obs.M50))^2)^2
#D2.M50.mod5 <- ((mean(mod5.M50))^2 - (mean(obs.M50))^2)^2
#D2.M50.mod6 <- ((mean(mod6.M50))^2 - (mean(obs.M50))^2)^2

for (j in 1:69) {

	MSE.mean.mod1 <- MSE.mean.mod1 + (obs.mean[j] - mod1.mean[j])^2	
	MSE.mean.mod2 <- MSE.mean.mod2 + (obs.mean[j] - mod2.mean[j])^2
	MSE.mean.mod3 <- MSE.mean.mod3 + (obs.mean[j] - mod3.mean[j])^2
	MSE.mean.mod4 <- MSE.mean.mod4 + (obs.mean[j] - mod4.mean[j])^2
	MSE.mean.mod5 <- MSE.mean.mod5 + (obs.mean[j] - mod5.mean[j])^2
	MSE.mean.mod6 <- MSE.mean.mod6 + (obs.mean[j] - mod6.mean[j])^2
	MSE.var.mod1 <- MSE.var.mod1 + (obs.var[j] - mod1.var[j])^2
	MSE.var.mod2 <- MSE.var.mod2 + (obs.var[j] - mod2.var[j])^2
	MSE.var.mod3 <- MSE.var.mod3 + (obs.var[j] - mod3.var[j])^2
	MSE.var.mod4 <- MSE.var.mod4 + (obs.var[j] - mod4.var[j])^2
	MSE.var.mod5 <- MSE.var.mod5 + (obs.var[j] - mod5.var[j])^2
	MSE.var.mod6 <- MSE.var.mod6 + (obs.var[j] - mod6.var[j])^2
	MSE.2mom.mod1 <- MSE.2mom.mod1 + (obs.2mom[j] - mod1.2mom[j])^2
	MSE.2mom.mod2 <- MSE.2mom.mod2 + (obs.2mom[j] - mod2.2mom[j])^2
	MSE.2mom.mod3 <- MSE.2mom.mod3 + (obs.2mom[j] - mod3.2mom[j])^2
	MSE.2mom.mod4 <- MSE.2mom.mod4 + (obs.2mom[j] - mod4.2mom[j])^2
	MSE.2mom.mod5 <- MSE.2mom.mod5 + (obs.2mom[j] - mod5.2mom[j])^2
	MSE.2mom.mod6 <- MSE.2mom.mod6 + (obs.2mom[j] - mod6.2mom[j])^2
	
	MAE.med.mod1 <- MAE.med.mod1 + abs(obs.med[j] - mod1.med[j])  
	MAE.med.mod2 <- MAE.med.mod2 + abs(obs.med[j] - mod2.med[j])  
	MAE.med.mod3 <- MAE.med.mod3 + abs(obs.med[j] - mod3.med[j]) 
	MAE.med.mod4 <- MAE.med.mod4 + abs(obs.med[j] - mod4.med[j]) 
	MAE.med.mod5 <- MAE.med.mod5 + abs(obs.med[j] - mod5.med[j])  	
	MAE.med.mod6 <- MAE.med.mod6 + abs(obs.med[j] - mod6.med[j])  
	MAE.q75.mod1 <- MAE.q75.mod1 + abs(obs.q75[j] - mod1.q75[j]) 
	MAE.q75.mod2 <- MAE.q75.mod2 + abs(obs.q75[j] - mod2.q75[j])  
	MAE.q75.mod3 <- MAE.q75.mod3 + abs(obs.q75[j] - mod3.q75[j]) 
	MAE.q75.mod4 <- MAE.q75.mod4 + abs(obs.q75[j] - mod4.q75[j])
	MAE.q75.mod5 <- MAE.q75.mod5 + abs(obs.q75[j] - mod5.q75[j]) 
	MAE.q75.mod6 <- MAE.q75.mod6 + abs(obs.q75[j] - mod6.q75[j]) 
	MAE.q90.mod1 <- MAE.q90.mod1 + abs(obs.q90[j] - mod1.q90[j]) 
	MAE.q90.mod2 <- MAE.q90.mod2 + abs(obs.q90[j] - mod2.q90[j])   
	MAE.q90.mod3 <- MAE.q90.mod3 + abs(obs.q90[j] - mod3.q90[j]) 
	MAE.q90.mod4 <- MAE.q90.mod4 + abs(obs.q90[j] - mod4.q90[j])
	MAE.q90.mod5 <- MAE.q90.mod5 + abs(obs.q90[j] - mod5.q90[j])  
	MAE.q90.mod6 <- MAE.q90.mod6 + abs(obs.q90[j] - mod6.q90[j])  

	MAE.M5.mod1 <- MAE.M5.mod1 + abs(obs.M5[j] - mod1.M5[j])
	MAE.M10.mod1 <- MAE.M10.mod1 + abs(obs.M10[j] - mod1.M10[j])
	MAE.M25.mod1 <- MAE.M25.mod1 + abs(obs.M25[j] - mod1.M25[j])
	MAE.M50.mod1 <- MAE.M50.mod1 + abs(obs.M50[j] - mod1.M50[j])

	MAE.M5.mod2 <- MAE.M5.mod2 + abs(obs.M5[j] - mod2.M5[j])
	MAE.M10.mod2 <- MAE.M10.mod2 + abs(obs.M10[j] - mod2.M10[j])
	MAE.M25.mod2 <- MAE.M25.mod2 + abs(obs.M25[j] - mod2.M25[j])
	MAE.M50.mod2 <- MAE.M50.mod2 + abs(obs.M50[j] - mod2.M50[j])

	MAE.M5.mod3 <- MAE.M5.mod3 + abs(obs.M5[j] - mod3.M5[j])
	MAE.M10.mod3 <- MAE.M10.mod3 + abs(obs.M10[j] - mod3.M10[j])
	MAE.M25.mod3 <- MAE.M25.mod3 + abs(obs.M25[j] - mod3.M25[j])
	MAE.M50.mod3 <- MAE.M50.mod3 + abs(obs.M50[j] - mod3.M50[j])

	MAE.M5.mod4 <- MAE.M5.mod4 + abs(obs.M5[j] - mod4.M5[j])
	MAE.M10.mod4 <- MAE.M10.mod4 + abs(obs.M10[j] - mod4.M10[j])
	MAE.M25.mod4 <- MAE.M25.mod4 + abs(obs.M25[j] - mod4.M25[j])
	MAE.M50.mod4 <- MAE.M50.mod4 + abs(obs.M50[j] - mod4.M50[j])

	MAE.M5.mod5 <- MAE.M5.mod5 + abs(obs.M5[j] - mod5.M5[j])
	MAE.M10.mod5 <- MAE.M10.mod5 + abs(obs.M10[j] - mod5.M10[j])
	MAE.M25.mod5 <- MAE.M25.mod5 + abs(obs.M25[j] - mod5.M25[j])
	MAE.M50.mod5 <- MAE.M50.mod5 + abs(obs.M50[j] - mod5.M50[j])

	MAE.M5.mod6 <- MAE.M5.mod6 + abs(obs.M5[j] - mod6.M5[j])
	MAE.M10.mod6 <- MAE.M10.mod6 + abs(obs.M10[j] - mod6.M10[j])
	MAE.M25.mod6 <- MAE.M25.mod6 + abs(obs.M25[j] - mod6.M25[j])
	MAE.M50.mod6 <- MAE.M50.mod6 + abs(obs.M50[j] - mod6.M50[j])

#low <- 0

	#Integrated quadratic distance
#	integrand1 <- function(x) {(exp(-(1+0.1*((x-loc[j])/scale[j]))^(-1/0.1)) - exp(-(1+0.1*((x-mod1.loc[j])/mod1.scale[j]))^(-1/0.1)))^2}
#	IQd.mod1 <- IQd.mod1 + integrate(integrand1, lower=low, upper=Inf,subdivisions=100000)$value
#	integrand2 <- function(x) {(exp(-(1+0.1*((x-loc[j])/scale[j]))^(-1/0.1)) - exp(-(1+0.1*((x-mod2.loc[j])/mod2.scale[j]))^(-1/0.1)))^2}
#	IQd.mod2 <- IQd.mod1 + integrate(integrand2, lower=low, upper=Inf,subdivisions=100000)$value
#	integrand3 <- function(x) {(exp(-(1+0.1*((x-loc[j])/scale[j]))^(-1/0.1)) - exp(-(1+0.1*((x-mod3.loc[j])/mod3.scale[j]))^(-1/0.1)))^2}
#	IQd.mod3 <- IQd.mod1 + integrate(integrand3, lower=low, upper=Inf,subdivisions=100000)$value
#	integrand4 <- function(x) {(exp(-(1+0.1*((x-loc[j])/scale[j]))^(-1/0.1)) - exp(-(1+0.1*((x-mod4.loc[j])/mod4.scale[j]))^(-1/0.1)))^2}
#	IQd.mod4 <- IQd.mod1 + integrate(integrand4, lower=low, upper=Inf,subdivisions=100000)$value
#	integrand5 <- function(x) {(exp(-(1+0.1*((x-loc[j])/scale[j]))^(-1/0.1)) - exp(-(1+0.1*((x-mod5.loc[j])/mod5.scale[j]))^(-1/0.1)))^2}
#	IQd.mod5 <- IQd.mod1 + integrate(integrand5, lower=low, upper=Inf,subdivisions=100000)$value
#	integrand6 <- function(x) {(exp(-(1+0.1*((x-loc[j])/scale[j]))^(-1/0.1)) - exp(-(1+0.1*((x-mod6.loc[j])/mod6.scale[j]))^(-1/0.1)))^2}
#	IQd.mod6 <- IQd.mod1 + integrate(integrand6, lower=low, upper=Inf,subdivisions=100000)$value
#
}

a <- cbind(round(MSE.mean.mod1/69,2),round(MSE.mean.mod2/69,2),round(MSE.mean.mod3/69,2),round(MSE.mean.mod4/69,2),round(MSE.mean.mod5/69,2),round(MSE.mean.mod6/69,2))
b <- cbind(round(MSE.2mom.mod1/69,2),round(MSE.2mom.mod2/69,2),round(MSE.2mom.mod3/69,2),round(MSE.2mom.mod4/69,2),round(MSE.2mom.mod5/69,2),round(MSE.2mom.mod6/69,2))
c <- cbind(round(MAE.med.mod1/69,2),round(MAE.med.mod2/69,2),round(MAE.med.mod3/69,2),round(MAE.med.mod4/69,2),round(MAE.med.mod5/69,2),round(MAE.med.mod6/69,2))
d <- cbind(round(MAE.q75.mod1/69,2),round(MAE.q75.mod2/69,2),round(MAE.q75.mod3/69,2),round(MAE.q75.mod4/69,2),round(MAE.q75.mod5/69,2),round(MAE.q75.mod6/69,2))
e <- cbind(round(MAE.q90.mod1/69,2),round(MAE.q90.mod2/69,2),round(MAE.q90.mod3/69,2),round(MAE.q90.mod4/69,2),round(MAE.q90.mod5/69,2),round(MAE.q90.mod6/69,2))
f <- cbind(round(MAE.M5.mod1/69,2),round(MAE.M5.mod2/69,2),round(MAE.M5.mod3/69,2),round(MAE.M5.mod4/69,2),round(MAE.M5.mod5/69,2),round(MAE.M5.mod6/69,2))
g <- cbind(round(MAE.M10.mod1/69,2),round(MAE.M10.mod2/69,2),round(MAE.M10.mod3/69,2),round(MAE.M10.mod4/69,2),round(MAE.M10.mod5/69,2),round(MAE.M10.mod6/69,2))
h <- cbind(round(MAE.M25.mod1/69,2),round(MAE.M25.mod2/69,2),round(MAE.M25.mod3/69,2),round(MAE.M25.mod4/69,2),round(MAE.M25.mod5/69,2),round(MAE.M25.mod6/69,2))
i <- cbind(round(MAE.M50.mod1/69,2),round(MAE.M50.mod2/69,2),round(MAE.M50.mod3/69,2),round(MAE.M50.mod4/69,2),round(MAE.M50.mod5/69,2),round(MAE.M50.mod6/69,2))

results <- rbind(a,b,c,d,e,f,g,h,i)
colnames(results) <- c("mod1","mod2","mod3","mod4","mod5","mod6")   #mod2 = mod3, mod3=mod4, mod5=mod2. 3 first models is with 2-cov sigma-model
rownames(results) <- c("MSE.mean","MSE.var","MAE.med","MAE.q75","MAE.q90","MAE.M5","MAE.M10","MAE.M25","MAE.M50")
return(results)




