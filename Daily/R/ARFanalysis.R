##################################################
#
# Script that computes the areal intensity with increasing area.
# region: East, West, Mid, North
# threshold: ~2 year return value for the region
#
# Anita Verpe Dyrrdal, met.no, feb2012
#
##################################################



extractEventsExM2 <- function(region,start.year,end.year,threshold) {

pathtofiles <- "/p/PhD/Research/Comparison_methods/ARFanalysis/"

#ID of grid cells in the region
ID <- read.table(paste(pathtofiles,region,"/tabID.txt",sep=""), header=FALSE, dec=",")
colnames(ID) <- c("X","Y","ID")
regionID <- ID$ID + 1   #ArcMap starts at 0, while R starts at 1

size.region <- length(regionID)
length.region <- sqrt(size.region)

ID <- read.table(paste(pathtofiles,region,"/Center/tabID.txt",sep=""), header=FALSE, dec=",")
colnames(ID) <- c("X","Y","ID")
centerID <- ID$ID + 1   #ArcMap starts at 0, while R starts at 1

size.center <- length(centerID)
length.center <- sqrt(size.center)

#Vector with number of days in the months
nodays.vector <- c(31,28,31,30,31,30,31,31,30,31,30,31)
idx.max = NULL
offset <- c(0,1,-1,length.region,-length.region,length.region+1,-(length.region+1),length.region-1,-(length.region-1))
rr.area.matrix <- NULL

#Number of cells on each side of the rectangles
#a <- c(7,15,21,27,33,41)
a <- c(20,16,13,10,7,3)
#b <- c(17,29,45,63,79,91)
b <- c(45,39,31,22,14,8)

for (year in start.year:end.year) {

	for (month in 1:12) {

		nodays <- nodays.vector[month]

		if (month == 2) {
		if(is.integer(((year/4)-1):(year/4))) nodays <-29
		}

		for (day in 1:nodays) {

			#Read precipitation grid
			filename=sprintf("/klimagrid/daily/rr_uncorrected/binary/%4i/%2.2i/rr_%4i_%2.2i_%2.2i.bil",year,month,year,month,day)
			print(filename)
			con=file(filename,open="rb")
			rr.grid<- readBin(con, integer(), size=2, n=1550*1195)
			close(con)
			rr.center <- rr.grid[centerID]
			rr.region <- rr.grid[regionID]
			rm(rr.grid)
			gc(reset=TRUE)
			rr.center[rr.center == 10000] = NA
			rr.center <- rr.center/10    # [mm]
			rr.region[rr.region == 10000] = NA
			rr.region <- rr.region/10    # [mm]
	
			max.rr <- max(rr.center,na.rm=T)
			
			#Select storms with max within the center region and an intensity above/equal to 2-year return level - 10%. 
			if(max.rr == max(rr.region,na.rm=T) & max.rr >= (threshold-(threshold*0.1))) {
				idx.max <- cbind(idx.max,which(rr.region == max.rr)[1])
				rr.area <- max.rr
				n <- 1
				shape <- 0
				for (j in c(30,25,20,15,10,5)) {
					idx.square <- NULL
					idx.recv <- NULL
					idx.rech <- NULL

					#Square area around max
					if(j==30 | shape==1) {
					idx.square <- cbind(idx.square,seq(idx.max-j,idx.max+j))
					for (k in 1:j) {
						idx.square <- cbind(idx.square,seq(idx.max-(k*length.region)-j,idx.max-(k*length.region)+j))
						idx.square <- cbind(idx.square,seq(idx.max+(k*length.region)-j,idx.max+(k*length.region)+j))
					}
					idx <- idx.square
					}
		
					#Vertical rectangle around max
					if(j==30 | shape==2) {
					idx.recv <- cbind(idx.recv,seq(idx.max-a[n],idx.max+a[n]))
					for (k in 1:b[n]) {
						idx.recv <- cbind(idx.recv,seq(idx.max-(k*length.region)-a[n],idx.max-(k*length.region)+a[n]))
						idx.recv <- cbind(idx.recv,seq(idx.max+(k*length.region)-a[n],idx.max+(k*length.region)+a[n]))
					}
					idx <- idx.recv
					}

					#Horizontal rectangle around max
					if(j==30 | shape==3) {					
					idx.rech <- cbind(idx.rech,seq(idx.max-b[n],idx.max+b[n]))
					for (k in 1:a[n]) {
						idx.rech <- cbind(idx.rech,seq(idx.max-(k*length.region)-b[n],idx.max-(k*length.region)+b[n]))
						idx.rech <- cbind(idx.rech,seq(idx.max+(k*length.region)-b[n],idx.max+(k*length.region)+b[n]))
					}
					idx <- idx.rech
					}

					#Optimized/maximized area mean precipitation
					if(j==30) { 
						rr.area.max <- max(mean(rr.region[idx.square],na.rm=T),mean(rr.region[idx.recv],na.rm=T),mean(rr.region[idx.rech],na.rm=T))
						if(rr.area.max == mean(rr.region[idx.square],na.rm=T)) shape <- 1
						if(rr.area.max == mean(rr.region[idx.recv],na.rm=T)) shape <- 2
						if(rr.area.max == mean(rr.region[idx.rech],na.rm=T)) shape <- 3
					} else {
						rr.area.max <- mean(rr.region[idx],na.rm=T)
					}

					rr.area <- c(rr.area,rr.area.max)
					n <- n + 1	
				}
				rr.area.matrix <- cbind(rr.area.matrix,rr.area)
			}
		}
	}
}

if(length(ncol(rr.area.matrix))==0 | !is.numeric(rr.area.matrix)) break

ARF <- matrix(NA,ncol(rr.area.matrix),nrow(rr.area.matrix)+1)
ARF[,1] <- rr.area.matrix[1,]
ARF[,2] <- rr.area.matrix[1,]/rr.area.matrix[1,]

for (i in ncol(ARF):3) {
	ARF[,i] <- rr.area.matrix[ncol(ARF)-i+2,]/rr.area.matrix[1,]
}

ARF <- round(ARF,digits=3)

write.table(ARF, paste("/p/PhD/Research/Comparison_methods/ARFanalysis/",region,"/Results/ARF_",region,".txt",sep=""),quote=F,row.names=F,sep="\t")

print(paste("File ARF_",region,".txt written",sep=""))

return(ARF)

}

#Extract events with a certain return period

extractEventsMX <- function(region,start.year,end.year,RV) {

pathtofiles <- "/p/PhD/Research/Comparison_methods/ARFanalysis/"

#ID of grid cells in the region
ID <- read.table(paste(pathtofiles,region,"/tabID.txt",sep=""), header=FALSE, dec=",")
colnames(ID) <- c("X","Y","ID")
regionID <- ID$ID + 1   #ArcMap starts at 0, while R starts at 1

size.region <- length(regionID)
length.region <- sqrt(size.region)

ID <- read.table(paste(pathtofiles,region,"/Center/tabID.txt",sep=""), header=FALSE, dec=",")
colnames(ID) <- c("X","Y","ID")
centerID <- ID$ID + 1   #ArcMap starts at 0, while R starts at 1

size.center <- length(centerID)
length.center <- sqrt(size.center)

#Vector with number of days in the months
nodays.vector <- c(31,28,31,30,31,30,31,31,30,31,30,31)
region.idx = NULL
offset <- c(0,1,-1,length.region,-length.region,length.region+1,-(length.region+1),length.region-1,-(length.region-1))
rr.area.matrix <- NULL

#Number of cells on each side of the rectangles
#a <- c(7,15,21,27,33,41)
a <- c(20,16,13,10,7,3)
#b <- c(17,29,45,63,79,91)
b <- c(45,39,31,22,14,8)

for (year in start.year:end.year) {

	for (month in 1:12) {

		nodays <- nodays.vector[month]

		if (month == 2) {
		if(is.integer(((year/4)-1):(year/4))) nodays <-29
		}

		for (day in 1:nodays) {

			#Read precipitation grid
			filename=sprintf("/klimagrid/daily/rr_uncorrected/binary/%4i/%2.2i/rr_%4i_%2.2i_%2.2i.bil",year,month,year,month,day)
			print(filename)
			con=file(filename,open="rb")
			rr.grid<- readBin(con, integer(), size=2, n=1550*1195)
			close(con)
			rr.center <- rr.grid[centerID]
			rr.region <- rr.grid[regionID]
			rm(rr.grid)
			gc(reset=TRUE)
			rr.center[rr.center == 10000] = NA
			rr.center <- rr.center/10    # [mm]
			rr.region[rr.region == 10000] = NA
			rr.region <- rr.region/10    # [mm]
	
			events <- which(rr.center > RV-1 & rr.center < RV+1)
			
			#Select storms with an intensity (RV) corresponding to the return period in question 
			if(length(events)>0) {

				max.rr.idx <- events[abs(rr.center[events]-RV)==min(abs(rr.center[events]-RV))][1]
				center.idx <- centerID[max.rr.idx]				
				max.rr <- rr.center[max.rr.idx]
				region.idx <- cbind(region.idx,which(regionID==center.idx)[1])

				rr.area <- max.rr
				n <- 1
				shape <- 0
				for (j in c(30,25,20,15,10,5)) {
					idx.square <- NULL
					idx.recv <- NULL
					idx.rech <- NULL

					#Square area around max
					if(j==30 | shape==1) {
					idx.square <- cbind(idx.square,seq(region.idx-j,region.idx+j))
					for (k in 1:j) {
						idx.square <- cbind(idx.square,seq(region.idx-(k*length.region)-j,region.idx-(k*length.region)+j))
						idx.square <- cbind(idx.square,seq(region.idx+(k*length.region)-j,region.idx+(k*length.region)+j))
					}
					idx <- idx.square
					}
		
					#Vertical rectangle around max
					if(j==30 | shape==2) {
					idx.recv <- cbind(idx.recv,seq(region.idx-a[n],region.idx+a[n]))
					for (k in 1:b[n]) {
						idx.recv <- cbind(idx.recv,seq(region.idx-(k*length.region)-a[n],region.idx-(k*length.region)+a[n]))
						idx.recv <- cbind(idx.recv,seq(region.idx+(k*length.region)-a[n],region.idx+(k*length.region)+a[n]))
					}
					idx <- idx.recv
					}

					#Horizontal rectangle around max
					if(j==30 | shape==3) {					
					idx.rech <- cbind(idx.rech,seq(region.idx-b[n],region.idx+b[n]))
					for (k in 1:a[n]) {
						idx.rech <- cbind(idx.rech,seq(region.idx-(k*length.region)-b[n],region.idx-(k*length.region)+b[n]))
						idx.rech <- cbind(idx.rech,seq(region.idx+(k*length.region)-b[n],region.idx+(k*length.region)+b[n]))
					}
					idx <- idx.rech
					}

					#Optimized/maximized area mean precipitation
					if(j==30) { 
						rr.area.max <- max(mean(rr.region[idx.square],na.rm=T),mean(rr.region[idx.recv],na.rm=T),mean(rr.region[idx.rech],na.rm=T))
						if(rr.area.max == mean(rr.region[idx.square],na.rm=T)) shape <- 1
						if(rr.area.max == mean(rr.region[idx.recv],na.rm=T)) shape <- 2
						if(rr.area.max == mean(rr.region[idx.rech],na.rm=T)) shape <- 3
					} else {
						rr.area.max <- mean(rr.region[idx],na.rm=T)
					}

					rr.area <- c(rr.area,rr.area.max)
					n <- n + 1

				}
				rr.area.matrix <- cbind(rr.area.matrix,rr.area)
			}
		}
	}
}

if(length(ncol(rr.area.matrix))==0 | !is.numeric(rr.area.matrix)) break

ARF <- matrix(NA,ncol(rr.area.matrix),nrow(rr.area.matrix)+1)
ARF[,1] <- rr.area.matrix[1,]
ARF[,2] <- rr.area.matrix[1,]/rr.area.matrix[1,]

for (i in ncol(ARF):3) {
	ARF[,i] <- rr.area.matrix[ncol(ARF)-i+2,]/rr.area.matrix[1,]
}

ARF <- round(ARF,digits=3)

write.table(ARF, paste("/p/PhD/Research/Comparison_methods/ARFanalysis/",region,"/Results/ARF_",RV,"_",region,".txt",sep=""),quote=F,row.names=F,sep="\t")

print(paste("File ARF_",RV,"_",region,".txt written",sep=""))

return(ARF)

}

#Extract extreme value distribution (annual max) from differently sized areas within the region
extractEVD <- function(region) {

if (region=="East") id <- 284136
no.years <- 2010-1957+1

nodays.vector <- c(31,28,31,30,31,30,31,31,30,31,30,31)
rr.8 <- list(Year=NULL,rr=NULL)
rr.16 <- list(Year=NULL,rr=NULL)
rr.24 <- list(Year=NULL,rr=NULL)
rr.32 <- list(Year=NULL,rr=NULL)
rr.40 <- list(Year=NULL,rr=NULL)
rr.48 <- list(Year=NULL,rr=NULL)
rr.56 <- list(Year=NULL,rr=NULL)
rr.64 <- list(Year=NULL,rr=NULL)
rr.72 <- list(Year=NULL,rr=NULL)

outfile <- paste("/p/PhD/Research/Comparison_methods/ARFanalysis/",region,"/AM/rr_AM.txt",sep="")
unlink(outfile)



for (year in 1957:2010) {

	for (month in 1:12) {

		nodays <- nodays.vector[month]

		if (month == 2) {
		if(is.integer(((year/4)-1):(year/4))) nodays <-29
		}

		for (day in 1:nodays) {

mean.rr<-c()

			#Read precipitation grid
			filename=sprintf("/klimagrid/daily/rr_uncorrected/binary/%4i/%2.2i/rr_%4i_%2.2i_%2.2i.bil",year,month,year,month,day)
			print(filename)
			con=file(filename,open="rb")
			rr.grid<- readBin(con, integer(), size=2, n=1550*1195)
			close(con)
			rr.grid[rr.grid == 10000] = NA
			rr.grid <- rr.grid/10    # [mm]

			for (j in c(8,16,24,32,40,48,56,64,72)) {

				idx <- c()

				idx <- cbind(idx,seq(id-j,id+j))
				for (k in 1:j) {
					idx <- cbind(idx,seq(id-(k*1195)-j,id-(k*1195)+j))
					idx <- cbind(idx,seq(id+(k*1195)-j,id+(k*1195)+j))
				}
				mean.rr <- cbind(mean.rr,round(mean(rr.grid[idx],na.rm=T),digits=1))
				#if (mean.rr>eval(parse(text=(as.character(paste("max.",j,sep="")))))) assign(paste("rr.",j,sep=""),cbind(eval(parse(text=(as.character(paste("rr.",j,sep=""))))),mean.rr))

#if(j==8) rr.8$Year <- rbind(rr.8$Year,year)
#if(j==16) rr.16$Year <- rbind(rr.16$Year,year)
#if(j==24) rr.24$Year<- rbind(rr.24$Year,year)
#if(j==32) rr.32$Year<- rbind(rr.32$Year,year)
#if(j==40) rr.40$Year<- rbind(rr.40$Year,year)
#if(j==48) rr.48$Year<- rbind(rr.48$Year,year)
#if(j==56) rr.56$Year<- rbind(rr.56$Year,year)
#if(j==64) rr.64$Year<- rbind(rr.64$Year,year)
#if(j==72) rr.72$Year<- rbind(rr.72$Year,year)

#if(j==8) rr.8$rr <- rbind(rr.8$rr,mean.rr)
#if(j==16) rr.16$rr <- rbind(rr.16$rr,mean.rr)
#if(j==24) rr.24$rr <- rbind(rr.24$rr,mean.rr)
#if(j==32) rr.32$rr <- rbind(rr.32$rr,mean.rr)
#if(j==40) rr.40$rr <- rbind(rr.40$rr,mean.rr)
#if(j==48) rr.48$rr <- rbind(rr.48$rr,mean.rr)
#if(j==56) rr.56$rr <- rbind(rr.56$rr,mean.rr)
#if(j==64) rr.64$rr <- rbind(rr.64$rr,mean.rr)
#if(j==72) rr.72$rr <- rbind(rr.72$rr,mean.rr)


			}
zz <- file(outfile, "a")
cat(paste(mean.rr),"\n",file=zz,append=TRUE)

		}
	}
}

#write.table(rr.1, paste("/p/PhD/Research/Comparison_methods/ARFanalysis/",region,"/AM/rr1_AM.txt",sep=""),row.names=F,quote=F)
#write.table(rr.8, paste("/p/PhD/Research/Comparison_methods/ARFanalysis/",region,"/AM/rr8_AM.txt",sep=""),row.names=F,quote=F)
#write.table(rr.16, paste("/p/PhD/Research/Comparison_methods/ARFanalysis/",region,"/AM/rr16_AM.txt",sep=""),row.names=F,quote=F)
#write.table(rr.24, paste("/p/PhD/Research/Comparison_methods/ARFanalysis/",region,"/AM/rr24_AM.txt",sep=""),row.names=F,quote=F)
#write.table(rr.32, paste("/p/PhD/Research/Comparison_methods/ARFanalysis/",region,"/AM/rr32_AM.txt",sep=""),row.names=F,quote=F)
#write.table(rr.40, paste("/p/PhD/Research/Comparison_methods/ARFanalysis/",region,"/AM/rr40_AM.txt",sep=""),row.names=F,quote=F)
#write.table(rr.48, paste("/p/PhD/Research/Comparison_methods/ARFanalysis/",region,"/AM/rr48_AM.txt",sep=""),row.names=F,quote=F)
#write.table(rr.56, paste("/p/PhD/Research/Comparison_methods/ARFanalysis/",region,"/AM/rr56_AM.txt",sep=""),row.names=F,quote=F)
#write.table(rr.64, paste("/p/PhD/Research/Comparison_methods/ARFanalysis/",region,"/AM/rr64_AM.txt",sep=""),row.names=F,quote=F)
#write.table(rr.72, paste("/p/PhD/Research/Comparison_methods/ARFanalysis/",region,"/AM/rr72_AM.txt",sep=""),row.names=F,quote=F)

}












