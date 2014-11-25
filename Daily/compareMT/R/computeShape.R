# Script that computes the shape parameter of GEV for each grid cell of the uncorrected RR grid

library(ismev)
library(minpack.lm)
source("/vol/klimadata/applikasjon/gridding/src/iobin.R")
source("/vol/klimadata/applikasjon/gridding/src/met_stat.R")

computeShape <-function(startYear,endYear) {

noYears <- endYear - startYear + 1

ncol <- 1195
nrow <- 1550

#Read file with indexes for non-NA values (new version)
#TabID <- as.numeric(as.matrix(read.table("/home/anitavd/scripts/TabID.txt",header=TRUE)))

year.idx <- 1

#Read annual max files for all years
for (year in startYear:endYear) {
	
	filename=sprintf("/home/anitavd/PhD/Daily/annualMax/rr_uncor_anMax_%4i.bil",year)
	con=file(filename,open="rb")
	maxgrid=readBin(con,integer(),size=2,n=ncol*nrow)
    	maxgrid[maxgrid>=10000] <- NA
	maxgrid[maxgrid==-1] <- NA
	if(year==1957) {
		TabID <- which(!is.na(maxgrid))
		yearMax <- array(NA,dim=c(length(TabID),noYears))
	}
  	yearMax[,year.idx] <- maxgrid[TabID]/10   #mm
	
	close(con)
	rm(maxgrid)
	gc(reset=TRUE)

	year.idx <- year.idx + 1

} #end year-loop

shape <- array(NA,dim=(length(TabID)))

for (i in 1:length(TabID)) {
	if(is.na(mean(yearMax[i,]))) {
		shape[i] = NA
	}else {
		#shape[i] <- (gev.fit(yearMax[i,]*1.13,show=F)$mle[3] + 5)*100   #to get decimals and avoid negative numbers
		#WLS		
		x <- yearMax[i,]*1.13
		x <- sort(x)
		n <- length(x)
		y <- (1:n)/(n+1)
		w <- y
		data <- list(x=x,y=y,w=w)		
		wls <- try(nlsLM(y ~ exp(-(1+(sh*((x-loc)/sc)))^(-1/sh)),data=data,weights=w,start=list(loc=gev.fit(x,show=F)$mle[1],sc=gev.fit(x,show=F)$mle[2],sh=gev.fit(x,show=F)$mle[3])),silent=T)
		start.sh <- c(0.01,-gev.fit(x,show=F)$mle[3],-0.1,0.1,-0.3,0.3)
		s <- 1
		while(class(wls) == "try-error") {
			wls <- try(nlsLM(y ~ exp(-(1+(sh*((x-loc)/sc)))^(-1/sh)),data=data,weights=w,start=list(loc=gev.fit(x,show=F)$mle[1],sc=gev.fit(x,show=F)$mle[2],sh=start.sh[s])),silent=T)
			s <- s+1
		}
		shape[i] <- round((coef(wls)[3] + 5)*100,digits=0)
	}
} #end i-loop

shapeGrid <- array(NA,dim=c(1195,1550))
shapeGrid[TabID] <- shape   # back to full grid

#filename <- "/home/anitavd/PhD/Daily/compareMT/shape"
filename <- "/home/anitavd/PhD/Daily/compareMT/shape.wls"

writebinfile(shapeGrid,paste(filename,".bil",sep=""))

hdrfile <- paste(filename,".hdr",sep="")
blwfile <- paste(filename,".blw",sep="")

system("cp /home/anitavd/GISfile.hdr /home/anitavd/PhD/Daily/compareMT/")
system("cp /home/anitavd/GISfile.blw /home/anitavd/PhD/Daily/compareMT/")
system(paste("mv /home/anitavd/PhD/Daily/compareMT/GISfile.hdr ",hdrfile,sep=""))
system(paste("mv /home/anitavd/PhD/Daily/compareMT/GISfile.blw ",blwfile,sep=""))

}
