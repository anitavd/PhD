# Script that computes the variance of the GEV shape parameter for each grid cell of the uncorrected RR grid

library(evir)
source("/klimadata/applikasjon/gridding/src/iobin.R")
source("/klimadata/applikasjon/gridding/src/met_stat.R")

computeVarShape <-function(startYear,endYear) {

noYears <- endYear - startYear + 1

ncol <- 1195
nrow <- 1550

#Read file with indexes for non-NA values (new version)
TabID <- as.numeric(as.matrix(read.table("/home/anitavd/scripts/TabID.txt",header=TRUE)))

yearMax <- array(NA,dim=c(length(TabID),noYears))

year.idx <- 1

#Read annual max files for all years
for (year in startYear:endYear) {
	
	filename=sprintf("/fou/klima/anitavd/klimagrid/annualMax_rr_uncor/rr1d/rrymax/binary/rrymax_%4i.bil",year)
	con=file(filename,open="rb")
	maxgrid=readBin(con,integer(),size=2,n=ncol*nrow)
    	maxgrid[maxgrid>=10000] <- NA
	maxgrid[maxgrid==-1] <- NA
  	yearMax[,year.idx] <- maxgrid[TabID]/10   #mm
	
	close(con)
	rm(maxgrid)
	gc(reset=TRUE)

	year.idx <- year.idx + 1

} #end year-loop

shape <- array(NA,dim=(length(TabID)))

for (i in 1:length(TabID)) {
	shape[i] <- (gev(yearMax[i,])$varcov[1,1] + 1)*100   #to get decimals and avoid negative numbers
} #end i-loop

shapeGrid <- array(NA,dim=c(1195,1550))
shapeGrid[TabID] <- shape   # back to full grid

filename <- "/home/anitavd/PhD/Comparison_methods/compareMT/varShape"

writebinfile(shapeGrid,paste(filename,".bil",sep=""))

hdrfile <- paste(filename,".hdr",sep="")
blwfile <- paste(filename,".blw",sep="")

system("cp /home/anitavd/GISfile.hdr /home/anitavd/PhD/Comparison_methods/compareMT/")
system("cp /home/anitavd/GISfile.blw /home/anitavd/PhD/Comparison_methods/compareMT/")
system(paste("mv /home/anitavd/PhD/Comparison_methods/compareMT/GISfile.hdr ",hdrfile,sep=""))
system(paste("mv /home/anitavd/PhD/Comparison_methods/compareMT/GISfile.blw ",blwfile,sep=""))

}
