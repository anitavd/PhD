########################################################################
#
# Compute normal values from the uncorrected precipitation grids
#
#
#
#
#
# Anita Verpe Dyrrdal, met.no, Oct 2011
#
#
#########################################################################

rm(list=ls())

source("/klimadata/applikasjon/gridding/src/iobin.R")
source("/klimadata/applikasjon/gridding/src/met_stat.R")
library(Kendall)

computeNormalPrecip <- function(startYear, endYear) {

noYears <- endYear-startYear+1

# Find index for non-NA values 
TabID <- as.numeric(as.matrix(read.table("/home/anitavd/scripts/TabID.txt",header=TRUE)))

#Vector with number of days in the months
noDays.vector <- c(31,28,31,30,31,30,31,31,30,31,30,31)

# Grid med resultater
precipsum <- array(0,dim=(length(TabID)))

#Loop through all years
for (year in startYear:endYear) { 

	for (month in 1:12) {	

		noDays <- noDays.vector[month]

		if (month == 2) {
			if(is.integer(((year/4)-1):(year/4))) noDays <-29
		}

		for (day in 1:noDays) {

			#Read binary files with precipitation
			filename=sprintf("/klimadata/archive/MatthiasM/daily/rr_uncorrected/rr/binary/%4i/%2.2i/rr_%4i_%2.2i_%2.2i.bil",year,month,year,month,day)	
			print(filename)
			con=file(filename,open="rb")
			rr=readBin(con,integer(),size=2,n=1195*1550)
			rr[rr==10000] <- NA
			rr[rr==-1] <- NA
			rr[rr < 0] = 0
			rr <- rr / 10     # Values in mm
			close(con)

			precipsum <- precipsum + rr[TabID]

			rm(rr)
			gc(reset = TRUE)

		}
	}
}

#Compute normal values
normalprecip <- array(NA,dim=c(1195,1550))
normalprecip[TabID] <- round((precipsum/30),digits=0)
rm(precipsum)
gc(reset=TRUE)

#Write normal values to file
filename <- sprintf("/fou/klima/anitavd/klimagrid/rr_uncor/normal/normal_rruncor_%4i%4i.bil",startYear,endYear)
writebinfile(normalprecip,filename)

}



