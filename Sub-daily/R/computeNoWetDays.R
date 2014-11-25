###################################################################################
#
# PhD, Hourly precipitation covariate
# Script that computes frequency of wet days for the entire senorge grid
#
#
# Anita Verpe Dyrrdal, met.no, Apr-2013
#
###################################################################################

#rm(list=ls())

source("/vol/klimadata/applikasjon/gridding/src/iobin.R")
source("/vol/klimadata/applikasjon/gridding/src/met_stat.R")

computeNoWetDays <- function(startYear,endYear) {

noYears <- endYear-startYear+1

ncol <- 1195
nrow <- 1550

# Find index for non-NA values 
TabID <- as.numeric(as.matrix(read.table("/home/anitavd/scripts/TabID.txt",header=TRUE)))

#Time series for frequencies
freqSeries <- array(NA,dim=c(length(TabID),noYears))

idx.year <- 1

#Vector with number of days in the months
noDays.vector <- c(31,28,31,30,31,30,31,31,30,31,30,31)

#Loop through all years
for (year in startYear:endYear) { 

	# Grid med resultater
	frequency <- array(0,dim=(length(TabID)))

	for (month in 1:12) {	

		noDays <- noDays.vector[month]

		if (month == 2) {
			if(is.integer(((year/4)-1):(year/4))) noDays <-29
		}

		for (day in 1:noDays) {

			#Read binary files with precipitation
			filename=sprintf("/vol/klimagrid/daily/rr_uncorrected/binary/%4i/%2.2i/rr_%4i_%2.2i_%2.2i.bil",year,month,year,month,day)
			print(filename)
			con=file(filename,open="rb")
			rr=readBin(con,integer(),size=2,n=1195*1550)
			rr[rr==10000] <- NA
			rr[rr==-1] <- NA

			close(con)

			# Ekskludere NA verdier
			RR <- rr[TabID]
			# Set negative values to zero
			RR[RR < 0] = 0
			RR <- RR / 10     # Values in mm

			rm(rr)
			gc(reset=TRUE)

			frequency[which(RR>0.1)] <- frequency[which(RR>0.1)] + 1 

			rm(RR)
			gc(reset=TRUE)

		}#End day-loop

	}#End month-loop

	freqSeries[,idx.year] <- frequency

	rm(frequency)
	gc(reset=TRUE)

	idx.year <- idx.year + 1

}#End year-loop

frequencyGrid <- array(NA,dim=c(1195,1550))
frequencyGrid[TabID] <- avrund(rowMeans(freqSeries))   # back to full grid

filename <- sprintf("/home/anitavd/PhD/Sub-daily/Covariates/NoWetDays_%4i%4i",startYear,endYear)

writebinfile(frequencyGrid,paste(filename,".bil",sep=""))

rm(frequencyGrid)
gc(reset=TRUE)


} # end function
