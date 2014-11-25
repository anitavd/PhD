source("/home/anitavd/scripts/iobin.R")
source("//home/anitavd/scripts/met_stat.R")
library(PBSmapping)
library(miIO, lib.loc="/home/anitavd/R_libs/")
library(ncdf)
library(evir)

shape_rr1_nora10 <- function(start.year,end.year) {

no.years <- end.year - start.year + 1

maxSeries <- array(NA,dim=c(80*164,no.years))

year.idx <- 1

#Vector with number of days in the months
no.days.vector <- c(31,28,31,30,31,30,31,31,30,31,30,31)

for (year in start.year:end.year) {

	max <- array(0,dim=c(80,164))

	for (month in 1:12) {

		no.days <- no.days.vector[month]

		if (month == 2) {
			if(is.integer(((year/4)-1):(year/4))) no.days <-29
		}

		for (day in 1:no.days) {

			for (time in 0:23) {
	
				#Read felt file 
				prm.flt <- NULL
				prm.flt <- rbind(c(2,17,1000,0))
				filename <- sprintf("/fou/klima/hildeh/infrarisk/flt1hr/%i/%2.2i/fc.%i%2.2i%2.2i%2.2i",year,month,year,month,day,time)
				print(filename)
 
				file <- miReadFelt(files=filename,prm=cbind(2,17,1000,0),prg=rep(0,24),collapse.time=FALSE,acc=F,df=T,atime=F) #80*164*24 - the 24 values are equal

				rr1 <- as.array(file$data[,,1])

				#Check if max
				max[which(rr1 > max)] <- rr1[which(rr1 > max)]

				rm(rr1)
				rm(file)
				gc(reset=TRUE)

			}
		}
	}

	maxSeries[,year.idx] <- max
	rm(max)
	gc(reset=TRUE)

	year.idx <- year.idx + 1
}

shape <- array(NA,dim=80*164)

for (i in 1:(80*164)) {
print(i)
	shape[i] <- gev.fit(maxSeries[i,])$mle[3]*100   #to get decimals and avoid negative numbers
} #end i-loop

return(shape)

}




