##################################################
#
# Script that computes the areal intensity with increasing area.
# region: East, West, Mid, North
# threshold: ~2 year return value for the region
#
# Anita Verpe Dyrrdal, met.no, feb2012
#
##################################################



increaseArea <- function(X, Y ,start.year,end.year,region) {

outpath <- paste("/win_P/PhD/Research/Comparison_methods/compareMT/testArealShape/newAnalysis/series/",region,"/",sep="")

#Vector with number of days in the months
nodays.vector <- c(31,28,31,30,31,30,31,31,30,31,30,31)

#Find point in binary file
x = round((X - (-75000 + 500)) / 1000, digits = 0) + 1
y = round((Y - (6450000 + 500)) / 1000, digits = 0) + 1

point = (1550 - y) * 1195 + (x - 1) + 1

#Open file to write result to
outfile <- paste(outpath, "RRarea.txt",sep="")
unlink(outfile)
zz <- file(outfile, "a")

#Write header to file
#cat(paste("Year", "Month", "Day", "RR1","RR21","RR41","RR61","RR81","RR101","RR121","RR141", sep="\t"), file=zz)
cat(paste("Year", "Month", "Day", "RR1","RR7","RR11","RR17","RR21","R27","RR31","RR37","RR41","RR47","RR51", sep="\t"), file=zz)
close(zz)

for (year in start.year:end.year) {

	for (month in 1:12) {

		nodays <- nodays.vector[month]

		if (month == 2) {
		if(is.integer(((year/4)-1):(year/4))) nodays <-29
		}

		for (day in 1:nodays) {

			rr.area.all <- c()

			#Read precipitation grid
			filename=sprintf("/klimagrid/daily/rr_uncorrected/binary/%4i/%2.2i/rr_%4i_%2.2i_%2.2i.bil",year,month,year,month,day)
			print(filename)
			con=file(filename,open="rb")
			rr.grid<- readBin(con, integer(), size=2, n=1550*1195)
			close(con)

			#for (j in c(0,10,20,30,40,50,60,70)) { 
			for (j in c(0,3,5,8,10,13,15,18,20,23,25)) {

			if (j==0) {
				idx <- point
			} else {

				idx <- NULL
				idx <- cbind(idx,seq(point-j,point+j))
				for (k in 1:j) {
					idx <- cbind(idx,seq(point-(k*1195)-j,point-(k*1195)+j))
					idx <- cbind(idx,seq(point+(k*1195)-j,point+(k*1195)+j))
				}
			}
			
			rr <- rr.grid[idx]
			rr[rr == 10000] = NA
			rr <- rr/10    # [mm]
			rr.area <- round(mean(rr, na.rm=TRUE),1)
			rm(rr)
			gc(reset=TRUE)  

			#Write values to file
			if (j==0) {
			zz <- file(outfile, "a")
			cat("\n",paste(year,month,day,rr.area,sep="\t"),"\t",file=zz,append=TRUE)
			close(zz)
			} else {
			zz <- file(outfile, "a")
			cat(paste(rr.area),"\t",file=zz,append=TRUE)
			close(zz)
			}
			}
		}
	}
}

}





				
