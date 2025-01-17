#Computes annual max from the 3-hour precipitation grids from NVE

source("/vol/klimadata/applikasjon/gridding/src/iobin.R")
source("/vol/klimadata/applikasjon/gridding/src/met_stat.R")

computeAnnualMax_rr3 <-function(start.year,end.year) {

noYears <- end.year - start.year + 1

#Read file with indexes for non-NA values
filename <- "/home/anitavd/PhD/Sub-daily/RR3_AM/snowmask.bil"
con=file(filename,open="rb")
snowmask=readBin(con,integer(),size=1,n=1550*1195)
close(con)
TabID <- which(snowmask==1)

maxSeries <- array(NA,dim=c(320590,noYears))

year.idx <- 1

noDays.vector <- c(31,28,31,30,31,30,31,31,30,31,30,31)

#Read annual max files for all years
for (year in start.year:end.year) {

# Grid with results
max <- array(0,dim=320590)

for (month in 1:12) {

noDays <- noDays.vector[month]

if (month == 2) {
	if(is.integer(((year/4)-1):(year/4))) noDays <-29
}

for (day in 1:noDays) {	
	
	filename <- sprintf("/vol/klimadata/archive/anitavd/RR3/%4i/rr_%4i_%2.2i_%2.2i.bar",year,year,month,day)
	print(filename)
	con=file(filename,open="rb")
	rr3=readBin(con,integer(),size=2,n=320590*8*2)
	rr3 <- rr3*0.001   #mm
	dim(rr3) <- c(320590,8)

	#Take max 3-hour precipitation from that day
	rr <- apply(rr3,1,max,na.rm=T)
	
	close(con)

	#Check if max
	max[which(rr > max)] <- rr[which(rr > max)]

	rm(rr)
	gc(reset=TRUE)

}

}

#Write out as snowmask-grid
filename <- paste("/home/anitavd/PhD/Sub-daily/RR3_AM/RR3_anMax_",year,".bil",sep="")
writebinfile(max,filename)

maxSeries[,year.idx] <- max
rm(max)
gc(reset=TRUE)

year.idx <- year.idx + 1

} #end year-loop


# calculating return values using the Gumbel function declared in met_stat.R 
print("Compute M5")
# Compute M5
M5 <- Gumbel(maxSeries,T=5)        # Gumbel 5 years return value, mm 

M5grid <- array(NA,dim=c(1195,1550))
M5grid[TabID] <- avrund(M5)   # back to full grid
filename <- "/home/anitavd/PhD/Sub-daily/RR3_AM/M5_RR3.bil"
writebinfile(M5grid,filename)
rm(M5grid)
gc(reset=TRUE)


}

