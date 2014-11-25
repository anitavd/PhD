# Script that extracts convective (20) and stratiform (19) precipitation from hindcast,
# on the dates og the 10 maximum observed precipitation amounts at different stations

library(PBSmapping)
library(miIO, lib.loc="/home/anitavd/R_libs/")

stnr <- 18701
obs <- read.table(paste("/home/anitavd/PhD/Sub-daily/data/tipping/max10/",stnr,"_hourly_AM10.txt",sep=""),skip=3)
colnames(obs) <- c("STNR","EAST","NORTH","LON","LAT","YEAR","MONTH","DAY","TIME","RR1")

no.obs <- length(obs$STNR)

#Find coordinates
LL <- cbind(obs$LAT[1],obs$LON[1])
colnames(LL) <- c("Y","X")

no.days <- c(31,28,31,30,31,30,31,31,30,31,30,31)

conv <- c()
strat <- c()
type <- c() 
season <- c()

for (i in 1:no.obs) {
	
	year <- obs$YEAR[i]
	month <- obs$MONTH[i]
	day <- obs$DAY[i]
	time <- obs$TIME[i] - 1
	if(time == -1) {
		time <- 23
		day <- day - 1
		if(day == 0) {
			month <- month - 1
			day <- no.days[month]
			if(month==2) {
				if(is.integer(((year/4)-1):(year/4))) day <- 29
			}
		}
	} 

	#Convective
	prm <- NULL
	prm <- rbind(c(2,20,1000,0))

	filename <- sprintf("/fou/klima/hildeh/infrarisk/flt1hprc/%i/%2.2i/fc.%i%2.2i%2.2i%2.2i",year,month,year,month,day,time)
	print(filename)
 
	file <- miReadFelt(files=filename,sites=LL,prm=prm,prg=rep(0,24),collapse.time=FALSE,acc=F,df=T,atime=F) #80*164*24 - the 24 values are equal
	rr1 <- file$RR2[1]
	
	conv <- c(conv,rr1)

	#Stratiform
	prm <- NULL
	prm <- rbind(c(2,19,1000,0))

	filename <- sprintf("/fou/klima/hildeh/infrarisk/flt1hprl/%i/%2.2i/fc.%i%2.2i%2.2i%2.2i",year,month,year,month,day,time)
	print(filename)
 
	file <- miReadFelt(files=filename,sites=LL,prm=prm,prg=rep(0,24),collapse.time=FALSE,acc=F,df=T,atime=F) #80*164*24 - the 24 values are equal
	rr1 <- file$RR1[1]
	
	strat <- c(strat,rr1)

	if(((conv[i] > strat[i]*2) & (conv[i] > 0.5)) | (strat[i] == 0 & conv[i] > 0)) type <- c(type,1)
	else if(((strat[i] > conv[i]*2) & (strat[i] > 0.5)) | (conv[i] == 0 & strat[i] > 0)) type <- c(type,2)
	else type <- c(type,NA)

	if(month > 4 & month < 9) season <- c(season,1)
	else season <- c(season,2)
}






