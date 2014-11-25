#####################################################################################
#
# Script that takes areal precipitation series (extracted from climate grids with
# extractArealPrecip.R) and computes MT (return periods) and PMP using the
# NERC / Growth factor / Førland / M5 - method
# Subfunctions modified from scripts by Eli Alfnes are included
#
# pathtofiles (mandatory): Path to areal series file. Results are also written to this directory
# field.name (mandatory): Name of the study field, f.ex. a catchment 
#
# Writes results to a text file, including annual maxima
#
# Anita Verpe Dyrrdal, met.no, November 2011
#
#####################################################################################


#source("/home/anitavd/phd/R/calc_pmp_nday.R")          #R script that computes pmp, created by Eli Alfnes
source("/vol/klimadata/applikasjon/gridding/src/iobin.R")
source("/vol/klimadata/applikasjon/gridding/src/met_stat.R")
#library(date)

computeArealExtremes <- function(pathtofiles, field.name) {

#Read daily areal timeseries
areal.rr <- read.table(paste(pathtofiles,field.name,"/",field.name,"_areal_precipitation_grid.txt",sep=""), header=TRUE, skip=9)
#Normal annual areal precipitation
arealPN <- read.table(paste(pathtofiles,field.name,"/","arealPN.txt",sep=""), header=FALSE, dec=".")
#Area of field
area <- read.table(paste(pathtofiles,field.name,"/","area.txt",sep=""), header=FALSE, dec=".")

#Find first date, last date and number of years
start.year <- areal.rr$Year[1]
end.year <- tail(areal.rr$Year,1)
no.years <- end.year - start.year + 1
start.month <- areal.rr$Month[1]
end.month <- tail(areal.rr$Month,1)

#Use default season definitions, or define new ones?
default <- readline("Use default seasons? (y/n) ")
if (default == "n") {
	#Define winter (The winter ends in the current year, f.ex. the winter of 2011 is defined as the winter 2010/2011)
	winterdef <- readline("Define winter season (m/mm with space between each month): ")
	winterdef <- strsplit(winterdef," ")
	no <- length(winterdef[[1]])
	winter <- as.numeric(winterdef[[1]][1])
	for (i in 2:no) {
		winter <- append(winter,as.numeric(winterdef[[1]][i]),after=i-1)
	}
	#Define spring
	springdef <- readline("Define spring season (m/mm with space between each month): ")
	springdef <- strsplit(springdef," ")
	no <- length(springdef[[1]])
	spring <- as.numeric(springdef[[1]][1])
	for (i in 2:no) {
		spring <- append(spring,as.numeric(springdef[[1]][i]),after=i-1)
	}
	#Define summer
	summerdef <- readline("Define summer season (m/mm with space between each month): ")
	summerdef <- strsplit(summerdef," ")
	no <- length(summerdef[[1]])
	summer <- as.numeric(summerdef[[1]][1])
	for (i in 2:no) {
		summer <- append(summer,as.numeric(summerdef[[1]][i]),after=i-1)
	}
	#Define fall
	falldef <- readline("Define fall season (m/mm with space between each month): ")
	falldef <- strsplit(falldef," ")
	no <- length(falldef[[1]])
	fall <- as.numeric(falldef[[1]][1])
	for (i in 2:no) {
		fall <- append(fall,as.numeric(falldef[[1]][i]),after=i-1)
	}
} else { 
	#Default seasons
	winter=c(12,1,2)
	spring=c(3,4,5)
	summer=c(6,7,8)
	fall=c(9,10,11)
}
rm(default)

#Use default durations or define new ones?
default <- readline("Use default durations? (y/n) ")
if (default == "n") {
	durdef <- readline("Define durations (# of hours with space between each duration. Hours > 24 have to be a multiple of 24): ")
	durdef <- strsplit(durdef," ")
	no <- length(durdef[[1]])
	nHours <- as.numeric(durdef[[1]][1])
	for (i in 2:no) {
		nHours <- append(nHours,as.numeric(durdef[[1]][i]),after=i-1)
	}
} else {
	#Default durations
	nHours <- c(1,2,6,12,24,48,72)
}

#Different methodology is used for estimation of several-days extremes (directly) and sub-daily extremes (M5(24hr) --> M5(nhr))  
#Compute extreme precipitation for several-days duration
nDays <- as.integer(nHours[nHours>=24]/24)
#Create several-days precipitation sums (first values become NA) using the function nDaySum (met_stat.R, E.Alfnes)
area.nDays <- array(NA,dim=c(length(areal.rr$MAP),length(nDays)))
for (i in 1:length(nDays)) area.nDays[,i] <- nDaySum(areal.rr$MAP,nDays[i],sides=1)

#Give area.nDays Year, Month and Day as in areal.rr
area.nDays <- cbind(areal.rr[,1:3],area.nDays)
#colnames(area.nDays) <- c("Year","Month","Day","d1","d2")   #generalize!!!!!!!

#Generate array of annual and seasonal maximum areal precipitation 
areal.max <- array(NA,c(5,no.years,length(nDays)))   
startidx.annual <- 1
endidx.annual <- no.years
if(start.year > 1957) startidx.winter <- 1
else startidx.winter <- 2
endidx.winter <- no.years
startidx.spring <- 1
endidx.spring <- no.years
startidx.summer <- 1
endidx.summer <- no.years
startidx.fall <- 1
endidx.fall <- no.years
if(start.month!=1) startidx.annual <- 2
if(end.month!=12) endidx.annual <- no.years - 1
if(end.month<tail(winter,1)) endidx.winter <- no.years - 1
if(start.month>spring[1]) startidx.spring <- 2
if(end.month<tail(spring,1)) endidx.spring <- no.years - 1
if(start.month>summer[1]) startidx.summer <- 2
if(end.month<tail(summer,1)) endidx.summer <- no.years - 1
if(start.month>fall[1]) startidx.fall <- 2
if(end.month<tail(fall,1)) endidx.fall <- no.years - 1
n <- 4  
for (i in 1:length(nDays)) { 
	areal.max[1,startidx.annual:endidx.annual,i] <- AreaAnnualMax(area.nDays[,n],area.nDays[,1],start.year+startidx.annual-1,start.year+endidx.annual-1) 
	areal.max[2,startidx.winter:endidx.winter,i] <- computeSeasonMax(area.nDays[,n],area.nDays[,1],area.nDays[,2],start.year+startidx.winter-1,start.year+endidx.winter-1,winter)  
	areal.max[3,startidx.spring:endidx.spring,i] <- computeSeasonMax(area.nDays[,n],area.nDays[,1],area.nDays[,2],start.year+startidx.spring-1,start.year+endidx.spring-1,spring)
	areal.max[4,startidx.summer:endidx.summer,i] <- computeSeasonMax(area.nDays[,n],area.nDays[,1],area.nDays[,2],start.year+startidx.summer-1,start.year+endidx.summer-1,summer)
	areal.max[5,startidx.fall:endidx.fall,i] <- computeSeasonMax(area.nDays[,n],area.nDays[,1],area.nDays[,2],start.year+startidx.fall-1,start.year+endidx.fall-1,fall)
	n <- n + 1
}

#Write areal.max to a file
write.table(areal.max[1,,1],paste(pathtofiles,field.name,"/",field.name,"_annualMaxima.txt",sep=""),quote=FALSE,row.names=FALSE)

print(paste("Annual maxima written to file ",paste(pathtofiles,field.name,"/",field.name,"_annualMaxima.txt",sep=""),sep=""))		

#Return periods (Inf = PMP)
retper <- array(c(5,10,50,100,200,500,1000,Inf)) 

#Compute return values and pmp  
#dim(areal.MT) = length(retper) (return periods),5 (seasons),length(nHours) (durations)
areal.MT <- array(NA,c(length(retper),5,length(nHours)))
   
#Go through each season  
for (i in 1:5) {     					 
	areal.MT[,i,(length(nHours)-length(nDays)+1):length(nHours)] <- round(computeArealMT(areal.max[i,,],T=retper,nDay=nDays),digits=0)
}

#Compute extreme precipitation for sub-daily duratons using rates computed by the function M5TM524 (met_stat.R, E.Alfnes)  
for (i in 1:(length(nHours)-length(nDays))) {
	rate <- M5TM524(arealPN,nHours[i])
	areal.MT[,,i] <- round(areal.MT[,,(length(nHours)-length(nDays)+1)]*as.numeric(rate),digits=0)  #her skjer det noe med areal.MT!!!!!!!!!!!
}
	
#Create table with M5
M5 <- areal.MT[1,,]
#Create table with MT(24hr)
idx <- which(nHours == 24)
MT.24hr <- areal.MT[,,idx]
#Add row with ratio M5(season)/M5(year)
MT.24hr <- rbind(c(1,round(M5[2,1]/M5[1,1],digits=2),round(M5[3,1]/M5[1,1],digits=2),round(M5[4,1]/M5[1,1],digits=2),round(M5[5,1]/M5[1,1],digits=2)),MT.24hr)
colnames(MT.24hr) <- c("Year","Winter","Spring","Summer","Fall")
rownames(MT.24hr) <- c("M5-season/M5-year","M5", "M10", "M50", "M100", "M200", "M500", "M1000", "PMP")
M5.24hr.year <- MT.24hr[2,1]

#Create table with MT and PMP for different durations, one table for each season
MT.nhr.year <- areal.MT[,1,] 
cnames <- c()
for (i in 1:length(nHours)) cnames <- c(cnames,nHours[i]) 
rnames <- c("M5", "M10", "M50", "M100", "M200", "M500", "M1000", "PMP")
colnames(MT.nhr.year) <- cnames
rownames(MT.nhr.year) <- rnames
MT.nhr.winter <- areal.MT[,2,] 
colnames(MT.nhr.winter) <- cnames
rownames(MT.nhr.winter) <- rnames
MT.nhr.spring <- areal.MT[,3,] 
colnames(MT.nhr.spring) <- cnames
rownames(MT.nhr.spring) <- rnames
MT.nhr.summer <- areal.MT[,4,] 
colnames(MT.nhr.summer) <- cnames
rownames(MT.nhr.summer) <- rnames
MT.nhr.fall <- areal.MT[,5,] 
colnames(MT.nhr.fall) <- cnames
rownames(MT.nhr.fall) <- rnames

#Write results to file
outfile <- paste(pathtofiles, field.name,"/", field.name, "_estimated_extreme_precipitation_grid.txt",sep="")
unlink(outfile)
zz <- file(outfile, "a")

cat(paste("\n\nEstimated extreme precipitation for ",field.name,"\n\nNormal annual precipitation: PN ~ ", round(arealPN,digits=0), "\n\nArea size: ",round(area/1000000,digits=0)," km²\n\nM5(24hr) / PN ~ ", round(M5.24hr.year*100/arealPN,digits=1)," % ===> M5(24hr) ~ ",M5.24hr.year," mm\n\nEstimated 24-hour precipitation values\n\n"), file=zz)
close(zz)
write.table(MT.24hr,outfile,append=TRUE,quote=FALSE,col.names=NA,sep="\t")

zz <- file(outfile, "a")
cat(paste("\n\nEstimated n-hour precipitation values\nYear\n\n"), file=zz)
close(zz)
write.table(MT.nhr.year,outfile,append=TRUE,quote=FALSE,col.names=NA,sep="\t")

zz <- file(outfile, "a")
cat(paste("\n\nWinter\n\n"), file=zz)
close(zz)
write.table(MT.nhr.winter,outfile,append=TRUE,quote=FALSE,col.names=NA,sep="\t")

zz <- file(outfile, "a")
cat(paste("\n\nSpring\n\n"), file=zz)
close(zz)
write.table(MT.nhr.spring,outfile,append=TRUE,quote=FALSE,col.names=NA,sep="\t")

zz <- file(outfile, "a")
cat(paste("\n\nSummer\n\n"), file=zz)
close(zz)
write.table(MT.nhr.summer,outfile,append=TRUE,quote=FALSE,col.names=NA,sep="\t")

zz <- file(outfile, "a")
cat(paste("\n\nFall\n\n"), file=zz)
close(zz)
write.table(MT.nhr.fall,outfile,append=TRUE,quote=FALSE,col.names=NA,sep="\t")

#return(areal.MT)
print(paste("Results written to file ",outfile,sep=""))	



}

################################################################################################
#Compute annual maxima
AreaAnnualMax <- function(area.values,years,syear,eyear) {

annual.max <- vector(mode="integer",length=0)

for(year in syear:eyear) {

	start.idx <- which(years == year)[1]
	end.idx <- tail(which(years == year),1)

	if (sum(!is.na(area.values[start.idx:end.idx])*1) > 0)  #at least one value != NA
		annual.max[length(annual.max)+1] <- max(area.values[start.idx:end.idx],na.rm=TRUE)
	else 
		annual.max[length(annual.max)+1] <- NA
}
return(annual.max)
}


#####################################################################################
# Compute seasonal maxima
computeSeasonMax <- function(area.values,years,months,syear,eyear,season) {

season.max <- vector(mode="integer",length=0)
season.max <- NULL

for(year in syear:eyear) {

	season.values <- NULL

	if(season[1]>6 & tail(season,1)<6) {       #winter season including two different years      		
		start.idx <- which(years == year-1 & months == season[1])[1]
		end.idx <- tail(which(years == year & months == tail(season,1)),1)
	} else {
		start.idx <- which(years == year & months == season[1])[1]
		end.idx <- tail(which(years == year & months == tail(season,1)),1)
	}

       	season.values <- area.values[start.idx:end.idx]
	season.max <- c(season.max,max(season.values,na.rm=TRUE))
}
return(season.max)
}

###############################################################################################
#-------------------------------------------------
# Calculate return values for daily percipitation (modified areal.pmp in met_stat, E.Alfnes)
#-------------------------------------------------
# The Gumbel equation is used to calculate the 5 years return value 
# The Nerc equation is used to calculate the <5,Inf] years return 
# values where Inf represent the PMP value
# area_max	array of time series of yearly area maximum values,
#		nrow = num of areas, ncol = num of years
# T		vector of return period(s)
# returns X 	array of return values, nrow = num of return periods, 
#		ncol = num of durations
###############################################################################################

computeArealMT <- function(area.max,T,nDay=NA) {
  
if (length(T[T<5])>0)
stop("computeArealMT not implemented for return periods less than 5 years")

M5 <- computeMT_Gumbel(area.max,T=5) #Return periods = 5, array with rows=durations, columns=return periods (in this case only one)
    
# omregning fra døgn- til n*24-timersverdier
nd24nh <- c(1.13,1.04,1.03,1.02,1.02,1,1,1,1,1,1,1)
factor <- nd24nh[nDay]
M5 <- M5 * factor
T <- T[T>5]                             #Additional return periods
X <- array(NA,dim=c(length(T)+1,length(M5)))    
X[1,] <- avrund(M5)			# X[T=5] = M5
if(nrow(X)>1) X[2:nrow(X),] <- avrund(computeMT_Nerc(M5,T))                           

return(X)
}

####################################################################################################
#--------------------------------------------------
# Calculate return values using the Gumbel equation (modified Gumbel in met_stat.R, E.Alfnes)
#--------------------------------------------------
# area.max	array of time series of yearly area maximum values,
#		nrow = num of areas, ncol = num of years
# T		vector of return period(s)
# returns X	array of GUMBEL return values, nrow = num of durations in days, 
#		ncol = num of return periods
####################################################################################################

computeMT_Gumbel <- function(area.max, T=5) {
  
if (is.vector(area.max)) {
	Xmean <- mean(area.max,na.rm=TRUE)    #mean of annual max
	Xstdev <- sd(area.max,na.rm=TRUE)     #stdev of annual max
}
else if (is.array(area.max)) {
	Xmean <- colMeans(area.max,na.rm=TRUE)    
	Xstdev <- apply(area.max,2,sd,na.rm=TRUE)     
} else stop("area.max is neither a vector nor an array")

# Gumbel equation
X <- Xmean - drop(array( (sqrt(6)/pi)*(0.577+log(-log((T-1)/T)))%x%Xstdev ,
            dim=c(length(Xstdev),length(T)) ))
return(X)
}

###############################################################################################
#--------------------------------------------------
# Calculate return values using the NERC equation (Modified Nerc from met_stat.R, E.Alfnes)
#--------------------------------------------------
# M5		vector of 5 years return values calculated using 
#		the Gumbel equation
# T		vector of return periods
# returns NX 	array of NERC return values, nrow = num of areas, 
#		ncol = num of return periods
###############################################################################################

computeMT_Nerc <- function(M5, T=1000) {
  
NX <- array(NA,dim=c(length(T),length(M5))) 

for (i in 1:length(M5)) {
	if (!is.na(M5[i])) {
		if (M5[i] > 1000)     C <- NA  # C not defined for M5 >1000
		else if(M5[i] > 350)  C <- (0.167-0.0145^log(M5[i]))
		else if(M5[i] > 25)   C <- (0.3584-0.0473*log(M5[i]))
		else if(M5[i] > 15)   C <- (0.300-0.0294*log(M5[i]))
		else if(M5[i] > 10)   C <- 0.219
		else if(M5[i] > 2)    C <- (0.165+0.0236*log(M5[i]))
		else                  C <- NA  # C not defined for M5 <= 2 
 
      		if (M5[i] > 200)      T[!is.finite(T)] <- 10000
      		else if (M5[i] > 125) T[!is.finite(T)] <- 22503-62.4*M5[i]
      		else if (M5[i] > 45)  T[!is.finite(T)] <- 47829-262.9*M5[i]
      		else                  T[!is.finite(T)] <- 36000
 
      		for (tidx in 1:length(T))
        		NX[tidx,i] <- M5[i] * exp(C*(log(T[tidx]-0.5)-1.5))
    	} 
} 
return(NX)
}

