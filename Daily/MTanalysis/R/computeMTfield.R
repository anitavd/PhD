#####################################################################################
#
# Script that takes areal precipitation series (extracted from climate grids with
# extractArealPrecip.R) and computes MT (return periods) and PMP using the
# NERC / Growth factor / Førland / M5 - method for 1 to several day precipitation sums.
# The time period is chosen by the user. 
# Subfunctions modified from scripts by Eli Alfnes are included
#
# pathtofiles (mandatory): Path to areal series file. Results are also written to this directory
#			   "/p/PhD/Research/Comparison_methods/Output_GB/"
# field.name (optional): Name of the study field, f.ex. a catchment
#
# Writes results to a text file
#
# Anita Verpe Dyrrdal, met.no, Jan 2012
#
#####################################################################################


#source("/home/anitavd/phd/R/calc_pmp_nday.R")          #R script that computes pmp, created by Eli Alfnes
source("/klimadata/applikasjon/gridding/src/iobin.R")
source("/klimadata/applikasjon/gridding/src/met_stat.R")
library(date)

computeMTfield <- function(pathtofiles, outpath, field.name, start.year, end.year, nDays) {

#Read daily areal timeseries
areal.rr <- read.table(paste(pathtofiles,field.name,"/",field.name,"_areal_precipitation_grid.txt",sep=""), header=TRUE, skip=9)
#Normal annual areal precipitation
arealPN <- read.table(paste(pathtofiles,field.name,"/","arealPN.txt",sep=""), header=FALSE, dec=".")
#Area of field
area <- read.table(paste(pathtofiles,field.name,"/","area.txt",sep=""), header=FALSE, dec=".")

#Default seasons
winter=c(12,1,2)
spring=c(3,4,5)
summer=c(6,7,8)
fall=c(9,10,11)

no.years <- end.year - start.year + 1

if(start.year==1957) {
	start.month <- areal.rr$Month[1]
	end.month <- tail(areal.rr$Month,1)
} else {
	start.month=1
	end.month=12
}

#Create several-days precipitation sums (first values become NA) using the function nDaySum (met_stat.R, E.Alfnes)
area.nDays <- array(NA,dim=c(length(areal.rr$MAP),length(nDays)))
for (i in 1:length(nDays)) area.nDays[,i] <- nDaySum(areal.rr$MAP,nDays[i],sides=1)
#area.nDays <- areal.rr

#Give area.nDays Year, Month and Day as in areal.rr
area.nDays <- cbind(areal.rr[,1:3],area.nDays)
#colnames(area.nDays) <- c("Year","Month","Day","d1","d5","d10")   #generalize!!!!!!!
#area.nDays$MAP5 <- nDaySum(areal.rr$MAP,nDays[2],sides=1)
#area.nDays$MAP10 <- nDaySum(areal.rr$MAP,nDays[3],sides=1)

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

#Return periods (Inf = PMP)
retper <- array(c(5,10,50,100,200,500,1000,Inf)) 

#Compute return values and pmp  
areal.MT <- array(NA,c(length(retper),5,length(nDays)))
   
#Go through each season  
for (i in 1:5) {     					 
	areal.MT[,i,1:length(nDays)] <- round(computeArealMT(areal.max[i,,],T=retper,nDay=nDays),digits=0)
}	



MT.matrix <- matrix(NA,5*length(nDays),length(retper))

#Create table with MT and PMP for different durations, one table for each season
l=1
for (i in 1:5) {
	for (j in 1:length(nDays)) {
		MT.matrix[l,] <- areal.MT[,i,j] 
		l <- l + 1
	}
}

rnames <- c("Y1","Y2","Y3","W1","W2","W3","Sp1","Sp2","Sp3","S1","S2","S3","F1","F2","F3")
cnames <- c("M5", "M10", "M50", "M100", "M200", "M500", "M1000", "PMP")
colnames(MT.matrix) <- cnames
rownames(MT.matrix) <- rnames

#Write results to file
outfile <- paste(outpath,field.name,"/", field.name, "_estimated_extreme_precipitation_grid_",start.year,end.year,".txt",sep="")
write.table(MT.matrix,outfile,quote=FALSE,sep="\t",col.names=NA)


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

