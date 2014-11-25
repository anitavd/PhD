#############################################################
#
# Skript som reknar ut maks aarlege nedboerssummar,
# og som kan brukast i staden for C-scripta.
#
# Inneheld to funksjonar:
# ------------------------
# - AnnMax_scen: for scenariar/kontrollkjoringar
# - AnnMax_hist: for historiske data
#
# Kjoerast med feks:
# --------------------
# startYear <- 1957
# endYear <- 2010
# nday <- 1
# source("AnnualMax.R")
# AnnMax_hist(startYear,endYear,nday)
#
# Reidun 01.2013
#############################################################

source("/vol/klimadata/applikasjon/gridding/src/iobin.R")
source("/vol/klimadata/applikasjon/gridding/src/met_stat.R")


################
# AnnMax_hist:
################

AnnMax_hist <-function(startYear,endYear) {

noYears <- endYear - startYear + 1

idx.year <- 1

TabID <- as.numeric(as.matrix(read.table("/home/anitavd/scripts/TabID.txt",header=TRUE)))

#Vector with number of days in the months
noDays.vector <- c(31,28,31,30,31,30,31,31,30,31,30,31)

#Loop through all years
for (year in startYear:endYear) {
	print(year)
	monthMax = array(0,dim=c(length(TabID),12))
	for (month in 1:12) {
#		print(month)
		noDays <- noDays.vector[month]
		if (month == 2) {
			if(is.integer(((year/4)-1):(year/4))) noDays <-29
		}
		startDay <- 1
		dayMax = array(0,dim=c(length(TabID),noDays))
			for (day in startDay:noDays) {
			#Read binary files with precipitation sum
	 		filename <- sprintf("/vol/klimagrid/daily/rr_uncorrected/binary/%4i/%2.2i/rr_%4i_%2.2i_%2.2i.bil",year,month
,year,month,day)
			con=file(filename,open="rb")
			rr=readBin(con,integer(),size=2,n=1195*1550)
			close(con)

			# Berre Norge:
            RR <- rr[TabID]
			RR[RR<0] <- 0
			dayMax[,day] = RR
			rm(rr)
			rm(RR)
            gc(reset=TRUE)
		}#End day-loop
		monthMax[,month] = apply(dayMax,1,max)
		rm(dayMax)
	    gc(reset=TRUE)
	}#End month-loop
	yearRR = apply(monthMax,1,max)
    yearMax <- array(NA,dim=c(1195,1550))
    yearMax[TabID] <- yearRR
	filename <- sprintf("/home/anitavd/PhD/Daily/annualMax/rr_uncor_anMax_%4i",year)
	writebinfile(yearMax,paste(filename,".bil",sep=""))

    rm(monthMax)
	rm(yearRR)
	rm(yearMax)
	gc(reset=TRUE)

	idx.year <- idx.year + 1
}#End year-loop
}#End AnnMax_hist
