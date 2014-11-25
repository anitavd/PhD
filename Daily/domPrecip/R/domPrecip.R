

source("/klimadata/applikasjon/gridding/src/iobin.R")
source("/klimadata/applikasjon/gridding/src/met_stat.R")

domPrecip <- function() {

#Read file with indexes for non-NA values (new version)
TabID <- as.numeric(as.matrix(read.table("/home/anitavd/scripts/TabID.txt",header=TRUE)))

no.years <- 2009 - 1957 + 1

dom.all <- array(NA,dim=c(length(TabID),no.years))

j <- 0

for (year in 1957:2009) {

	dom <- array(NA,dim=(length(TabID)))

	filename=sprintf("/fou/klima/anitavd/klimagrid/annualMax_rr_uncor/rr1d/rrymax/binary/rrymax_%4i.bil",year)
	print(filename)
	con=file(filename,open="rb")
	am <- readBin(con, integer(), size=2, n=1550*1195)
	close(con)
	am <- am[TabID]
	am[am == 10000] = NA

	filename=sprintf("/fou/klima/anitavd/klimagrid/annualMax_rr_uncor/rr1d/rrmmax/binary/%4i/rrmmax_%4i_06.bil",year,year)
	print(filename)
	con=file(filename,open="rb")
	jun <- readBin(con, integer(), size=2, n=1550*1195)
	close(con)
	jun <- jun[TabID]
	jun[jun == 10000] = NA

	filename=sprintf("/fou/klima/anitavd/klimagrid/annualMax_rr_uncor/rr1d/rrmmax/binary/%4i/rrmmax_%4i_07.bil",year,year)
	print(filename)
	con=file(filename,open="rb")
	jul <- readBin(con, integer(), size=2, n=1550*1195)
	close(con)
	jul <- jul[TabID]
	jul[jul == 10000] = NA

	filename=sprintf("/fou/klima/anitavd/klimagrid/annualMax_rr_uncor/rr1d/rrmmax/binary/%4i/rrmmax_%4i_08.bil",year,year)
	print(filename)
	con=file(filename,open="rb")
	aug <- readBin(con, integer(), size=2, n=1550*1195)
	close(con)
	aug <- aug[TabID]
	aug[aug == 10000] = NA

	filename=sprintf("/fou/klima/anitavd/klimagrid/annualMax_rr_uncor/rr1d/rrmmax/binary/%4i/rrmmax_%4i_09.bil",year,year)
	print(filename)
	con=file(filename,open="rb")
	sep <- readBin(con, integer(), size=2, n=1550*1195)
	close(con)
	sep <- sep[TabID]
	sep[sep == 10000] = NA

	filename=sprintf("/fou/klima/anitavd/klimagrid/annualMax_rr_uncor/rr1d/rrmmax/binary/%4i/rrmmax_%4i_10.bil",year,year)
	print(filename)
	con=file(filename,open="rb")
	oct <- readBin(con, integer(), size=2, n=1550*1195)
	close(con)
	oct <- oct[TabID]
	oct[oct == 10000] = NA

	filename=sprintf("/fou/klima/anitavd/klimagrid/annualMax_rr_uncor/rr1d/rrmmax/binary/%4i/rrmmax_%4i_11.bil",year,year)
	print(filename)
	con=file(filename,open="rb")
	nov <- readBin(con, integer(), size=2, n=1550*1195)
	close(con)
	nov <- nov[TabID]
	nov[nov == 10000] = NA

	filename=sprintf("/fou/klima/anitavd/klimagrid/annualMax_rr_uncor/rr1d/rrmmax/binary/%4i/rrmmax_%4i_12.bil",year,year)
	print(filename)
	con=file(filename,open="rb")
	des <- readBin(con, integer(), size=2, n=1550*1195)
	close(con)
	des <- des[TabID]
	des[des == 10000] = NA

	filename=sprintf("/fou/klima/anitavd/klimagrid/annualMax_rr_uncor/rr1d/rrmmax/binary/%4i/rrmmax_%4i_01.bil",year+1,year+1)
	print(filename)
	con=file(filename,open="rb")
	jan <- readBin(con, integer(), size=2, n=1550*1195)
	close(con)
	jan <- jan[TabID]
	jan[jan == 10000] = NA

	filename=sprintf("/fou/klima/anitavd/klimagrid/annualMax_rr_uncor/rr1d/rrmmax/binary/%4i/rrmmax_%4i_02.bil",year+1,year+1)
	print(filename)
	con=file(filename,open="rb")
	feb <- readBin(con, integer(), size=2, n=1550*1195)
	close(con)
	feb <- feb[TabID]
	feb[feb == 10000] = NA

	filename=sprintf("/fou/klima/anitavd/klimagrid/annualMax_rr_uncor/rr1d/rrmmax/binary/%4i/rrmmax_%4i_03.bil",year+1,year+1)
	print(filename)
	con=file(filename,open="rb")
	mar <- readBin(con, integer(), size=2, n=1550*1195)
	close(con)
	mar <- mar[TabID]
	mar[mar == 10000] = NA

	filename=sprintf("/fou/klima/anitavd/klimagrid/annualMax_rr_uncor/rr1d/rrmmax/binary/%4i/rrmmax_%4i_04.bil",year+1,year+1)
	print(filename)
	con=file(filename,open="rb")
	apr <- readBin(con, integer(), size=2, n=1550*1195)
	close(con)
	apr <- apr[TabID]
	apr[apr == 10000] = NA

	filename=sprintf("/fou/klima/anitavd/klimagrid/annualMax_rr_uncor/rr1d/rrmmax/binary/%4i/rrmmax_%4i_05.bil",year+1,year+1)
	print(filename)
	con=file(filename,open="rb")
	mai <- readBin(con, integer(), size=2, n=1550*1195)
	close(con)
	mai <- mai[TabID]
	mai[mai == 10000] = NA

	for (i in 1:length(TabID)) {

		if (am[i] == jun[i] | am[i] == jul[i] | am[i] == aug[i]) dom[i] <- 1
		if (am[i] == sep[i] | am[i] == oct[i] | am[i] == nov[i]) dom[i] <- 2
		if (am[i] == des[i] | am[i] == jan[i] | am[i] == feb[i]) dom[i] <- 3
		if (am[i] == mar[i] | am[i] == apr[i] | am[i] == mai[i]) dom[i] <- 4

	}

	dom.all[,j] <- dom
	j <- j+1

}

dom.mean <- array(1,dim=(length(TabID)))

for (k in 1:length(TabID)) {

	if (length(which(dom.all[k,] == 2)) > length(which(dom.all[k,] == 1)) ) dom.mean[k] <- 2
	if (length(which(dom.all[k,] == 3)) > length(which(dom.all[k,] == 2)) ) dom.mean[k] <- 3
	if (length(which(dom.all[k,] == 4)) > length(which(dom.all[k,] == 3)) ) dom.mean[k] <- 4

}
	

dom.grid <- array(NA,dim=c(1195,1550))
dom.grid[TabID] <- dom.mean   # back to full grid

filename <- sprintf("/home/anitavd/PhD/Comparison_methods/domPrecip/domPrecip")

writebinfile(dom.grid,paste(filename,".bil",sep=""))

hdrfile <- paste(filename,".hdr",sep="")
blwfile <- paste(filename,".blw",sep="")

system("cp /home/anitavd/GISfile.hdr /home/anitavd/PhD/Comparison_methods/domPrecip/")
system("cp /home/anitavd/GISfile.blw /home/anitavd/PhD/Comparison_methods/domPrecip/")
system(paste("mv /home/anitavd/PhD/Comparison_methods/domPrecip/GISfile.hdr ",hdrfile,sep=""))
system(paste("mv /home/anitavd/PhD/Comparison_methods/domPrecip/GISfile.blw ",blwfile,sep=""))

rm(dom.grid)
rm(dom)
rm(dom.all)
gc(reset=TRUE)

}
