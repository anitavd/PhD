

source("/klimadata/applikasjon/gridding/src/iobin.R")
source("/klimadata/applikasjon/gridding/src/met_stat.R")

domPrecip_max <- function() {

#Read file with indexes for non-NA values (new version)
TabID <- as.numeric(as.matrix(read.table("/home/anitavd/scripts/TabID.txt",header=TRUE)))

no.years <- 2010 - 1957 + 1

max.sum <- array(0,dim=(length(TabID)))
max.aut <- array(0,dim=(length(TabID)))

for (year in 1957:2010) {

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

	for (i in 1:length(TabID)) {

		max.sum[i] <- max(max.sum[i],max(jun[i],jul[i],aug[i]))
		max.aut[i] <- max(max.aut[i],max(sep[i],oct[i],nov[i]))

	}

}

dom <- array(2,dim=(length(TabID)))
dom[which(max.sum > max.aut)] <- 1

dom.grid <- array(NA,dim=c(1195,1550))
dom.grid[TabID] <- dom   # back to full grid

filename <- sprintf("/home/anitavd/PhD/Comparison_methods/domPrecip/domPrecip_max")

writebinfile(dom.grid,paste(filename,".bil",sep=""))

hdrfile <- paste(filename,".hdr",sep="")
blwfile <- paste(filename,".blw",sep="")

system("cp /home/anitavd/GISfile.hdr /home/anitavd/PhD/Comparison_methods/domPrecip/")
system("cp /home/anitavd/GISfile.blw /home/anitavd/PhD/Comparison_methods/domPrecip/")
system(paste("mv /home/anitavd/PhD/Comparison_methods/domPrecip/GISfile.hdr ",hdrfile,sep=""))
system(paste("mv /home/anitavd/PhD/Comparison_methods/domPrecip/GISfile.blw ",blwfile,sep=""))

rm(dom.grid)
rm(dom)
gc(reset=TRUE)

}
