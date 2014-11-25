

filename=sprintf("/vol/klimagrid/senorge/dem1.bil")
con=file(filename,open="rb")
elev <- readBin(con,integer(),size=2,n=1550*1195)
close(con)
elev[which(elev ==-1)] = NA
dim(elev) <- c(1195,1550)
#xcoord <- seq(1,1195,by=1)
#ycoord <- seq(1550,1,by=-1)
#elev <- elev[,ycoord]


diff <- array(NA,dim=c(1195,1550))

for (i in 2:1550) {

	diff[,i] <- elev[,i]-elev[,i-1]
}

diff <- round((diff + 1500)/10,digits=0)

writebinfile(diff,"~/PhD/test.bil")
