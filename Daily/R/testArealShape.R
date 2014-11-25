library(ismev)

result <- array(NA,dim=c(25,7))
n<-6842983
#n<-6809864
e <- 286932
#e<-41501
XX <- c(e-10000,e-5000,e,e+5000,e+10000)
YY <- c(n-10000,n-5000,n,n+5000,n+10000)

start.year <- 1957
end.year <- 2011

p <- 1

for (l in 5) {
	for (m in 5) {
		X <- XX[l]
		Y <- YY[m]

#outpath <- paste("/win_P/PhD/Research/Comparison_methods/compareMT/testArealShape/newAnalysis/series/",region,"/",sep="")

#Vector with number of days in the months
nodays.vector <- c(31,28,31,30,31,30,31,31,30,31,30,31)

#Find point in binary file
x = round((X - (-75000 + 500)) / 1000, digits = 0) + 1
y = round((Y - (6450000 + 500)) / 1000, digits = 0) + 1

point = (1550 - y) * 1195 + (x - 1) + 1

max.year <- c()

for (year in start.year:end.year) {

	daily <- c()

	for (month in 1:12) {

		nodays <- nodays.vector[month]

		if (month == 2) {
		if(is.integer(((year/4)-1):(year/4))) nodays <-29
		}

		for (day in 1:nodays) {

			rr.area.all <- c()

			#Read precipitation grid
			filename=sprintf("/vol/klimagrid/daily/rr_uncorrected/binary/%4i/%2.2i/rr_%4i_%2.2i_%2.2i.bil",year,month,year,month,day)
			print(filename)
			con=file(filename,open="rb")
			rr.grid <- readBin(con, integer(), size=2, n=1550*1195)
			close(con)

			rr.all <- c()

			#for (j in c(0,10,20,30,40,50,60,70)) { 
			for (j in c(0,9,19,29,39,49,59)) {

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
			rr.all <- c(rr.all,round(mean(rr, na.rm=TRUE),1))
			rm(rr)
			gc(reset=TRUE) 

			}
			rm(rr.grid)
			gc(reset=T)

			daily <- rbind(daily,rr.all)

		}
	}

	max.year <- rbind(max.year,apply(daily,2,max))
	rm(daily)
	gc(reset=TRUE)
}

shape <- c()

for (i in 1:7) {
	shape <- c(shape,gev.fit(max.year[,i],show=F)$mle[3])
}

result[p,] <- shape

p <- p + 1

	}
}




