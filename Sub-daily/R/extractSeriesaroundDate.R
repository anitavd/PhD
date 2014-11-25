stnr <- 97250

obs <- read.table(paste("/home/anitavd/PhD/Sub-daily/data/tipping/max10/",stnr,"_hourly_AM10.txt",sep=""),skip=3)
colnames(obs) <- c("STNR","EAST","NORTH","LON","LAT","YEAR","MONTH","DAY","TIME","RR1")

no.obs <- length(obs$STNR)

for (i in 1:no.obs) {

	year <- obs$YEAR[i]
	month <- obs$MONTH[i]
	day <- obs$DAY[i]
	time <- obs$TIME[i]
	rr1 <- obs$RR1[i]

	fd <- paste(15,".",month-1,".",year,sep="")
	td <- paste(15,".",month+1,".",year,sep="")
	if(month==1) fd <- paste(15,".",12,".",year-1,sep="")
	if(month==12) td <- paste(15,".",01,".",year+1,sep="")

	del1 <- "http://klapp/metnopub/production/metno?re=17&p=RR_1&nmt=0&h=0&h=1&h=2&h=3&h=4&h=5&h=6&h=7&h=8&h=9&h=10&h=11&h=12&h=13&h=14&h=15&h=16&h=17&h=18&h=19&h=20&h=21&h=22&h=23&ddel=dot&del=;&ct=text/plain&split=1&nod=0"

	for (n in 1:length(stnr))  {del1 <- paste(del1,"&s=",stnr[n],sep="")}
	del1 <- paste(del1,"&fd=",fd,"&td=",td,sep="")

	data <- read.table(del1,header=T,na.strings="NA")
	data$RR_1[which(data$RR_1==".")]=0.0
	if(is.factor(data$RR_1)) data$RR_1 <- as.numeric(levels(data$RR_1))[data$RR_1]

	idx <- which(data$Year==year & data$Month==month & data$Day==day & data$Time.UTC.== time)
	series <- data$RR_1[(idx-11):(idx+11)]
	rr12 <- 0
	for (j in 1:12) {
		rr12 <- max(rr12,sum(series[j:(j+11)]))
	}

	ratio <- rr1/rr12

	if(ratio <= 0.3) obs$ratio[i] <- 2
	else if(ratio >= 0.7) obs$ratio[i] <- 1
	else obs$ratio[i] <- 0 

}


	
	

	
