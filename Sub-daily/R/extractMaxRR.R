#Tar ut årlig max timesnedbør fra stasjoner i KDVH

maxRR <- function(Stnr, startYear, endYear, outpath){

noYears <- endYear-startYear+1

max <- list(Stnr=rep(Stnr,noYears),Year=seq(startYear,endYear),RR=rep(NA,noYears))

i<-1

#http://klapp.oslo.dnmi.no/metnopub/production/metno?re=14&tab=DVH_DOGN&p=TAM&p=TAN&p=TAX&fd=13.11.2002&td=12.12.2004&ddel=dot&del=space&ct=text/plain&s=18700&nod=NA&split=1

#ved noen stasjoner finnes det ikke døgnnedbør så man må summere opp timesnedbøren mellom kl 8 og 7.

del1 <- "http://klapp/metnopub/production/metno?re=17&ddel=dot&del=;&ct=text/plain&split=1&nod=-999&p=RR_24&h=7"

for (j in 0:50) {

test <- try(read.table(paste(del1,"&fd=01.01.1957&td=31.12.2011&",Stnr+j,sep=""), header = TRUE, na.strings = "."),T) 
 
 	if(class(test) != "try-error") {
		
		StNr <- Stnr+j
		break()
	}
 }

print(StNr) 

  for (n in 1:length(StNr)){del1 <- paste(del1,"&s=",StNr[n],sep="")}

 for (year in startYear: endYear) {

 del2 <- paste(del1,"&fd=01.01.",year,"&td=31.12.",year,sep="")

  Series <- try(read.table(del2, header = TRUE, na.strings = "."),T)

  if(class(Series) == "try-error") {
	max$RR[i]=NA
	next()
 }

  Series$RR[which(Series$RR==-999)] = NA

  if(length(which(is.na(Series$RR)))==length(Series$RR)) {
	max$RR[i]=NA
 } else {
  	max$RR[i] <- max(Series$RR,na.rm=T)
 }

i <- i+1

}

  filename <- paste(outpath, StNr,"_max_RR24.txt", sep="") 

  write.table(max,filename,sep="\t",row.names=FALSE,quote=FALSE)

  return(max)
}
