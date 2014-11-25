##########################################################
# Reads hourly precipitation from KDVH and computes
# annual max daily precipitation for GEV analysis
# AVD, MET, Jun-2014
##########################################################

stations <- c(18020,18320,18701,19490,19510,19710)   #Osloarea
  
  AM <- seq(1971,2014)  
  WF <- seq(1971,2014) 
  WI <- seq(1971,2014) 
  
  for(n in 1:length(stations)) {
    station <- stations[n]
    print(station)
    am24h <- list(Year=c(),pr=c(),wf=c(),wi=c())   #3-hr pr, wet event frequency, mean wet event intensity
    
    for (year in 1971:2014) {
      
      print(year)
      am24h$Year <- c(am24h$Year,year)
      
      data <- "http://klapp/metnopub/production/metno?re=17&nmt=0&p=RR_1&h=0&h=1&h=2&h=3&h=4&h=5&h=6&h=7&h=8&h=9&h=10&h=11&h=12&h=13&h=14&h=15&h=16&h=17&h=18&h=19&h=20&h=21&h=22&h=23&ddel=dot&del=;&ct=text/plain&split=1&nod=NA"
      FD = paste("01.05.",year,sep="")
      TD = paste("30.09.",year,sep="")
      data <- paste(data,"&fd=",FD,"&td=",TD,"&s=",station,sep="")
      
      pr1h <- try(read.table(data, header = TRUE, na.strings = "NA"),silent=T)
      
      if(class(pr1h)=="try-error") {
        am3h$pr = c(am3h$pr,NA)
        am3h$wf = c(am3h$wf,NA)
        am3h$wi = c(am3h$wi,NA)
        next()
      }
      
      if(is.factor(pr1h$RR_1)) {
        pr <- as.numeric(levels(pr1h$RR_1))[pr1h$RR_1]
      }else {
        pr <- pr1h$RR_1
      }
      if(length(pr) < 3000) {
        am24h$pr = c(am24h$pr,NA)
        am24h$wf = c(am24h$wf,NA)
        am24h$wi = c(am24h$wi,NA)
        next()
      }
      
      pr[which(is.na(pr))] = 0
      
      if(length(pr[which(is.na(pr) | pr==0)]) == length(pr)) {
        am24h$pr = c(am24h$pr,NA)
        am24h$wf = c(am24h$wf,NA)
        am24h$wi = c(am24h$wi,NA)
        next()
      }
      
      am <- 0
      sm <- 0
      fm <- 0
      ssp <- 0   #sum of summer precipitation
      wf <- 0 #number of wet events
      
      for (i in seq(1,length(pr)-23,by=24)) {
        
        pr24h <- sum(pr[i:(i+23)])
        am <- max(pr24h,am,na.rm=T)
        if(pr24h > 1) {
          ssp <- ssp + pr24h
          wf <- wf + 1
        }
      }
      if(am<10) {
        am <- NA
        wf <- NA
      }
      am24h$pr <- c(am24h$pr,am)
      am24h$wf <- c(am24h$wf,wf)
      am24h$wi <- c(am24h$wi,round(ssp/wf,digits=2))
    }
#    source("~/PhD//Future//R/correct_obs.R")
    
    AM <- cbind(AM,am24h$pr)
    WF <- cbind(WF,am24h$wf)
    WI <- cbind(WI,am24h$wi)
    
  }

#write.table(AM,"~/PhD//Future//Data//Obs//Osloarea_AM_pr24h_19712014.txt",row.names=F,quote=F)
#write.table(WF,"~/PhD//Future//Data//Obs//Osloarea_WF_pr24h_19712014.txt",row.names=F,quote=F)
#write.table(WI,"~/PhD//Future//Data//Obs//Osloarea_WI_pr24h_19712014.txt",row.names=F,quote=F)
