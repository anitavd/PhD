


stations <- c(18020,18320,18700,19490,19510,19710)

AM <- seq(1971,2014)  
WF <- seq(1971,2014) 
WI <- seq(1971,2014) 

for(n in 1:length(stations)) {
  station <- stations[n]
  print(station)
  amday <- list(Year=c(),pr=c(),wf=c(),wi=c())   #3-hr pr, wet event frequency, mean wet event intensity
  
  for (year in 1971:2014) {
    
    print(year)
    amday$Year <- c(amday$Year,year)

    data <- "http://klapp/metnopub/production/metno?re=14&p=RR&ddel=dot&del=;&ct=text/plain&split=1&nod=-999"

    FD = paste("01.05.",year,sep="")
    TD = paste("30.09.",year,sep="")

    data <- paste(data,"&fd=",FD,"&td=",TD,"&s=",station,sep="")

    prday <- try(read.table(data, header = TRUE, na.strings = "NA"),silent=T)
    
    if(class(prday)=="try-error") {
      amday$pr = c(amday$pr,NA)
      amday$wf = c(amday$wf,NA)
      amday$wi = c(amday$wi,NA)
      next()
    }
    
    if(is.factor(prday$RR)) {
      pr <- as.numeric(levels(prday$RR))[prday$RR]
    }else {
      pr <- prday$RR
    }
    
    if(length(pr) < 130) {
      amday$pr = c(amday$pr,NA)
      amday$wf = c(amday$wf,NA)
      amday$wi = c(amday$wi,NA)
      next()
    }
    
    pr[which(is.na(pr))] = 0
    
    if(length(pr[which(is.na(pr) | pr==0)]) == length(pr)) {
      amday$pr = c(amday$pr,NA)
      amday$wf = c(amday$wf,NA)
      amday$wi = c(amday$wi,NA)
      next()
    }
    
    am <- max(pr,na.rm=T)
    if(am<10) am <- NA
    ssp <- sum(pr[which(pr>1)])
    wf <- length(which(pr>1))

    amday$pr <- c(amday$pr,am)
    amday$wf <- c(amday$wf,wf)
    amday$wi <- c(amday$wi,round(ssp/wf,digits=2))  
  }
      
  AM <- cbind(AM,amday$pr)
  WF <- cbind(WF,amday$wf)
  WI <- cbind(WI,amday$wi) 
}
