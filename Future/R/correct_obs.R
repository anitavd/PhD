if(station==3030) {
  for (k in c(1985,1992,1988,1989,2010,2011,2012,2013)) {
    if(!is.na(am3h$pr[which(am3h$Year==k)]) & (am3h$pr[which(am3h$Year==k)] < (mean(am3h$pr,na.rm=T) - sd(am3h$pr,na.rm=T)))) am3h$pr[which(am3h$Year==k)] = NA
  }
  am3h$pr[which(am3h$Year==2012)] = NA
  am3h$wf[which(am3h$Year==2012)] = NA
  am3h$wi[which(am3h$Year==2012)] = NA
  #msp$pr[which(msp$Year==2012)] = NA
  #wetf$pr[which(wetf$Year==2012)] = NA
}
if(station==3810) {
  for (k in c(1979,1980,1986,1991,1993,1996,1997,1998,1999,2000,2005,2006,2007,2012,2013)) {
    if(!is.na(am3h$pr[which(am3h$Year==k)]) & (am3h$pr[which(am3h$Year==k)] < (mean(am3h$pr,na.rm=T) - sd(am3h$pr,na.rm=T)))) am3h$pr[which(am3h$Year==k)] = NA
  }
  am3h$pr[which(am3h$Year==2000)] = NA
  am3h$wf[which(am3h$Year==2000)] = NA
  am3h$wi[which(am3h$Year==2000)] = NA
  #msp$pr[which(msp$Year==2000)] = NA
  #wetf$pr[which(wetf$Year==2000)] = NA
}
if(station==4781) {
  for (k in c(1971,1982,1996,1998,2009,2010,2011,2012,2013)) {
    if(!is.na(am3h$pr[which(am3h$Year==k)]) & (am3h$pr[which(am3h$Year==k)] < (mean(am3h$pr,na.rm=T) - sd(am3h$pr,na.rm=T)))) am3h$pr[which(am3h$Year==k)] = NA
  }
  am3h$pr[which(am3h$Year==2008)] = NA
  am3h$wf[which(am3h$Year==2008)] = NA
  am3h$wi[which(am3h$Year==2008)] = NA
  #msp$pr[which(msp$Year==2008)] = NA
  #wetf$pr[which(wetf$Year==2008)] = NA
}
if(station==12290) {
  for (k in c(1971,1983,1985,1988,1989,1990,1993,1994,1997,2003,2004,2005,2007,2009,2010:2013)) {
    if(!is.na(am3h$pr[which(am3h$Year==k)]) & (am3h$pr[which(am3h$Year==k)] < (mean(am3h$pr,na.rm=T) - sd(am3h$pr,na.rm=T)))) am3h$pr[which(am3h$Year==k)] = NA
  }
  am3h$pr[which(am3h$Year==1989)] = NA
  am3h$wf[which(am3h$Year==1989)] = NA
  am3h$wi[which(am3h$Year==1989)] = NA
  am3h$pr[which(am3h$Year==1990)] = NA
  am3h$wf[which(am3h$Year==1990)] = NA
  am3h$wi[which(am3h$Year==1990)] = NA
}
if(station==17870) {
  for (k in c(1971:1973)) {
    if(!is.na(am3h$pr[which(am3h$Year==k)]) & (am3h$pr[which(am3h$Year==k)] < (mean(am3h$pr,na.rm=T) - sd(am3h$pr,na.rm=T)))) am3h$pr[which(am3h$Year==k)] = NA
  }
  am3h$pr[which(am3h$Year==1994)] = NA
  am3h$wf[which(am3h$Year==1994)] = NA
  am3h$wi[which(am3h$Year==1994)] = NA
}
if(station==18020) {
  for (k in c(1971:1984,1989,1992,1993,1998,1999,2000,2002)) {
    if(!is.na(am3h$pr[which(am3h$Year==k)]) & (am3h$pr[which(am3h$Year==k)] < (mean(am3h$pr,na.rm=T) - sd(am3h$pr,na.rm=T)))) am3h$pr[which(am3h$Year==k)] = NA
  }
  am3h$pr[which(am3h$Year==1989)] = NA
  am3h$wf[which(am3h$Year==1989)] = NA
  am3h$wi[which(am3h$Year==1989)] = NA
  #msp$pr[which(msp$Year==1989)] = NA
  #wetf$pr[which(wetf$Year==1989)] = NA
}
if(station==18320) {
  for (k in c(1971:1986,1990,1993,1999,2005)) {
    if(!is.na(am3h$pr[which(am3h$Year==k)]) & (am3h$pr[which(am3h$Year==k)] < (mean(am3h$pr,na.rm=T) - sd(am3h$pr,na.rm=T)))) am3h$pr[which(am3h$Year==k)] = NA
  }
  am3h$pr[which(am3h$Year==1990 | am3h$Year==1993)] = NA
  am3h$wf[which(am3h$Year==1990 | am3h$Year==1993)] = NA
  am3h$wi[which(am3h$Year==1990 | am3h$Year==1993)] = NA
  #msp$pr[which(msp$Year==1990 | msp$Year==1993)] = NA
  #wetf$pr[which(wetf$Year==1990 | wetf$Year==1993)] = NA
}
if(station==18701) {
  for (k in c(1972,1981,1998)) {
    if(!is.na(am3h$pr[which(am3h$Year==k)]) & (am3h$pr[which(am3h$Year==k)] < (mean(am3h$pr,na.rm=T) - sd(am3h$pr,na.rm=T)))) am3h$pr[which(am3h$Year==k)] = NA
  }
}
if(station==19490) {
  for (k in c(1972,1982,1983,1989,1990,1993,1994,2010,2011)) {
    if(!is.na(am3h$pr[which(am3h$Year==k)]) & (am3h$pr[which(am3h$Year==k)] < (mean(am3h$pr,na.rm=T) - sd(am3h$pr,na.rm=T)))) am3h$pr[which(am3h$Year==k)] = NA
  }
  am3h$pr[which(am3h$Year==1990)] = NA
  am3h$wf[which(am3h$Year==1990)] = NA
  am3h$wi[which(am3h$Year==1990)] = NA
}
if(station==19510) {
  for (k in c(1979,1982,1983,1993,1994,1995,1997:2008)) {
    if(!is.na(am3h$pr[which(am3h$Year==k)]) & (am3h$pr[which(am3h$Year==k)] < (mean(am3h$pr,na.rm=T) - sd(am3h$pr,na.rm=T)))) am3h$pr[which(am3h$Year==k)] = NA
  }
}
if(station==19710) {
  for (k in c(1971:1984,1986,1995,2002,2007,2011:2013)) {
    if(!is.na(am3h$pr[which(am3h$Year==k)]) & (am3h$pr[which(am3h$Year==k)] < (mean(am3h$pr,na.rm=T) - sd(am3h$pr,na.rm=T)))) am3h$pr[which(am3h$Year==k)] = NA
  }
}
if(station==20300) {
  for (k in c(1972,1985,1990,1993)) {
    if(!is.na(am3h$pr[which(am3h$Year==k)]) & (am3h$pr[which(am3h$Year==k)] < (mean(am3h$pr,na.rm=T) - sd(am3h$pr,na.rm=T)))) am3h$pr[which(am3h$Year==k)] = NA
  }
}
if(station==26890) {
  for (k in c(1991,1993,1996:2013)) {
    if(!is.na(am3h$pr[which(am3h$Year==k)]) & (am3h$pr[which(am3h$Year==k)] < (mean(am3h$pr,na.rm=T) - sd(am3h$pr,na.rm=T)))) am3h$pr[which(am3h$Year==k)] = NA
  }
}
if(station==30310) {
  for (k in c(1972,1993:1997,2001:2013)) {
    if(!is.na(am3h$pr[which(am3h$Year==k)]) & (am3h$pr[which(am3h$Year==k)] < (mean(am3h$pr,na.rm=T) - sd(am3h$pr,na.rm=T)))) am3h$pr[which(am3h$Year==k)] = NA
  }
}
if(station==39150) {
  for (k in c(1971:1974,1981,1982,1983,1984,1985,1988,1989,1990,1991,1992,1993,1994,2008,2009,2012)) {
    if(!is.na(am3h$pr[which(am3h$Year==k)]) & (am3h$pr[which(am3h$Year==k)] < (mean(am3h$pr,na.rm=T) - sd(am3h$pr,na.rm=T)))) am3h$pr[which(am3h$Year==k)] = NA
  }
  am3h$pr[which(am3h$Year==1993 | am3h$Year==1974)] = NA
  am3h$wf[which(am3h$Year==1993 | am3h$Year==1974)] = NA
  am3h$wi[which(am3h$Year==1993 | am3h$Year==1974)] = NA
  #msp$pr[which(msp$Year==1993 | msp$Year==1974)] = NA
  #wetf$pr[which(wetf$Year==1993 | wetf$Year==1974)] = NA
}
if(station==47240) {
  for (k in c(1979,1982,1986,1987,1988,1990,1995,2003:2012)) {
    if(!is.na(am3h$pr[which(am3h$Year==k)]) & (am3h$pr[which(am3h$Year==k)] < (mean(am3h$pr,na.rm=T) - sd(am3h$pr,na.rm=T)))) am3h$pr[which(am3h$Year==k)] = NA
  }
  am3h$pr[which(am3h$Year==2003)] = NA
  am3h$wf[which(am3h$Year==2003)] = NA
  am3h$wi[which(am3h$Year==2003)] = NA
  #msp$pr[which(msp$Year==2003)] = NA
  #wetf$pr[which(wetf$Year==2003)] = NA
}
if(station==60940) {
  for (k in c(1976,1981,1986,1987,1990,1993,1995)) {
    if(!is.na(am3h$pr[which(am3h$Year==k)]) & (am3h$pr[which(am3h$Year==k)] < (mean(am3h$pr,na.rm=T) - sd(am3h$pr,na.rm=T)))) am3h$pr[which(am3h$Year==k)] = NA
  }
  am3h$pr[which(am3h$Year==1995)] = NA
  am3h$wf[which(am3h$Year==1995)] = NA
  am3h$wi[which(am3h$Year==1995)] = NA
  #msp$pr[which(msp$Year==1995)] = NA
  #wetf$pr[which(wetf$Year==1995)] = NA
}
if(station==64300) {
  for (k in c(1971:1973,1980:1984,1986,1987,1988,2002,2008,2009)) {
    if(!is.na(am3h$pr[which(am3h$Year==k)]) & (am3h$pr[which(am3h$Year==k)] < (mean(am3h$pr,na.rm=T) - sd(am3h$pr,na.rm=T)))) am3h$pr[which(am3h$Year==k)] = NA
  }
  am3h$pr[which(am3h$Year==1986 | am3h$Year==1984 | am3h$Year==1988)] = NA
  am3h$wf[which(am3h$Year==1986 | am3h$Year==1984 | am3h$Year==1988)] = NA
  am3h$wi[which(am3h$Year==1986 | am3h$Year==1984 | am3h$Year==1988)] = NA
  #msp$pr[which(msp$Year==1986 | msp$Year==1984 | msp$Year==1988)] = NA
  #wetf$pr[which(wetf$Year==1986 | wetf$Year==1984 | wetf$Year==1988)] = NA
}