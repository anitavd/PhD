#def Calc_Coordinates(lon_2transform,lat_2transform):
#  ''' Returns lat lon coordinates on a polar rotated sphere, from the input of the North
#    Pole longitude, and North Pole latitude (that is the rotated position of the pole),
#    and the lon and lat which you wish to transform (as two speperate floating point values).
#    Note   - Currently this has the CORDEX EUR-11 pole shift hardcoded into the routine, as
#    lo_polo = 198. (Cartesian Lontidue of N. Pole Shift), and la_polo = 39.25 (Latitude of N.Pole shift)
#   '''
# latlonlist: list with Stnr Lon Lat

Calc_Coordinates <- function(latlonlist) {
  
  latlonlist <- as.data.frame(latlonlist)
  lo_polo=198                  # Lon and lat of the new position of the north pole
  la_polo=39.25
  rotcoord <- c()
  for (i in 1:length(latlonlist$Stnr)) {  
    lo = latlonlist$X[i]
    la = latlonlist$Y[i]
    lon=lo*(pi/180)               # Transform into radians
    lat=la*(pi/180)
    lon_polo=lo_polo*pi/180       # Transformation into radians
    lat_polo=la_polo*pi/180
    phi=pi-lon_polo               # Calcuus of the angles we are rotating to move
    teta=-(pi/2-lat_polo)         # from the real north pole to the new one
    x=cos(lon)*cos(lat)           # Change in coordinates from lon, lat to cardinates
    y=sin(lon)*cos(lat)
    z=sin(lat)
    xr=cos(teta)*cos(phi)*x-cos(teta)*sin(phi)*y-sin(teta)*z    # Calculus of the new rotated cordinates in cartesians
    yr=sin(phi)*x+cos(phi)*y
    zr=sin(teta)*cos(phi)*x-sin(teta)*sin(phi)*y+cos(teta)*z
    lonr=atan(yr/xr)               # Transformation from cartesians into lon and lat again
    latr=asin(zr)
    #if (lonr < 0.):
    #    lonr=2* pi+lonr            # If the longitude is negative
    rotcoord <- rbind(rotcoord,c(latlonlist$Stnr[i],lonr*180/pi, latr*180/pi))
  }
  colnames(rotcoord) <- c("Stnr","rLon","rLat")
  return(rotcoord)
}