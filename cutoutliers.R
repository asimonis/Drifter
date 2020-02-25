# Created by AES, 22 January 2020

#Required libraries
library(geosphere)

#Remove outliers based on speed between locations
# INPUTS to Function:
# data = a dataframe of locations containing dateTime, lat, long
# toofast = maximum allowable speed (km/hour) between detections

cutoutliers<-function(data,toofast){

c= length(data$lat)
dist= distGeo(cbind(data$long[1:(c-1)],data$lat[1:(c-1)]),cbind(data$long[2:c],data$lat[2:c])) / 1000   #distance in km
dist[c]= 0

etime = as.numeric(difftime(data$dateTime[1:(c-1)],data$dateTime[2:c],units="hours"))
etime[c] = 0
speed = dist/etime

outliers= which(speed>toofast)+1

if (length(outliers) > 0) {
  data$lat[outliers]= NA
  data = data[!is.na(data$lat),]
 
}
return(data)
}


