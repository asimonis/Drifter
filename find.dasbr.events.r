
#Required libraries
library(RSQLite)
library(dplyr)

sqlite <- dbDriver("SQLite")

## FUNCTION find.dasbr.events ##

# Created by AES, 04 Feb 2020
# Function finds locations of species of interset

# INPUTS to Function:
# station.numbers = a vector.  Each list element contains station numbers from a data file to plot, e.g., station.numbers = c(1,3,7).
# speciesID = a list of the species ID's to include in the plot; e.g. list(c("ZC","BB","BW43","MS","MD","BW39V","BW70","BWC","IP","?BW","BW","BW26-47"))
# DBDir = directory containing sqlite databases from Pamguard
# spotcsvfile = a list containing names of csv file/s containing date-time-locations for a set of spot devices, created using Advanced Download in the SPOT website
# DriftFile = a csv file listing the database file for each drift
# lookup = lookup table indicating spotID and deployment period for each DASBR station.  LOOKUP MUST ONLY INCLUDE DEVICES THAT WILL BE PLOTTED.

find.dasbr.events <- function(station.numbers = NULL, speciesID, DBDir=NULL,spotcsvfile,DriftFile,lookupfile){
 SpDF<-data.frame()
 
  if(is.null(DBDir)) {
    DBDir<-choose.dir(caption = "Choose folder containing databases with event info")} 
  setwd(DBDir)
  
  #Load in lookup file
  lookup = read.csv(lookupfile)
  ncsvfiles = length(spotcsvfile[[1]])
  stations <- unique(lookup$station)
  
  # combine GPS locations from multiple csv files into a single table, with most recent data at the top
  spotcsv = read.csv(spotcsvfile[[1]][ncsvfiles],header=FALSE)
  if(ncsvfiles>1){
    for(p in (ncsvfiles-1):1){spotcsv = rbind(spotcsv,read.csv(spotcsvfile[[1]][p],header=FALSE))}
    if(!is.null(station.numbers)){  # if user has specified only a subset of DASBRs, this filters the data accordingly
      spotcsv = spotcsv[spotcsv$V2 %in% lookup$spot.number[lookup$station %in% station.numbers] , ]
    }}
  
  colnames(spotcsv) = c("dateTime", "spotID", "readingType", "lat", "long")
  dateTime = strptime(spotcsv$dateTime, "%m/%d/%Y %H:%M")
  n.stations = length(station.numbers)
  
  #Read in Drift Database lookup for Event Info
  DriftDB<-read.csv(DriftFile)
  
  #Loop through stations
    for(i in 1:n.stations){
      if(is.na(lookup$dateTimeStart[i*2])) next  # for DASBRS that have not yet been deployed, skip to next record
      if(!is.na(lookup$dateTimeStart[i*2])){  # for DASBRS that have been deployed...
        # spot data for station i
        data.i <- spotcsv[spotcsv$spotID %in% lookup$spot.number[lookup$station==stations[i]], ]  
        # date-time info (in date time format) for station i
        dateTime.i <- dateTime[spotcsv$spotID %in% lookup$spot.number[lookup$station==stations[i]]]
        # truncate data to only include location records while DASBR was deployed, and plot the tracks
        if(!is.na(lookup$dateTimeEnd[i*2])){ # for DASBRs that have been retrieved
          data.i.trunc <- data.i[dateTime.i >= strptime(unique(lookup$dateTimeStart[lookup$station==stations[i]]), "%m/%d/%Y %H:%M")
                                 & dateTime.i <= strptime(unique(lookup$dateTimeEnd[lookup$station==stations[i]]), "%m/%d/%Y %H:%M"), ]
          data.i.trunc$dateTime<-strptime(data.i.trunc$dateTime,format="%m/%d/%Y %H:%M:%OS")
          
          #Remove outliers based on speed between detections (set to 4 km/hour here)
          data.i.trunc<-cutoutliers(data.i.trunc,4)
 
          ##Find and Save BW Events
          #Load in appropriate database
          dbInd<-which(DriftDB$Drift==stations[i])
          conn <- dbConnect(sqlite,file.path(DBDir,DriftDB$Database[dbInd]))                  
          Events <- dbReadTable(conn, "Click_Detector_OfflineEvents")         #read offline events
          Events$eventType<-gsub(" ", "", Events$eventType, fixed = TRUE)
          
          Events<-filter(Events,eventType %in% speciesID[[1]])
          Events$dateTime<-strptime(Events$UTC,format="%Y-%m-%d %H:%M:%OS")
          Events$dateTime<-as.POSIXct(Events$dateTime)
          if(nrow(Events)>0){
          #Compare Event Start Time with Time in Drift GPS data.frame (data.i.trunc)
          for(e in 1:nrow(Events)){
            TD<-as.numeric(difftime(Events$dateTime[e],data.i.trunc$dateTime,units="min"))
            GPSind<-which.min(abs(TD))
            Events$lat[e]<-data.i.trunc$lat[GPSind]
            Events$long[e]<-data.i.trunc$long[GPSind]
            Events$Drift[e]<-station.numbers[i]}
          
          if(nrow(SpDF)<1){SpDF<-Events}else{SpDF<-rbind(SpDF,Events)}}
          remove(Events)
        }}}
  SpDF<-select(SpDF,dateTime,lat,long,eventType,nClicks, Drift)
  return(SpDF)
}


