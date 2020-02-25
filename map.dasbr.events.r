#Marmap tutorial: https://www.molecularecologist.com/2015/07/marmap/

#Required libraries
library(marmap)
library(RSQLite)
library(RColorBrewer)
library(dplyr)

sqlite <- dbDriver("SQLite")

## FUNCTION map.dasbr.events ##

# Created by AES, 30 August 2019; Adapted from MAP.DASBR2 (by JEM, 3 Sept 2016)
# Function plots the path of a specified DASBR and associated events along its path

# INPUTS to Function:
# station.numbers = a vector.  Each list element contains station numbers from a data file to plot, e.g., station.numbers = c(1,3,7).
# speciesID = a list of the species ID's to include in the plot; e.g. list(c("ZC","BB","BW43","MS","MD","BW39V","BW70","BWC","IP","?BW","BW","BW26-47"))
# spotcsvfile = a list containing names of csv file/s containing date-time-locations for a set of spot devices, created using Advanced Download in the SPOT website
# shiptrack.xy = two-column matrix with longitude and latitude coordinates of the ship's progress, in decimal degrees
# lookup = lookup table indicating spotID and deployment period for each DASBR station.  LOOKUP MUST ONLY INCLUDE DEVICES THAT WILL BE PLOTTED.
# showStudyBound = TRUE to show the CA Current tradtional study boundary, FALSE to suppress this
# extent, defines map extent.  Options are 'CCES' (default; CCES=US + MX), 'US', 'MX' or 'Other'
# figtitle = Title to appear at top of each figure
# SpColor = a list of colors to use to plot event types; defaults to list of 9 contrasting colors
# See example after function code

map.dasbr.events <- function(outfilename, station.numbers = NULL, speciesID, MapDir=NULL,DBDir=NULL,spotcsvfile, 
                             DriftFile='Drift_FileLookup.csv', shiptrack.xy=NULL, lookupfile="spotlookup_US&MX_RETRIEVED.csv", 
                             showStudyBound=TRUE, extent="CCES",figtitle=NULL,SpColor=NULL){
  
  if(is.null(MapDir)){
    MapDir<-choose.dir(caption = "Choose folder containing map data")} 
  if(is.null(DBDir)) {
    DBDir<-choose.dir(caption = "Choose folder containing databases with event info")} 
  if(is.null(figtitle)){
    figtitle<-readline('What are you plotting? (this will be the main title of the figure): ')} 
  setwd(MapDir)
  
  #Create folder to save maps
  dir.create(paste("EventLocations/",Sys.Date(), sep=""), showWarnings = FALSE,recursive = TRUE)

  
  # define boundaries of the map
  if(extent=="CCES"){lon1=-132;lon2=-114;lat1=27;lat2=50}
  if(extent=="US") {lon1=-132;lon2= -116;lat1=30;lat2=50}
  if(extent=="MX") {lon1=-122;lon2= -114;lat1=26;lat2=36}
  if(extent=="Other"){
  lon1<-readline('Please enter the left boundary of the map [-180:180]: ')
  lon2<-readline('Please enter the right boundary of the map [-180:180]: ')
  lat1<-readline('Please enter the top boundary of the map [-90:90]: ')
  lat2<-readline('Please enter the bottom boundary of the map [-90:90]: ')
  lon1<-as.numeric(lon1); lon2<-as.numeric(lon2); lat1<-as.numeric(lat1); lat2<-as.numeric(lat2)
  }
  
  #Extract bathymetry data from NOAA (saves local file to speed future performance)
  bat<-getNOAA.bathy(lon1,lon2,lat1,lat2,resolution=4,keep=TRUE)
  
  #Create color palettes
  blues<-c("royalblue4","royalblue3",
           "royalblue2","royalblue1")
  greys<-c(grey(0.8),grey(0.93),grey(0.99))
  dasbr.ptColor = "black"  # color for DASBR track and deployment points
  SPLabels<-unlist(unique(speciesID))
  
  if(is.null(SpColor)){
    SpColor<-c('red','orange','cyan','yellow','pink','green','lightblue','magenta2','firebrick2')} 

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
  setwd(MapDir)
  DriftDB<-read.csv(DriftFile)
  
  #Create Plot
  for(S in 1:length(speciesID)){
  outfilenameS<-paste(outfilename,S,sep="_")
  plotfile<-paste(MapDir,"/","EventLocations/",Sys.Date(),'/',outfilenameS,'.png', sep="")
  png(plotfile,width=4.5,height=6,units="in",res=300)
  # plotfile<-paste(MapDir,"/","EventLocations/",Sys.Date(),'/',outfilenameS,'.pdf', sep="")
  # pdf(plotfile,width=4.5,height=6)
  plot.bathy(bat,image=TRUE,bpal=list(c(0,max(bat),greys),c(min(bat),0,blues)),land=TRUE,n=0, main=figtitle, deepest.isobath=-500, shallowest.isobath=-500,col='grey32',asp=NA)
  scaleBathy(bat, deg=3.02, x="bottomleft", inset=5)
  
  #Add Study Boundaries (optional)
  if(showStudyBound==TRUE) {
    studyArea_file<-file.choose(new=2)
    print(paste('Study area boundary defined by:',studyArea_file))
    studyArea_dir<-dirname(studyArea_file)
    studyArea <- read.csv(studyArea_file)
    lines(studyArea$LonDD,studyArea$LatDD)
  }
  
  #Loop through data for each station and plot DASBR tracks
  #Offset parameter moves text label slightly away from deployment point; User may need to adjust offset for different map boundaries 
  offset<-0.3
  
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
        lines(data.i.trunc$long, data.i.trunc$lat, col=dasbr.ptColor)  # plot the track
        points(data.i.trunc$long[1], data.i.trunc$lat[1], pch=17,cex=.5) # add points showing DASBR retrievals

      # add points showing DASBR origins
      points(data.i.trunc$long[nrow(data.i.trunc)], data.i.trunc$lat[nrow(data.i.trunc)], col=dasbr.ptColor, pch=15,cex=.5)
      # add station number labels
      text(data.i.trunc$long[nrow(data.i.trunc)]+offset, data.i.trunc$lat[nrow(data.i.trunc)]+offset, labels=stations[i], cex=0.5)
      
      ##Add points showing BW Events
      dbInd<-which(DriftDB$Drift==stations[i])
      
      #Load in appropriate database
      conn <- dbConnect(sqlite,file.path(DBDir,DriftDB$Database[dbInd]))                  
      Events <- dbReadTable(conn, "Click_Detector_OfflineEvents")         #read offline events
      Events$eventType<-gsub(" ", "", Events$eventType, fixed = TRUE)
      
      Events<-filter(Events,eventType %in% speciesID[[S]])
      Events$dateTime<-strptime(Events$UTC,format="%Y-%m-%d %H:%M:%OS")
      Events$dateTime<-as.POSIXct(Events$dateTime)
      
      #Compare Event Start Time with Time in Drift GPS data.frame (data.i.trunc)
      for(e in 1:nrow(Events)){
      TD<-as.numeric(difftime(Events$dateTime[e],data.i.trunc$dateTime,units="min"))
      GPSind<-which.min(abs(TD))
      Events$lat[e]<-data.i.trunc$lat[GPSind]
      Events$long[e]<-data.i.trunc$long[GPSind]}
      
      # add points showing event locations
      for(t in 1:length(unique(speciesID[[S]]))){
      ColInd<-which(SPLabels == speciesID[[S]][t])
      SubEvent<-dplyr::filter(Events,Events$eventType==speciesID[[S]][t])
      points(SubEvent$long, SubEvent$lat, col=SpColor[ColInd], pch=19,cex=.5)
      # points(SubEvent$long, SubEvent$lat, col='LightGray', pch=19,cex=.5)
      }}}}
  
  legend('topright', legend=speciesID[[S]], pch=19,cex=1,pt.cex = 1, col=SpColor[which(SPLabels == speciesID[[S]][1]):ColInd], bg="white")
  
  dev.off()}
  }

# #Examples
# #Map of all retrieved DASBRs
# map.dasbr.events(outfilename = "DASBRmap_Drift_4-23", station.numbers=c(4,7,8,10,12,13,14,16,17,18,19,20,21,22,23),
#                  speciesID=list(c("ZC"),c("BB","MS"),c("BW43","BW39V","BWC"),c("?BW"),c("BW")),
#                  MapDir='C:/Users/anne.simonis/Documents/CCE DASBR/code/CCE Map Data',DBDir='C:/Users/anne.simonis/Documents/CCE DASBR/code/CCE Map Data',
#                  spotcsvfile = list(c("DASBRs 1 to 6 - July 4 1700 to Aug 1 1700.csv", "DASBRs 1 to 7 - July 31 1700 to Aug 8 1700.csv",
#                                       "DASBRs 1 to 10 - Aug 8 1700 to Aug 26 1700.csv", "DASBRs 1 to 13 - Aug 26 1700 to Sep 12 1700.csv",
#                                       "DASBRs 1 to 13 - Sep 12 1700 to Oct 3 1700.csv", "DASBRs 1 to 13 - Oct 3 1700 to Oct 23 1700.csv",
#                                       "DASBRs 16 to 22 - Oct 29 1700 to Nov 9 1700.csv",
#                                       "DASBRs 16 to 22 - Nov 8 1600 to Nov 17 1600-rogue location for DASBR 20 removed.csv",
#                                       "DASBRs 16 to 23 - Nov 17 1600 to Dec 3 1600.csv")),
#                  DriftFile='Drift_FileLookup.csv',
#                  shiptrack.xy=NULL, lookupfile="spotlookup_US&MX_RETRIEVED.csv", showStudyBound=FALSE, extent="CCES",figtitle='CCE:2018')




# 
# outfilename="DASBRmap_DriftAll"
# station.numbers=c(4,7,8,10,12,13,14,16)
# speciesID=list(c("BW43","BW39V","BWC","ZC"))
# MapDir = 'C:/Users/anne.simonis/Documents/CCE DASBR/code/CCE Map Data'
# DBDir='C:/Users/anne.simonis/Documents/CCE DASBR/code/CCE Map Data'
# spotcsvfile = list(c("DASBRs 1 to 6 - July 4 1700 to Aug 1 1700.csv", "DASBRs 1 to 7 - July 31 1700 to Aug 8 1700.csv",
#                      "DASBRs 1 to 10 - Aug 8 1700 to Aug 26 1700.csv", "DASBRs 1 to 13 - Aug 26 1700 to Sep 12 1700.csv",
#                      "DASBRs 1 to 13 - Sep 12 1700 to Oct 3 1700.csv", "DASBRs 1 to 13 - Oct 3 1700 to Oct 23 1700.csv",
#                      "DASBRs 16 to 22 - Oct 29 1700 to Nov 9 1700.csv",
#                      "DASBRs 16 to 22 - Nov 8 1600 to Nov 17 1600-rogue location for DASBR 20 removed.csv",
#                      "DASBRs 16 to 23 - Nov 17 1600 to Dec 3 1600.csv"))
# DriftFile='Drift_FileLookup.csv'
# shiptrack.xy=NULL
# lookupfile="spotlookup_US&MX_RETRIEVED.csv"
# showStudyBound=FALSE
# extent="CCES"
# figtitle="Test"
