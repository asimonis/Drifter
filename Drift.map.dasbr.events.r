
#2. Create mini map for single drift and large map for single species

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

Drift.map.dasbr.events <- function(outfilename, station.numbers = NULL, speciesID=NULL, MapDir=NULL,DBDir=NULL,spotcsvfile, DriftFile='Drift_FileLookup.csv',
                             shiptrack.xy=NULL, lookupfile="spotlookup_US&MX_RETRIEVED.csv",figtitle=NULL,SpColor=NULL){
  
  if(is.null(MapDir)){
    MapDir<-choose.dir(caption = "Choose folder containing map data")} 
  if(is.null(DBDir)) {
    DBDir<-choose.dir(caption = "Choose folder containing databases with event info")} 
  if(is.null(figtitle)){
    figtitle<-readline('What are you plotting? (this will be the main title of the figure): ')} 
  setwd(MapDir)
  
  #Create folder to save maps
  dir.create(paste("EventLocations/",Sys.Date(), sep=""), showWarnings = FALSE,recursive = TRUE)
  
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
  stations <- unique(station.numbers)
  
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
    #Loop through data for each station and plot DASBR tracks
    #OffsetV and offsetH parameter moves text label slightly away (V=vetical, H=horizontal)...
    #from deployment point; User may need to adjust offset for different map boundaries 
    offsetV<-0.2
    offsetH<-0
    
    for(n in 1:n.stations){
      if(is.na(lookup$dateTimeStart[n*2])) next  # for DASBRS that have not yet been deployed, skip to next record
      if(!is.na(lookup$dateTimeStart[n*2])){  # for DASBRS that have been deployed...
        
        outfilenameN<-paste(outfilename,'Drift',station.numbers[n],sep="_")
       
         # spot data for station n
        data.n <- spotcsv[spotcsv$spotID %in% lookup$spot.number[lookup$station==stations[n]], ]  
        # date-time info (in date time format) for station n
        dateTime.n <- dateTime[spotcsv$spotID %in% lookup$spot.number[lookup$station==stations[n]]]
        
        # truncate data to only include location records while DASBR was deployed, and plot the tracks
        if(!is.na(lookup$dateTimeEnd[n*2])){ # for DASBRs that have been retrieved
          data.n.trunc <- data.n[dateTime.n >= strptime(unique(lookup$dateTimeStart[lookup$station==stations[n]]), "%m/%d/%Y %H:%M")
                                 & dateTime.n <= strptime(unique(lookup$dateTimeEnd[lookup$station==stations[n]]), "%m/%d/%Y %H:%M"), ]
          data.n.trunc$dateTime<-strptime(data.n.trunc$dateTime,format="%m/%d/%Y %H:%M:%OS")
         
          #Remove outliers based on speed between detections (set to 1 km/hour here)
          data.n.trunc<-cutoutliers(data.n.trunc,4)
          if(stations[n] %in% c(14,21)){buffer<-0.5} 
          if(stations[n] %in% c(7,12,16,17,18,20,22,23)){buffer<-1}
          if(stations[n] %in% c(4,8,10,13,19)){buffer<-2}
          lat1<-min(data.n.trunc$lat)-buffer
          lat2<-max(data.n.trunc$lat)+buffer
          lon1<-min(data.n.trunc$lon)-buffer
          lon2<-max(data.n.trunc$lon)+buffer
          
          #Extract bathymetry data from NOAA (saves local file to speed future performance)
          bat<-getNOAA.bathy(lon1,lon2,lat1,lat2,resolution=1,keep=TRUE)
        
          #Plot
          figtitleN = paste(figtitle,"Drift",stations[n],sep=" ")
          plotfile<-paste(MapDir,"/","EventLocations/",Sys.Date(),'/',outfilenameN,'.png', sep="")
          png(plotfile,width=4.5,height=6,units="in",res=300)
          plot.bathy(bat,image=TRUE,bpal=list(c(0,max(bat),greys),c(min(bat),0,blues)),land=TRUE,n=0, main=figtitleN, deepest.isobath=-500, shallowest.isobath=-500,col='grey32',asp=NA)
          scaleBathy(bat, deg=0.12, x="bottomleft", inset=5)

          lines(data.n.trunc$long, data.n.trunc$lat, col=dasbr.ptColor)  # plot the track
          points(data.n.trunc$long[1], data.n.trunc$lat[1], pch=17,cex=1) # add points showing DASBR retrievals
          
          # add points showing DASBR origins
          points(data.n.trunc$long[nrow(data.n.trunc)], data.n.trunc$lat[nrow(data.n.trunc)], col=dasbr.ptColor, pch=15,cex=1)
          # add station number labels
          if(stations[n] %in% c(19)){offsetV<- -0.3}
          if(stations[n] %in% c(18)){offsetV<- -0.2}
          if(stations[n] %in% c(20)){offsetV<- -0.1}
          if(stations[n] %in% c(21)){offsetV<- -0.15}
          if(stations[n] %in% c(14)){offsetV<- 0.1}
          if(stations[n] %in% c(12)){offsetV<- 0.1}
          
          if(stations[n] %in% c(10,13,16)){offsetH<- 0.2}
          if(stations[n] %in% c(23)){offsetH<- 0.1}
          if(stations[n] %in% c(22)){offsetH<- -0.2}
          if(stations[n] %in% c(14)){offsetH<- -0.1}
          if(stations[n] %in% c(12)){offsetH<- 0.1}
           text(data.n.trunc$long[nrow(data.n.trunc)]+offsetV, data.n.trunc$lat[nrow(data.n.trunc)]+offsetH, labels=stations[n], cex=1.5)
          
          ##Add points showing BW Events
          dbInd<-which(DriftDB$Drift==stations[n])
          
          #Load in appropriate database
          conn <- dbConnect(sqlite,file.path(DBDir,DriftDB$Database[dbInd]))                  
          Events <- dbReadTable(conn, "Click_Detector_OfflineEvents")         #read offline events
          Events$eventType<-gsub(" ", "", Events$eventType, fixed = TRUE)
          
          Events<-filter(Events,eventType %in% SPLabels)
          Events$dateTime<-strptime(Events$UTC,format="%Y-%m-%d %H:%M:%OS")
          Events$dateTime<-as.POSIXct(Events$dateTime)
          
          #Compare Event Start Time with Time in Drift GPS data.frame (data.n.trunc)
          for(e in 1:nrow(Events)){
            TD<-as.numeric(difftime(Events$dateTime[e],data.n.trunc$dateTime,units="min"))
            GPSind<-which.min(abs(TD))
            Events$lat[e]<-data.n.trunc$lat[GPSind]
            Events$long[e]<-data.n.trunc$long[GPSind]}
          
          # add points showing event locations
          for(t in 1:length(SPLabels)){
            SubEvent<-dplyr::filter(Events,Events$eventType==SPLabels[t])
            points(SubEvent$long, SubEvent$lat, col=SpColor[t], pch=19,cex=.75)
          }}}
    legend('topright', legend=SPLabels, pch=19,pt.cex = .75, cex=1, col=SpColor, bg="white")
    dev.off()}
}


# # 
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
# figtitle='CCES:2018'
# 


