#Marmap tutorial: https://www.molecularecologist.com/2015/07/marmap/

#Required libraries
library(marmap)
library(RColorBrewer)
library(dplyr)
source('C:/Users/anne.simonis/Documents/CCE DASBR/code/cutoutliers.r')

## FUNCTION map.drifts

# Created by AES, 22 February 2020
# Function plots the path of specified DASBR(s) 

# INPUTS to Function:
# station.numbers = a vector.  Each list element contains station numbers from a data file to plot, e.g., station.numbers = c(1,3,7).
# spotcsvfile = a list containing names of csv file/s containing date-time-locations for a set of spot devices, created using Advanced Download in the SPOT website
# shiptrack.xy = two-column matrix with longitude and latitude coordinates of the ship's progress, in decimal degrees
# lookup = lookup table indicating spotID and deployment period for each DASBR station.  LOOKUP MUST ONLY INCLUDE DEVICES THAT WILL BE PLOTTED.
# showStudyBound = TRUE to show the CA Current tradtional study boundary, FALSE to suppress this
# extent, defines map extent.  Options are 'CCES' (default; CCES=US + MX), 'US', 'MX' or 'Other'
# figtitle = Title to appear at top of each figure
# See example after function code

map.drifts <- function(outfilename, station.numbers = NULL, MapDir=NULL,spotcsvfile, 
                              shiptrack.xy=NULL, lookupfile="spotlookup_US&MX_All.csv", 
                             showStudyBound=TRUE, extent="CCES",figtitle=NULL){
  
  if(is.null(MapDir)){
    MapDir<-choose.dir(caption = "Choose folder containing map data")} 
  if(is.null(figtitle)){
    figtitle<-readline('What are you plotting? (this will be the main title of the figure): ')} 
  setwd(MapDir)
  
  #Create folder to save maps
  dir.create(paste("DriftMap/",Sys.Date(), sep=""), showWarnings = FALSE,recursive = TRUE)
  
  
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
  dasbr.ptColor = "Black" 
  
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
  
  #Create Plot
    plotfile<-paste(MapDir,"/","DriftMap/",Sys.Date(),'/',outfilename,'.png', sep="")
    png(plotfile,width=4.5,height=6,units="in",res=300)
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
        # spot data for station i
        data.i <- spotcsv[spotcsv$spotID %in% lookup$spot.number[lookup$station==stations[i]], ]  
        # date-time info (in date time format) for station i
        dateTime.i <- dateTime[spotcsv$spotID %in% lookup$spot.number[lookup$station==stations[i]]]
    
        # truncate data to include locations until DASBR was recovered or lost
        ThisStation<-filter(lookup,station==stations[i])
        if(is.na(ThisStation$dateTimeEnd[1])&&is.na(ThisStation$dateTimeEnd[2])){
          data.i.trunc <- data.i[dateTime.i >= strptime(unique(lookup$dateTimeStart[lookup$station==stations[i]]), "%m/%d/%Y %H:%M"), ]
        dasbr.trackCol<-'Gray'
          }else{
        data.i.trunc <- data.i[dateTime.i >= strptime(unique(lookup$dateTimeStart[lookup$station==stations[i]]), "%m/%d/%Y %H:%M")
                               & dateTime.i <= strptime(unique(lookup$dateTimeEnd[lookup$station==stations[i]]), "%m/%d/%Y %H:%M"), ]
        dasbr.trackCol<-'Black'}
          
           if(nrow(data.i.trunc)<1) next
          data.i.trunc$dateTime<-strptime(data.i.trunc$dateTime,format="%m/%d/%Y %H:%M:%OS")
          
          #Remove outliers based on speed between detections (set to 4 km/hour here)
          data.i.trunc<-cutoutliers(data.i.trunc,4)
          lines(data.i.trunc$long, data.i.trunc$lat, col=dasbr.trackCol)  # plot the track
          # add points showing DASBR origins
          points(data.i.trunc$long[nrow(data.i.trunc)], data.i.trunc$lat[nrow(data.i.trunc)], col=dasbr.ptColor, pch=15,cex=.5)
           # add points showing DASBR retrievals
          points(data.i.trunc$long[1], data.i.trunc$lat[1], pch=17,cex=.5,col=dasbr.ptColor)
          # add station number labels
          text(data.i.trunc$long[nrow(data.i.trunc)]+offset, data.i.trunc$lat[nrow(data.i.trunc)]+offset, labels=stations[i], cex=0.5)
    }
    legend('topright', legend=c("Recovered","Lost"),lty=1,cex=.75, col=c("black","gray"), bg="white")
    dev.off()
}

