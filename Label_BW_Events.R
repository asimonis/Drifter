# Rename event ID's for "blind" review by second analyst

# Read PamGuard Databases in SQLite3 
# For beaked whale events, change species ID to "?BW"
# Overwrite database files with generic ID

# Anne Simonis 04 Sept 2019

library(RSQLite)
sqlite <- dbDriver("SQLite")

# choose folder with databases in it
dbFolder<-choose.dir("G:/CCES/CCES PAMGUARD Analyses 2_00_16/JT Analysis/BlindDB")
readline("CAUTION: Database files will be overwritten. Acknowledge you have made a safe copy")

setwd(dbFolder)

# create list of all the SQLite database files within the folder
files= list.files(pattern=glob2rx("*.sqlite3"),recursive=TRUE,full.names=TRUE)
nfiles= length(files)
for (ifile in 1:nfiles) {
  
  cat(ifile," of ",nfiles,"  ",files[ifile],"\n")
  conn <- dbConnect(sqlite,files[ifile])                       #connect to the database
  
  Events <- dbReadTable(conn, "Click_Detector_OfflineEvents")         #read offline events
  Events$eventType<-gsub(" ", "", Events$eventType, fixed = TRUE) #Remove white space from ID labels
  
  BWLabels<-c("ZC|BB|BW43|MS|MD|BW39V|BW70|BWC|IP|?BW|BW|BW26-47")
  OtherLabels<-c("NBHF|PM|SHIP|?GG|GG")
  AllLabels<-paste(BWLabels,OtherLabels,sep="|")
  
  #Check for non-standard labels
  AllInd<-grep(AllLabels,Events$eventType,value=FALSE)
  if(length(AllInd)!=nrow(Events)){
    print("Non-standard labels detected. Go back and review IDs")
  }else{
    
    #Remove non-BW Events
    BW<-grep(BWLabels,Events$eventType,value=FALSE)
    # Events<-Events[BW,]
    
    #Rename all BW events to "?BW" and remove comments
    Events$eventType[BW]<-"?BW"
    Events$comment[BW]<-NA
    Events$colour[BW]<-7
    
    EventFileName= sub(pattern=".sqlite3",replacement="_BWEvents.sqlite3",x=files[ifile])
    
    # write.csv(Events,file=EventFileName)                                #write events to csv file
    dbWriteTable(conn,"Click_Detector_OfflineEvents",Events,overwrite=TRUE)
    dbDisconnect(conn)}
}

