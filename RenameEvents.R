#Quick script to rename events after we discovered
#"BW37V" were incorrectly labeled as "BW39V"
#Anne Simonis 24 Feb 2020


library(RSQLite)
sqlite <- dbDriver("SQLite")


setwd('D:/CCES/CCES PAMGUARD Analyses 2_00_16/Databases/Final Databases/')
dbfiles<-list.files()

for(d in 1:length(dbfiles)){
conn <- dbConnect(sqlite,dbfiles[d])                  
Events <- dbReadTable(conn, "Click_Detector_OfflineEvents")         #read offline events
Events$eventType<-gsub(" ", "", Events$eventType, fixed = TRUE)


ToChange<-which(Events$eventType=="BW39V")
Events$eventType[ToChange]<-"BW37V"

dbWriteTable(conn,"Click_Detector_OfflineEvents",Events,overwrite=TRUE)
dbDisconnect(conn)}
