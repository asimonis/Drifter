

map.dasbr.events(outfilename = "DASBRmap_Alldrifts", station.numbers=c(4,7,8,10,12,13,14,16,17,18,19,20,21,22,23),
                 speciesID=list(c("ZC"),c("BB","MS"),c("BW43","BW39V","BWC"),c("BW"),c("?BW"),("PM")),
                 MapDir='C:/Users/anne.simonis/Documents/CCE DASBR/code/CCE Map Data',DBDir='C:/Users/anne.simonis/Documents/CCE DASBR/code/CCE Map Data',
                                  spotcsvfile = list(c("DASBRs 1 to 6 - July 4 1700 to Aug 1 1700.csv", "DASBRs 1 to 7 - July 31 1700 to Aug 8 1700.csv",
                                                       "DASBRs 1 to 10 - Aug 8 1700 to Aug 26 1700.csv", "DASBRs 1 to 13 - Aug 26 1700 to Sep 12 1700.csv",
                                                       "DASBRs 1 to 13 - Sep 12 1700 to Oct 3 1700.csv", "DASBRs 1 to 13 - Oct 3 1700 to Oct 23 1700.csv",
                                                       "DASBRs 16 to 22 - Oct 29 1700 to Nov 9 1700.csv",
                                                       "DASBRs 16 to 22 - Nov 8 1600 to Nov 17 1600-rogue location for DASBR 20 removed.csv",
                                                       "DASBRs 16 to 23 - Nov 17 1600 to Dec 3 1600.csv")),
                                  DriftFile='Drift_FileLookup.csv',
                                  shiptrack.xy=NULL, lookupfile="spotlookup_US&MX_RETRIEVED.csv", showStudyBound=FALSE, extent="CCES",figtitle='Drifts 4-23')

#Try to map all drifts without any events 
map.drifts(outfilename = "Alldrifts", station.numbers=c(1:23),
                 MapDir='C:/Users/anne.simonis/Documents/CCE DASBR/code/CCE Map Data',
                 spotcsvfile = list(c("DASBRs 1 to 6 - July 4 1700 to Aug 1 1700.csv", "DASBRs 1 to 7 - July 31 1700 to Aug 8 1700.csv",
                                      "DASBRs 1 to 10 - Aug 8 1700 to Aug 26 1700.csv", "DASBRs 1 to 13 - Aug 26 1700 to Sep 12 1700.csv",
                                      "DASBRs 1 to 13 - Sep 12 1700 to Oct 3 1700.csv", "DASBRs 1 to 13 - Oct 3 1700 to Oct 23 1700.csv",
                                      "DASBRs 16 to 22 - Oct 29 1700 to Nov 9 1700.csv",
                                      "DASBRs 16 to 22 - Nov 8 1600 to Nov 17 1600-rogue location for DASBR 20 removed.csv",
                                      "DASBRs 16 to 23 - Nov 17 1600 to Dec 3 1600.csv")),
                 shiptrack.xy=NULL, lookupfile="spotlookup_US&MX_All.csv", showStudyBound=FALSE, extent="CCES",figtitle='All Drifts')





# Create mini maps for individual drifts using the same color scheme 
Drift.map.dasbr.events(outfilename = "DASBRmap", station.numbers=c(4,7,8,10,12,13,14,16,17,18,19,20,21,22,23),
                       speciesID=list(c("ZC","BB","MS","BW43","BW39V","BWC","BW")),
                       MapDir='C:/Users/anne.simonis/Documents/CCE DASBR/code/CCE Map Data',DBDir='C:/Users/anne.simonis/Documents/CCE DASBR/code/CCE Map Data',
                       spotcsvfile = list(c("DASBRs 1 to 6 - July 4 1700 to Aug 1 1700.csv", "DASBRs 1 to 7 - July 31 1700 to Aug 8 1700.csv",
                                            "DASBRs 1 to 10 - Aug 8 1700 to Aug 26 1700.csv", "DASBRs 1 to 13 - Aug 26 1700 to Sep 12 1700.csv",
                                            "DASBRs 1 to 13 - Sep 12 1700 to Oct 3 1700.csv", "DASBRs 1 to 13 - Oct 3 1700 to Oct 23 1700.csv",
                                            "DASBRs 16 to 22 - Oct 29 1700 to Nov 9 1700.csv",
                                            "DASBRs 16 to 22 - Nov 8 1600 to Nov 17 1600-rogue location for DASBR 20 removed.csv",
                                            "DASBRs 16 to 23 - Nov 17 1600 to Dec 3 1600.csv")),
                       DriftFile='Drift_FileLookup.csv',
                       shiptrack.xy=NULL, lookupfile="spotlookup_US&MX_RETRIEVED.csv",figtitle='CCES:2018')



EventInfoZc<-find.dasbr.events(station.numbers = c(4,7,8,10,12,13,14,16,17,18,19,20,21,22,23), speciesID=list(c("ZC")), 
                              DBDir='C:/Users/anne.simonis/Documents/CCE DASBR/code/CCE Map Data',
                  spotcsvfile = list(c("DASBRs 1 to 6 - July 4 1700 to Aug 1 1700.csv", "DASBRs 1 to 7 - July 31 1700 to Aug 8 1700.csv",
                                       "DASBRs 1 to 10 - Aug 8 1700 to Aug 26 1700.csv", "DASBRs 1 to 13 - Aug 26 1700 to Sep 12 1700.csv",
                                       "DASBRs 1 to 13 - Sep 12 1700 to Oct 3 1700.csv", "DASBRs 1 to 13 - Oct 3 1700 to Oct 23 1700.csv",
                                       "DASBRs 16 to 22 - Oct 29 1700 to Nov 9 1700.csv",
                                       "DASBRs 16 to 22 - Nov 8 1600 to Nov 17 1600-rogue location for DASBR 20 removed.csv",
                                       "DASBRs 16 to 23 - Nov 17 1600 to Dec 3 1600.csv")),DriftFile='Drift_FileLookup.csv',
                  lookupfile="spotlookup_US&MX_RETRIEVED.csv")

EventInfoBb<-find.dasbr.events(station.numbers = c(4,7,8,10,12,13,14,16,17,18,19,20,21,22,23), speciesID=list(c("BB")), 
                               DBDir='C:/Users/anne.simonis/Documents/CCE DASBR/code/CCE Map Data',
                               spotcsvfile = list(c("DASBRs 1 to 6 - July 4 1700 to Aug 1 1700.csv", "DASBRs 1 to 7 - July 31 1700 to Aug 8 1700.csv",
                                                    "DASBRs 1 to 10 - Aug 8 1700 to Aug 26 1700.csv", "DASBRs 1 to 13 - Aug 26 1700 to Sep 12 1700.csv",
                                                    "DASBRs 1 to 13 - Sep 12 1700 to Oct 3 1700.csv", "DASBRs 1 to 13 - Oct 3 1700 to Oct 23 1700.csv",
                                                    "DASBRs 16 to 22 - Oct 29 1700 to Nov 9 1700.csv",
                                                    "DASBRs 16 to 22 - Nov 8 1600 to Nov 17 1600-rogue location for DASBR 20 removed.csv",
                                                    "DASBRs 16 to 23 - Nov 17 1600 to Dec 3 1600.csv")),DriftFile='Drift_FileLookup.csv',
                               lookupfile="spotlookup_US&MX_RETRIEVED.csv")
length(unique(EventInfoBb$Drift))
nrow(EventInfoBb)

EventInfoMs<-find.dasbr.events(station.numbers = c(4,7,8,10,12,13,14,16,17,18,19,20,21,22,23), speciesID=list(c("MS")), 
                               DBDir='C:/Users/anne.simonis/Documents/CCE DASBR/code/CCE Map Data',
                               spotcsvfile = list(c("DASBRs 1 to 6 - July 4 1700 to Aug 1 1700.csv", "DASBRs 1 to 7 - July 31 1700 to Aug 8 1700.csv",
                                                    "DASBRs 1 to 10 - Aug 8 1700 to Aug 26 1700.csv", "DASBRs 1 to 13 - Aug 26 1700 to Sep 12 1700.csv",
                                                    "DASBRs 1 to 13 - Sep 12 1700 to Oct 3 1700.csv", "DASBRs 1 to 13 - Oct 3 1700 to Oct 23 1700.csv",
                                                    "DASBRs 16 to 22 - Oct 29 1700 to Nov 9 1700.csv",
                                                    "DASBRs 16 to 22 - Nov 8 1600 to Nov 17 1600-rogue location for DASBR 20 removed.csv",
                                                    "DASBRs 16 to 23 - Nov 17 1600 to Dec 3 1600.csv")),DriftFile='Drift_FileLookup.csv',
                               lookupfile="spotlookup_US&MX_RETRIEVED.csv")
length(unique(EventInfoMs$Drift))
nrow(EventInfoMs)

EventInfoBW39V<-find.dasbr.events(station.numbers = c(4,7,8,10,12,13,14,16,17,18,19,20,21,22,23), speciesID=list(c("BW39V")), 
                               DBDir='C:/Users/anne.simonis/Documents/CCE DASBR/code/CCE Map Data',
                               spotcsvfile = list(c("DASBRs 1 to 6 - July 4 1700 to Aug 1 1700.csv", "DASBRs 1 to 7 - July 31 1700 to Aug 8 1700.csv",
                                                    "DASBRs 1 to 10 - Aug 8 1700 to Aug 26 1700.csv", "DASBRs 1 to 13 - Aug 26 1700 to Sep 12 1700.csv",
                                                    "DASBRs 1 to 13 - Sep 12 1700 to Oct 3 1700.csv", "DASBRs 1 to 13 - Oct 3 1700 to Oct 23 1700.csv",
                                                    "DASBRs 16 to 22 - Oct 29 1700 to Nov 9 1700.csv",
                                                    "DASBRs 16 to 22 - Nov 8 1600 to Nov 17 1600-rogue location for DASBR 20 removed.csv",
                                                    "DASBRs 16 to 23 - Nov 17 1600 to Dec 3 1600.csv")),DriftFile='Drift_FileLookup.csv',
                               lookupfile="spotlookup_US&MX_RETRIEVED.csv")
length(unique(EventInfoBW39V$Drift))
nrow(EventInfoBW39V)

EventInfoBW43<-find.dasbr.events(station.numbers = c(4,7,8,10,12,13,14,16,17,18,19,20,21,22,23), speciesID=list(c("BW43")), 
                                  DBDir='C:/Users/anne.simonis/Documents/CCE DASBR/code/CCE Map Data',
                                  spotcsvfile = list(c("DASBRs 1 to 6 - July 4 1700 to Aug 1 1700.csv", "DASBRs 1 to 7 - July 31 1700 to Aug 8 1700.csv",
                                                       "DASBRs 1 to 10 - Aug 8 1700 to Aug 26 1700.csv", "DASBRs 1 to 13 - Aug 26 1700 to Sep 12 1700.csv",
                                                       "DASBRs 1 to 13 - Sep 12 1700 to Oct 3 1700.csv", "DASBRs 1 to 13 - Oct 3 1700 to Oct 23 1700.csv",
                                                       "DASBRs 16 to 22 - Oct 29 1700 to Nov 9 1700.csv",
                                                       "DASBRs 16 to 22 - Nov 8 1600 to Nov 17 1600-rogue location for DASBR 20 removed.csv",
                                                       "DASBRs 16 to 23 - Nov 17 1600 to Dec 3 1600.csv")),DriftFile='Drift_FileLookup.csv',
                                  lookupfile="spotlookup_US&MX_RETRIEVED.csv")
length(unique(EventInfoBW43$Drift))
nrow(EventInfoBW43)

EventInfoBWC<-find.dasbr.events(station.numbers = c(4,7,8,10,12,13,14,16,17,18,19,20,21,22,23), speciesID=list(c("BWC")), 
                               DBDir='C:/Users/anne.simonis/Documents/CCE DASBR/code/CCE Map Data',
                               spotcsvfile = list(c("DASBRs 1 to 6 - July 4 1700 to Aug 1 1700.csv", "DASBRs 1 to 7 - July 31 1700 to Aug 8 1700.csv",
                                                    "DASBRs 1 to 10 - Aug 8 1700 to Aug 26 1700.csv", "DASBRs 1 to 13 - Aug 26 1700 to Sep 12 1700.csv",
                                                    "DASBRs 1 to 13 - Sep 12 1700 to Oct 3 1700.csv", "DASBRs 1 to 13 - Oct 3 1700 to Oct 23 1700.csv",
                                                    "DASBRs 16 to 22 - Oct 29 1700 to Nov 9 1700.csv",
                                                    "DASBRs 16 to 22 - Nov 8 1600 to Nov 17 1600-rogue location for DASBR 20 removed.csv",
                                                    "DASBRs 16 to 23 - Nov 17 1600 to Dec 3 1600.csv")),DriftFile='Drift_FileLookup.csv',lookupfile="spotlookup_US&MX_RETRIEVED.csv")

EventInfoPm<-find.dasbr.events(station.numbers = c(4,7,8,10,12,13,14,16,17,18,19,20,21,22,23), speciesID=list(c("PM")), 
                                 DBDir='C:/Users/anne.simonis/Documents/CCE DASBR/code/CCE Map Data',
                                 spotcsvfile = list(c("DASBRs 1 to 6 - July 4 1700 to Aug 1 1700.csv", "DASBRs 1 to 7 - July 31 1700 to Aug 8 1700.csv",
                                                      "DASBRs 1 to 10 - Aug 8 1700 to Aug 26 1700.csv", "DASBRs 1 to 13 - Aug 26 1700 to Sep 12 1700.csv",
                                                      "DASBRs 1 to 13 - Sep 12 1700 to Oct 3 1700.csv", "DASBRs 1 to 13 - Oct 3 1700 to Oct 23 1700.csv",
                                                      "DASBRs 16 to 22 - Oct 29 1700 to Nov 9 1700.csv",
                                                      "DASBRs 16 to 22 - Nov 8 1600 to Nov 17 1600-rogue location for DASBR 20 removed.csv",
                                                      "DASBRs 16 to 23 - Nov 17 1600 to Dec 3 1600.csv")),DriftFile='Drift_FileLookup.csv',
                                 lookupfile="spotlookup_US&MX_RETRIEVED.csv")
length(unique(EventInfoPm$Drift))
nrow(EventInfoPm)


