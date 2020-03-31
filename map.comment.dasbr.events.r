library(PAMr)

setwd('D:/LLHARP Detections/Detection Parameters/Calibrated LLHARP Params')

#LLHARP 044 TEST 
LL044_Pcprs<-PAMrSettings(db ='D:/LLHARP Detections/Databases/PAM20016_LLHARP_BANTER_LL044.sqlite3', 
                              binaries = 'D:/LLHARP Detections/Binaries/BANTER_L044')

LL044_Pcprs<-addCalibration(LL044_Pcprs,
                                calFile='F:/transfer_functions/LongLineHARP/638_110701/683_PAMr_opposite.csv',
                                module = "ClickDetector")

LL044<-processPgDetections(LL044_Pcprs,mode="time",grouping="D:/LLHARP Detections/Detection Parameters/LL044_Encounter_Times.csv",
                               format = "%m/%d/%Y %H:%M")

save(LL044Test,file='LL044Test.rDa')

LLHARP044Test_Banter<-export_banter(LL044Test)


load('D:\\Sette_EditedEventTime_Run\\Sette_EditedEventTime_Run\\bantMDL_HICEAS-TowedArray_Sette-editedTime.rdata')
LL044Pred<-predict(bant.mdl,LLHARP044Test_Banter)

LL044CalDet<-getDetectorData(LL044Test)


