#CCE DASBR analysis 2019
#Make a confusion matrix from initial and final species ID
#Anne Simonis 10/30/2019

#Required libraries
library(xlsx)
library(caret)

options(java.parameters = "- Xmx4g")

setwd('D:/CCES/CCES PAMGUARD Analyses 2_00_16/Databases/')

Drift4<-read.xlsx('CCES species ID review_FINAL.xlsx',sheetName='Drift-4',startRow =2, header=TRUE,colIndex = c(4,6))
Drift7<-read.xlsx('CCES species ID review_FINAL.xlsx',sheetName='Drift-7',startRow =2, header=TRUE,colIndex = c(4,6))
Drift8<-read.xlsx('CCES species ID review_FINAL.xlsx',sheetName='Drift-8',startRow =2, header=TRUE,colIndex = c(4,6))
Drift10<-read.xlsx('CCES species ID review_FINAL.xlsx',sheetName='Drift-10',startRow =2, header=TRUE,colIndex = c(4,6))
Drift12<-read.xlsx('CCES species ID review_FINAL.xlsx',sheetName='Drift-12',startRow =2, header=TRUE,colIndex = c(4,6))
Drift13<-read.xlsx('CCES species ID review_FINAL.xlsx',sheetName='Drift-13',startRow =2, header=TRUE,colIndex = c(4,6))
Drift15<-read.xlsx('CCES species ID review_FINAL.xlsx',sheetName='Drift-15',startRow =2, header=TRUE,colIndex = c(4,6))
Drift16<-read.xlsx('CCES species ID review_FINAL.xlsx',sheetName='Drift-16',startRow =2, header=TRUE,colIndex = c(4,6))
Drift17<-read.xlsx('CCES species ID review_FINAL.xlsx',sheetName='Drift-17',startRow =2, header=TRUE,colIndex = c(4,6))
Drift18<-read.xlsx('CCES species ID review_FINAL.xlsx',sheetName='Drift-18',startRow =2, header=TRUE,colIndex = c(4,6))
Drift19<-read.xlsx('CCES species ID review_FINAL.xlsx',sheetName='Drift-19',startRow =2, header=TRUE,colIndex = c(4,6))
Drift20<-read.xlsx('CCES species ID review_FINAL.xlsx',sheetName='Drift-20',startRow =2, header=TRUE,colIndex = c(4,6))
Drift21<-read.xlsx('CCES species ID review_FINAL.xlsx',sheetName='Drift-21',startRow =2, header=TRUE,colIndex = c(4,6))
Drift22<-read.xlsx('CCES species ID review_FINAL.xlsx',sheetName='Drift-22',startRow =2, header=TRUE,colIndex = c(4,6))
Drift23<-read.xlsx('CCES species ID review_FINAL.xlsx',sheetName='Drift-23',startRow =2, header=TRUE,colIndex = c(4,6))


AllDrifts<-rbind(Drift4,Drift7,Drift8,Drift10,Drift12,Drift13,Drift15,Drift16,Drift17,Drift18,Drift19,Drift20,Drift21,Drift22,Drift23)
AllDrifts$initial.eventType <- gsub(" ", "", AllDrifts$initial.eventType)
AllDrifts$finalized.eventType <- gsub(" ", "", AllDrifts$finalized.eventType)

AllDrifts$initial.eventType<-factor(AllDrifts$initial.eventType,levels=c( "ZC","BB","MS","BW39V","BW43","BWC","BW70","BW","?BW"))
AllDrifts$finalized.eventType<-factor(AllDrifts$finalized.eventType,levels=c( "ZC","BB","MS","BW39V","BW43","BWC","BW70","BW","?BW"))

CM<-confusionMatrix(AllDrifts$initial.eventType,AllDrifts$finalized.eventType)

write.table(as.table(CM$table),file='CM_CCES.csv')
