# Date: 7th May 2015
# Author: Raja (mail.nvraja@gmail.com)
# ==========================================
paths = list(Data='/home/raja/Documents/Projects/TMJ/Data_20150504/Data/')

# The combined raw data: Set working directory where the files to be read are...
setwd(paths$Data)
rawDataList <- lapply(2008:2014, function(x) unique(read.csv(paste0("Q",x,x+1,".csv"), header = TRUE, sep=",")))
rawData <- do.call(rbind, rawDataList)
# structure of the data
str(rawData)

# The date-time variable to right format: 
rawData$Date <- as.Date(strptime(rawData$DocDate, "%B %d %Y 12:00AM"))

# Dummy table with all the dates:
seqDate <- list(Date=seq(as.Date("2008-08-20"),as.Date("2015-03-31"), by=1))
seqDate <- as.data.frame(seqDate)

# New dataframe: All dates included and with "NA" as value when not available
newdf <- merge(x=seqDate, y=rawData, by="Date", all.x=T)

# Dates where data is not available: This is either Diwali holiday (2008-10-27, 2009-10-17, 2010-11-05, 2011-10-26, 2012-11-13, 2013-11-02, 2014-10-22) or other day when store is closed.
noData <- newdf[is.na(newdf$FINYEAR), "Date"]
noData<- as.data.frame(noData)

# Distribution:
# Metals:
tapply(newdf$itm_MType_vc ,newdf$FINYEAR, summary)
# as.data.frame(tapply(newdf$itm_MType_vc ,newdf$FINYEAR, summary))

# itm_L2Name_vc:
tapply(newdf$itm_L2Name_vc ,newdf$FINYEAR, summary)
# as.data.frame(tapply(newdf$itm_L2Name_vc ,newdf$FINYEAR, summary))

# Entries per year: Note there are 13 NA's due to no data on date merge (this is also the sum of metal distribution)
nrow(rawData[rawData$FINYEAR=="[Q20082009]",])
nrow(rawData[rawData$FINYEAR=="[Q20092010]",])
nrow(rawData[rawData$FINYEAR=="[Q20102011]",])
nrow(rawData[rawData$FINYEAR=="[Q20112012]",])
nrow(rawData[rawData$FINYEAR=="[Q20122013]",])
nrow(rawData[rawData$FINYEAR=="[Q20132014]",])
nrow(rawData[rawData$FINYEAR=="[Q20142015]",])

# Store wise distribution:
tapply(rawData$Date ,rawData$Project, summary)

# Others:
table(rawData$quantity<0)
#rawData[rawData$quantity<0,]

table(rawData$UOM)
rawData[rawData$UOM=="Gms",c("UOM", "DocEntry", "DocDate")]

# Grouping the Project/Stores:
# require(dplyr)

## A new column "StoreCode" to map projects
newdf$StoreCode <- 0

newdf$StoreCode[newdf$Project=="TUT"| newdf$Project=="GOLD.TUT"|newdf$Project=="SIL.TUT"] <- "TUT"
newdf$StoreCode[newdf$Project=="TNI"| newdf$Project=="SIL.THN"|newdf$Project=="THENI" | newdf$Project=="GOLD.TNI"| newdf$Project=="SIL.TNI"] <- "TNI"
newdf$StoreCode[newdf$Project=="TKS"| newdf$Project=="GOLD.TKS"|newdf$Project=="SIL.TKS"] <- "TKS"
newdf$StoreCode[newdf$Project=="SVK"| newdf$Project=="SIL.SVK"| newdf$Project=="SIVAKASI" | newdf$Project=="GOLD.SVK" ] <- "SVK"
newdf$StoreCode[newdf$Project=="SLM"| newdf$Project=="GOLD.SLM"|newdf$Project=="SIL.SLM"] <- "SLM"
newdf$StoreCode[newdf$Project=="RMD"| newdf$Project=="RAMNAD"| newdf$Project=="SIL.RMD" | newdf$Project=="GOLD.RMD" ] <- "RMD"
newdf$StoreCode[newdf$Project=="RJM"| newdf$Project=="RAJAPALA"| newdf$Project=="SIL.RJM" | newdf$Project=="GOLD.RJM" ] <- "RJM"
newdf$StoreCode[newdf$Project=="PNI"| newdf$Project=="GOLD.PNI"|newdf$Project=="SIL.PNI"] <- "PNI"
newdf$StoreCode[newdf$Project=="MDU"| newdf$Project=="MADURAI"| newdf$Project=="sil.mdu" | newdf$Project=="SIL.MDU" | newdf$Project=="GOLD.MDU" ] <- "MDU"
newdf$StoreCode[newdf$Project=="KKD"| newdf$Project=="KARAIKUD"| newdf$Project=="sil.kar" | newdf$Project=="SIL.KAR" | newdf$Project=="GOLD.KKD" | newdf$Project=="SIL.KKD" ] <- "KKD"
newdf$StoreCode[newdf$Project=="DINDIGUL"| newdf$Project=="SIL.DGL"| newdf$Project=="GOLD.DGL" | newdf$Project=="DGL" ] <- "DGL"

newdf$StoreCode[newdf$Project=="SIL.CUM"| newdf$Project=="GOLD.CUM" | newdf$Project=="CUM" ] <- "CUM"
newdf$StoreCode[newdf$Project=="SIL.CBE"| newdf$Project=="GOLD.CBE" | newdf$Project=="CBE" ] <- "CBE"
newdf$StoreCode[newdf$Project=="SIL.APK"| newdf$Project=="GOLD.APK" | newdf$Project=="APK" ] <- "APK"
newdf$StoreCode[newdf$Project=="SIL.ANR"| newdf$Project=="GOLD.ANR" | newdf$Project=="ANR" ] <- "ANR"

newdf$StoreCode[newdf$Project=="SKL"] <- "SKL"
newdf$StoreCode[newdf$Project=="SGI"] <- "SGI"
newdf$StoreCode[newdf$Project=="OTN"] <- "OTN"
newdf$StoreCode[newdf$Project=="NKL"] <- "NKL"
newdf$StoreCode[newdf$Project=="KLP"] <- "KLP"
newdf$StoreCode[newdf$Project=="KGI"] <- "KGI"
newdf$StoreCode[newdf$Project=="GPM"] <- "GPM"
newdf$StoreCode[newdf$Project=="ERL"] <- "ERL"
newdf$StoreCode[newdf$Project=="ECM"] <- "ECM"
newdf$StoreCode[newdf$Project=="VPM"] <- "VPM"
newdf$StoreCode[newdf$Project=="DPI"] <- "DPI"
newdf$StoreCode[newdf$Project=="TPM"] <- "TPM"
newdf$StoreCode[newdf$Project=="TPR"] <- "TPR"
newdf$StoreCode[newdf$Project=="TVL"] <- "TVL"
newdf$StoreCode[newdf$Project=="UDP"] <- "UDP"
newdf$StoreCode[newdf$Project=="VKL"] <- "VKL"
newdf$StoreCode[newdf$Project=="VLR"] <- "VLR"

newdf$StoreCode[newdf$StoreCode==0] <- "MDU"

# Categories:
data.frame(table(newdf$StoreCode))

# Coercing StoreCode to factor class:
newdf$StoreCode <- as.factor(newdf$StoreCode)

# Time Series:
# Also loads "forecast" and "zoo"
library(fpp) 
# newdfts <- ts(newdf)

# Gold:
GData.MDU <- newdf[newdf$itm_MType_vc=="GOLD" & newdf$StoreCode=="MDU", ]
GData.MDU.Complete <- GData.MDU[complete.cases(GData.MDU),]

# Gold weight: day/date wise
GData.MDU.dateQuantity <- aggregate(quantity ~ Date, data=GData.MDU.Complete, FUN=sum, na.rm=TRUE)
GData.MDU.dateQuantity$Year <- format(GData.MDU.dateQuantity$Date, "%Y")

# To check the number of days in each year for which data is available:
table(GData.MDU.dateQuantity$Year)

# ggplot library for plotting:
require(ggplot2)

# Plot quatity of gold sold per day:
ggplot(GData.MDU.dateQuantity[GData.MDU.dateQuantity$Year=="2009",], aes(Date, quantity)) + geom_line()


