# Created Date: 7th May 2015
# Modified: 17th August 2015
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

## Store and Metal: Gold for Madurai
# Gold for Madurai:
GData.MDU <- newdf[newdf$itm_MType_vc=="GOLD" & newdf$StoreCode=="MDU", ]
GData.MDU.Complete <- GData.MDU[complete.cases(GData.MDU),]

# ======================================================
# Unit price
# Load unit price data (has both Gold and Silver price)
unitPrice <- read.csv("unitPrice.csv", header=T, sep=",", stringsAsFactors=F)
unitPrice$Date <- as.Date(strptime(unitPrice$CDate, "%B %d %Y 12:00AM"))

unitPrice[unitPrice$Date>"2011-10-18" & unitPrice$Date<"2011-10-25", ]

head(unitPrice[unitPrice$MType=="GOLD", ])


# Distribution of unit price:
# Gold
quantile(unitPrice[unitPrice$MType=="GOLD", "BoardRate"], prob= seq(0,1, length=21), type=5)
# Silver
quantile(unitPrice[unitPrice$MType=="SILVER", "BoardRate"], prob= seq(0,1, length=21), type=5)

unitPrice[unitPrice$BoardRate>4000, c("Date", "BoardRate")]

# Imputing for 2015-10-21(outlier) with mean:
unitPrice[unitPrice$Date=="2011-10-21" & unitPrice$MType=="GOLD", "BoardRate"] <- "2490.625"

# Filtering Gold:
unitPriceGold <- unitPrice[unitPrice$MType=="GOLD", ]
# Filtering out FY15
unitPriceGold <- unitPriceGold[unitPriceGold$Date<"2015-04-01", ]

# Unit price for all days:
# Dummy table with all the dates:
seqDatePrice <- list(Date=seq(as.Date("2009-11-21"), as.Date("2015-03-31"), by=1))
seqDatePrice <- as.data.frame(seqDatePrice)
# summary(seqDatePrice$Date)

# Finding the date for which unit price in not available
newdfPrice <- merge(x=seqDatePrice, y=unitPriceGold, by="Date", all.x=T)

# No unit price data:
noDataPrice <- newdfPrice[is.na(newdfPrice$BoardRate), "Date"]
noDataPrice <- as.data.frame(noDataPrice)

# Imputing NAs with previous/latest available price:
DateAll <- newdfPrice$BoardRate
#sum(is.na(DateAll))
for (i in 2:dim(newdfPrice)[1]) {
  if(is.na(newdfPrice$BoardRate[i])) (DateAll[i]=DateAll[i-1]) 
}

#sum(is.na(DateAll))

# Converting unit price to numeric
newdfPrice$goldUnitPrice <- as.numeric(DateAll)

## Gold Unit price for all days
goldUnitPrice <- newdfPrice[,c("Date", "goldUnitPrice")]

# ======================================================
## Sales data
# Date wise grouped:
library(dplyr)
GData.MDU.Complete.Date  <-	GData.MDU.Complete%>%
				group_by(Date)%>%
				summarise(quantitySum =sum(quantity, na.rm=TRUE))

str(GData.MDU.Complete.Date)
# ======================================================
## Merging daily sales with unit price data
DailySalesUnitPrice <- merge(GData.MDU.Complete.Date, goldUnitPrice, by="Date")

str(DailySalesUnitPrice)

# Previous day unit price:
DailySalesUnitPrice$goldUnitPricePreviousDay <- lag(DailySalesUnitPrice$goldUnitPrice)
str(DailySalesUnitPrice)

# ======================================================
## Including variables
#library(dplyr)
DailySalesUnitPriceChange <- mutate(DailySalesUnitPrice, Change=ifelse(goldUnitPrice>lag(goldUnitPrice),1,ifelse(goldUnitPrice==lag(goldUnitPrice),0,-1L)))

str(DailySalesUnitPriceChange)
DailySalesUnitPriceChange$Change[1] <- 0
head(DailySalesUnitPriceChange,25)

# Counter code:
# No. of +ve/neutral streak
increase <- rep(0, dim(DailySalesUnitPriceChange)[1])
if(DailySalesUnitPriceChange$Change[1]==1) (inc[1]=1) else increase[1]=1
for (i in 2:dim(DailySalesUnitPriceChange)[1]) {
  if(DailySalesUnitPriceChange$goldUnitPrice[i]>=DailySalesUnitPriceChange$goldUnitPrice[i-1]) (increase[i]=increase[i-1]+1) else increase[i]=0
}

DailySalesUnitPriceIncrease <- cbind(DailySalesUnitPriceChange, increase)
str(DailySalesUnitPriceIncrease)

# No. of -ve/neutral streak
decrease <- rep(0, dim(DailySalesUnitPriceChange)[1])
if(DailySalesUnitPriceChange$Change[1]==1) (decrease[1]=1) else decrease[1]=1
for (i in 2:dim(DailySalesUnitPriceChange)[1]) {
  if(DailySalesUnitPriceChange$goldUnitPrice[i]<=DailySalesUnitPriceChange$goldUnitPrice[i-1]) (decrease[i]=decrease[i-1]+1) else decrease[i]=0
}

DailySalesUnitPriceIncreaseDecrease <- cbind(DailySalesUnitPriceIncrease, decrease)
str(DailySalesUnitPriceIncreaseDecrease)
head(DailySalesUnitPriceIncreaseDecrease,50)

# Change in unit price w.r.t to last:
# one day
DailySalesUnitPriceIncreaseDecrease$priceChange1D <- c(0, diff(DailySalesUnitPriceIncreaseDecrease$goldUnitPrice, 1))
# 7 days
DailySalesUnitPriceIncreaseDecrease$priceChange7D <- c(rep(0,2), diff(DailySalesUnitPriceIncreaseDecrease$goldUnitPrice, 2))
# 14 days
DailySalesUnitPriceIncreaseDecrease$priceChange14D <- c(rep(0,14), diff(DailySalesUnitPriceIncreaseDecrease$goldUnitPrice, 14))
# 30 days
DailySalesUnitPriceIncreaseDecrease$priceChange30D <- c(rep(0,30), diff(DailySalesUnitPriceIncreaseDecrease$goldUnitPrice, 30))

# ======================================================
# previous days sales:
DailySalesUnitPriceIncreaseDecrease$PreviousDaySales <- lag(DailySalesUnitPriceIncreaseDecrease$quantitySum)
DailySalesUnitPriceIncreaseDecrease$PreviousDaySales[1] <- 0

# To obtain moving/rolling average from rollmean function in "zoo" package :
library(zoo)
#Average Sales in the last:
# 7 days:
DailySalesUnitPriceIncreaseDecrease$AvgSalesPast7 <- c(rep(NA, 6), rollmean(DailySalesUnitPriceIncreaseDecrease$PreviousDaySales,7, align="right"))
# 14 days:
DailySalesUnitPriceIncreaseDecrease$AvgSalesPast14 <- c(rep(NA, 13), rollmean(DailySalesUnitPriceIncreaseDecrease$PreviousDaySales,14, align="right"))
# 30 days:
DailySalesUnitPriceIncreaseDecrease$AvgSalesPast30 <- c(rep(NA, 29), rollmean(DailySalesUnitPriceIncreaseDecrease$PreviousDaySales,30, align="right"))

str(DailySalesUnitPriceIncreaseDecrease)
head(DailySalesUnitPriceIncreaseDecrease,40)
# ======================================================
# Copy of the dataset
dailySales <- DailySalesUnitPriceIncreaseDecrease[,]

# ======================================================
## Calendar variables:
# Day of the week:
dailySales$weekday <- weekdays(dailySales$Date)
# Month of the year:
dailySales$month <- months(dailySales$Date)
# Week of the year:
dailySales$weekNumber <- strftime(as.POSIXlt(dailySales$Date), format="%W")



