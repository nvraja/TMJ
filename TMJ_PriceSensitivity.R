# Created Date: 7th May 2015
# Modified: 14th September 2015
# Author: Raja et. al (Churn Data)
# ==========================================
paths = list(Data='/home/raja/Documents/Projects/TMJ/Data_20150504/Data/')

# The combined raw data: Set working directory where the files to be read are...
setwd(paths$Data)
rawDataList <- lapply(2008:2015, function(x) unique(read.csv(paste0("Q",x,x+1,".csv"), header = TRUE, sep=",")))
rawData <- do.call(rbind, rawDataList)
# structure of the data
str(rawData)

# The date-time variable to right format: 
rawData$Date <- as.Date(strptime(rawData$DocDate, "%B %d %Y 12:00AM"))

# Dummy table with all the dates:
seqDate <- list(Date=seq(as.Date("2008-08-20"),as.Date("2015-08-31"), by=1))
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
nrow(rawData[rawData$FINYEAR=="[Q20152016]",])

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
#unitPrice[unitPrice$Date=="2011-10-21" & unitPrice$MType=="GOLD", "BoardRate"] <- "2490.625"

# Filtering Gold:
unitPriceGold <- unitPrice[unitPrice$MType=="GOLD", ]
# Filtering out FY15
unitPriceGold <- unitPriceGold[unitPriceGold$Date<"2015-09-01", ]

# Plotting the gold unit price:
library(ggplot2)
unitPriceGold$BoardRate <- as.numeric(unitPriceGold$BoardRate)
unitPriceVariation <- ggplot(unitPriceGold[unitPriceGold$Date>"2009-01-01" & unitPriceGold$Date<"2015-08-31",], aes(Date, BoardRate)) + geom_line()

# Unit price for all days:
# Dummy table with all the dates:
seqDatePrice <- list(Date=seq(as.Date("2009-11-21"), as.Date("2015-08-31"), by=1))
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

# Outlier Unit price: Dates
outlierUnitPriceDates <- c("2009-12-11", "2009-12-31", "2011-01-28", "2011-08-28", "2011-11-22", "2011-12-21", "2011-12-22", "2012-05-16", "2012-05-19", "2013-05-04")


DailySalesUnitPrice$Date <- as.character(DailySalesUnitPrice$Date)
DailySalesUnitPrice <- DailySalesUnitPrice[!(DailySalesUnitPrice$Date %in% outlierUnitPriceDates), ]
DailySalesUnitPrice$Date <- as.Date(DailySalesUnitPrice$Date)

# Previous day unit price: (Outlier unit price dates removed after this exporation)
# DailySalesUnitPrice[DailySalesUnitPrice$Date>"2011-08-01" & DailySalesUnitPrice$Date<"2011-10-01",]
#"2011-08-25", 2011-09-24, 

DailySalesUnitPrice$goldUnitPricePreviousDay <- lag(DailySalesUnitPrice$goldUnitPrice)
str(DailySalesUnitPrice)

#DailySalesUnitPrice$Difference <- (DailySalesUnitPrice$goldUnitPricePreviousDay - DailySalesUnitPrice$goldUnitPrice)
#DailySalesUnitPrice[abs(DailySalesUnitPrice$Difference)>100,]

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
# 2 days
DailySalesUnitPriceIncreaseDecrease$priceChange2D <- c(rep(0,2), diff(DailySalesUnitPriceIncreaseDecrease$goldUnitPrice, 2))
# 3 days
DailySalesUnitPriceIncreaseDecrease$priceChange3D <- c(rep(0,3), diff(DailySalesUnitPriceIncreaseDecrease$goldUnitPrice, 3))
# 4 days
DailySalesUnitPriceIncreaseDecrease$priceChange4D <- c(rep(0,4), diff(DailySalesUnitPriceIncreaseDecrease$goldUnitPrice, 4))
# 5 days
DailySalesUnitPriceIncreaseDecrease$priceChange5D <- c(rep(0,5), diff(DailySalesUnitPriceIncreaseDecrease$goldUnitPrice, 5))
# 6 days
DailySalesUnitPriceIncreaseDecrease$priceChange6D <- c(rep(0,6), diff(DailySalesUnitPriceIncreaseDecrease$goldUnitPrice, 6))
# 7 days
DailySalesUnitPriceIncreaseDecrease$priceChange7D <- c(rep(0,7), diff(DailySalesUnitPriceIncreaseDecrease$goldUnitPrice, 7))
# 14 days
DailySalesUnitPriceIncreaseDecrease$priceChange14D <- c(rep(0,14), diff(DailySalesUnitPriceIncreaseDecrease$goldUnitPrice, 14))
# 30 days
DailySalesUnitPriceIncreaseDecrease$priceChange30D <- c(rep(0,30), diff(DailySalesUnitPriceIncreaseDecrease$goldUnitPrice, 30))

# ======================================================
## Identifying sales outliers:

# Plots
library(ggplot2)
ggplot(GData.MDU.Complete.Date[GData.MDU.Complete.Date$Date>"2009-01-01" & GData.MDU.Complete.Date$Date<"2010-01-01",], aes(Date, quantitySum)) + geom_line()
ggplot(GData.MDU.Complete.Date[GData.MDU.Complete.Date$Date>"2010-01-01" & GData.MDU.Complete.Date$Date<"2011-01-01",], aes(Date, quantitySum)) + geom_line()
ggplot(GData.MDU.Complete.Date[GData.MDU.Complete.Date$Date>"2011-01-01" & GData.MDU.Complete.Date$Date<"2012-01-01",], aes(Date, quantitySum)) + geom_line()
ggplot(GData.MDU.Complete.Date[GData.MDU.Complete.Date$Date>"2012-01-01" & GData.MDU.Complete.Date$Date<"2013-01-01",], aes(Date, quantitySum)) + geom_line()
ggplot(GData.MDU.Complete.Date[GData.MDU.Complete.Date$Date>"2013-01-01" & GData.MDU.Complete.Date$Date<"2014-01-01",], aes(Date, quantitySum)) + geom_line()
ggplot(GData.MDU.Complete.Date[GData.MDU.Complete.Date$Date>"2014-01-01" & GData.MDU.Complete.Date$Date<"2015-01-01",], aes(Date, quantitySum)) + geom_line()
ggplot(GData.MDU.Complete.Date[GData.MDU.Complete.Date$Date>"2015-01-01" & GData.MDU.Complete.Date$Date<"2015-08-31",], aes(Date, quantitySum)) + geom_line()

GData.MDU.Complete.Date <- as.data.frame(GData.MDU.Complete.Date)

# To get the dates where sales is an outlier: (cutoff based on the earlier plot)
GData.MDU.Complete.Date[GData.MDU.Complete.Date$Date>"2009-01-01" & GData.MDU.Complete.Date$Date<"2010-01-01" & GData.MDU.Complete.Date$quantitySum>8000, ]
GData.MDU.Complete.Date[GData.MDU.Complete.Date$Date>"2010-01-01" & GData.MDU.Complete.Date$Date<"2011-01-01" & GData.MDU.Complete.Date$quantitySum>6000, ]
GData.MDU.Complete.Date[GData.MDU.Complete.Date$Date>"2011-01-01" & GData.MDU.Complete.Date$Date<"2012-01-01" & GData.MDU.Complete.Date$quantitySum>8000, ]
GData.MDU.Complete.Date[GData.MDU.Complete.Date$Date>"2012-01-01" & GData.MDU.Complete.Date$Date<"2013-01-01" & GData.MDU.Complete.Date$quantitySum>6000, ]
GData.MDU.Complete.Date[GData.MDU.Complete.Date$Date>"2013-01-01" & GData.MDU.Complete.Date$Date<"2014-01-01" & GData.MDU.Complete.Date$quantitySum>5000, ]
GData.MDU.Complete.Date[GData.MDU.Complete.Date$Date>"2014-01-01" & GData.MDU.Complete.Date$Date<"2015-01-01" & GData.MDU.Complete.Date$quantitySum>5000, ]
GData.MDU.Complete.Date[GData.MDU.Complete.Date$Date>"2015-01-01" & GData.MDU.Complete.Date$Date<"2015-08-31" & GData.MDU.Complete.Date$quantitySum>5000, ]
GData.MDU.Complete.Date[GData.MDU.Complete.Date$Date>"2015-07-01" & GData.MDU.Complete.Date$Date<"2015-08-31" & GData.MDU.Complete.Date$quantitySum>5000, ]

# Distribution of quantity sold per day:
quantile(GData.MDU.Complete.Date[GData.MDU.Complete.Date$Date>"2009-01-01" & GData.MDU.Complete.Date$Date<"2010-01-01", "quantitySum"], prob=seq(0.95,1,length=11), type=5)
quantile(GData.MDU.Complete.Date[GData.MDU.Complete.Date$Date>"2010-01-01" & GData.MDU.Complete.Date$Date<"2011-01-01", "quantitySum"], prob=seq(0.95,1,length=11), type=5)
quantile(GData.MDU.Complete.Date[GData.MDU.Complete.Date$Date>"2011-01-01" & GData.MDU.Complete.Date$Date<"2012-01-01", "quantitySum"], prob=seq(0.95,1,length=11), type=5)
quantile(GData.MDU.Complete.Date[GData.MDU.Complete.Date$Date>"2012-01-01" & GData.MDU.Complete.Date$Date<"2013-01-01", "quantitySum"], prob=seq(0.95,1,length=11), type=5)
quantile(GData.MDU.Complete.Date[GData.MDU.Complete.Date$Date>"2013-01-01" & GData.MDU.Complete.Date$Date<"2014-01-01", "quantitySum"], prob=seq(0.95,1,length=11), type=5)
quantile(GData.MDU.Complete.Date[GData.MDU.Complete.Date$Date>"2014-01-01" & GData.MDU.Complete.Date$Date<"2015-01-01", "quantitySum"], prob=seq(0.95,1,length=11), type=5)
quantile(GData.MDU.Complete.Date[GData.MDU.Complete.Date$Date>"2015-01-01" & GData.MDU.Complete.Date$Date<"2015-08-31", "quantitySum"], prob=seq(0.95,1,length=11), type=5)
quantile(GData.MDU.Complete.Date[GData.MDU.Complete.Date$Date>"2015-07-01" & GData.MDU.Complete.Date$Date<"2015-08-31", "quantitySum"], prob=seq(0,1,length=11), type=5)

GData.MDU.Complete.Date[GData.MDU.Complete.Date$quantitySum>6000, ]

# Outlier Sales: Dates
outlierSalesDates <- c("2009-04-27", "2009-08-03", "2010-05-16", "2010-07-31", "2010-08-01", "2010-08-03", "2011-05-06", "2011-08-03", "2012-04-24", "2012-08-02", "2013-04-17", "2013-04-19", "2013-04-20", "2013-04-21", "2013-04-22", "2013-05-13", "2013-08-03", "2014-05-02", "2014-08-03", "2015-04-21")

DailySalesUnitPriceIncreaseDecrease$Date <- as.character(DailySalesUnitPriceIncreaseDecrease$Date)

DailySalesUnitPriceIncreaseDecreaseLessOutlier <- DailySalesUnitPriceIncreaseDecrease[!(DailySalesUnitPriceIncreaseDecrease$Date %in% outlierSalesDates), ]

DailySalesUnitPriceIncreaseDecreaseLessOutlier$Date <- as.Date(DailySalesUnitPriceIncreaseDecreaseLessOutlier$Date)

dataFinalizing <- DailySalesUnitPriceIncreaseDecreaseLessOutlier[,]
SalesVariation1 <- ggplot(dataFinalizing[dataFinalizing$Date>"2014-01-01" & dataFinalizing$Date<"2015-08-31",], aes(Date, quantitySum)) + geom_line() + xlab("Date") + ylab("Gold Sales (in grams)") + ggtitle("Sales w.r.t time (since 2014))")
SalesVariation2 <- ggplot(dataFinalizing[dataFinalizing$Date>"2014-01-01" & dataFinalizing$Date<"2015-06-30",], aes(Date, quantitySum)) + geom_line() + xlab("Date") + ylab("Gold Sales (in grams)") + ggtitle("Sales w.r.t time (January 2014 - June 2015))")
SalesVariation3 <- ggplot(dataFinalizing[dataFinalizing$Date>"2010-01-01" & dataFinalizing$Date<"2015-08-31",], aes(Date, quantitySum)) + geom_line() + xlab("Date") + ylab("Gold Sales (in grams)") + ggtitle("Sales w.r.t time (since 2010)")
SalesVariation4 <- ggplot(dataFinalizing[dataFinalizing$Date>"2015-01-01" & dataFinalizing$Date<"2015-08-31",], aes(Date, quantitySum)) + geom_line() + xlab("Date") + ylab("Gold Sales (in grams)") + ggtitle("Sales w.r.t time (2015: January - August)")
SalesVariation5 <- ggplot(dataFinalizing[dataFinalizing$Date>"2015-01-01" & dataFinalizing$Date<"2015-06-30",], aes(Date, quantitySum)) + geom_line() + xlab("Date") + ylab("Gold Sales (in grams)") + ggtitle("Sales w.r.t time (2015: January - June)")

# ======================================================
# previous days sales:
dataFinalizing$PreviousDaySales <- lag(dataFinalizing$quantitySum)
dataFinalizing$PreviousDaySales[1] <- 0

# To obtain moving/rolling average from rollmean function in "zoo" package :
library(zoo)
#Average Sales in the last:
# 2 days:
dataFinalizing$AvgSalesPast2 <- c(rep(NA, 1), rollmean(dataFinalizing$PreviousDaySales,2, align="right"))
# 3 days:
dataFinalizing$AvgSalesPast3 <- c(rep(NA, 2), rollmean(dataFinalizing$PreviousDaySales,3, align="right"))
# 4 days:
dataFinalizing$AvgSalesPast4 <- c(rep(NA, 3), rollmean(dataFinalizing$PreviousDaySales,4, align="right"))
# 5 days:
dataFinalizing$AvgSalesPast5 <- c(rep(NA, 4), rollmean(dataFinalizing$PreviousDaySales,5, align="right"))
# 6 days:
dataFinalizing$AvgSalesPast6 <- c(rep(NA, 5), rollmean(dataFinalizing$PreviousDaySales,6, align="right"))
# 7 days:
dataFinalizing$AvgSalesPast7 <- c(rep(NA, 6), rollmean(dataFinalizing$PreviousDaySales,7, align="right"))
# 14 days:
dataFinalizing$AvgSalesPast14 <- c(rep(NA, 13), rollmean(dataFinalizing$PreviousDaySales,14, align="right"))
# 30 days:
dataFinalizing$AvgSalesPast30 <- c(rep(NA, 29), rollmean(dataFinalizing$PreviousDaySales,30, align="right"))

str(dataFinalizing)
head(dataFinalizing,40)

# ======================================================
## Calendar variables:
# Day of the week:
dataFinalizing$weekday <- weekdays(dataFinalizing$Date)
# Month of the year:
dataFinalizing$month <- months(dataFinalizing$Date)
# Week of the year:
dataFinalizing$weekNumber <- strftime(as.POSIXlt(dataFinalizing$Date), format="%W")

str(dataFinalizing)
head(dataFinalizing,40)

# ======================================================
## A few more exploratory plots:
ggplot(dataFinalizing[dataFinalizing$Date>"2009-01-01" & dataFinalizing$Date<"2015-01-01",], aes(Date, priceChange1D)) + geom_line()
ggplot(dataFinalizing[dataFinalizing$Date>"2009-01-01" & dataFinalizing$Date<"2015-01-01",], aes(Date, priceChange7D)) + geom_line()
ggplot(dataFinalizing[dataFinalizing$Date>"2009-01-01" & dataFinalizing$Date<"2015-01-01",], aes(Date, priceChange14D)) + geom_line()
ggplot(dataFinalizing[dataFinalizing$Date>"2009-01-01" & dataFinalizing$Date<"2015-01-01",], aes(Date, priceChange30D)) + geom_line()

ggplot(dataFinalizing[dataFinalizing$Date>"2009-01-01" & dataFinalizing$Date<"2015-01-01",], aes(Date, goldUnitPrice)) + geom_line()

# 2-D plot of gold unit price and sales:
BoardrateSales1 <- ggplot(dataFinalizing[dataFinalizing$Date>"2014-01-01" & dataFinalizing$Date<"2015-08-30",], aes(goldUnitPrice, quantitySum)) + geom_line()  + xlab("Gold Board Rate") + ylab("Gold Sales (in grams)") + ggtitle("Board rate Vs Sales (January 2014 - August 2015)") + geom_smooth(method = "lm", se=FALSE, color="blue", aes(group=1))

BoardrateSales2 <- ggplot(dataFinalizing[dataFinalizing$Date>"2014-01-01" & dataFinalizing$Date<"2015-06-30",], aes(goldUnitPrice, quantitySum)) + geom_line()  + xlab("Gold Board Rate") + ylab("Gold Sales (in grams)") + ggtitle("Board rate Vs Sales (January 2014 - June 2015)") + geom_smooth(method = "lm", se=FALSE, color="red", aes(group=1))

BoardrateSales3 <- ggplot(dataFinalizing[dataFinalizing$Date>"2013-01-01" & dataFinalizing$Date<"2015-06-30",], aes(goldUnitPrice, quantitySum)) + geom_line()  + xlab("Gold Board Rate") + ylab("Gold Sales (in grams)") + ggtitle("Board rate Vs Sales (January 2013 - June 2015)") + geom_smooth(method = "lm", se=FALSE, color="red", aes(group=1))

# ======================================================
# Copy of data for modelling:
dataTestTrain <- dataFinalizing[dataFinalizing$Date>"2010-01-01" & dataFinalizing$Date<"2015-06-30",]
names(dataTestTrain)

# This is because 53 factors are the max that could be easily handled
dataTestTrain <- dataTestTrain[dataTestTrain$weekNumber!="53",]

# Converting "character" class to "factor" class
i <- sapply(dataTestTrain, is.character)
dataTestTrain[i] <- lapply(dataTestTrain[i], as.factor) 

# ======================================================
# ======================================================
# Modelling the data:
# Sampling to training and test sets:
#install.packages("caTools")
library(caTools)
set.seed(1729)
split = sample.split(dataTestTrain$quantitySum, SplitRatio = 0.8)

dataTrain = subset(dataTestTrain, split==TRUE)
dataTest = subset(dataTestTrain, split==FALSE)

# ======================================================
# The random forest model
#install.packages("randomForest")
library(randomForest)
RF1 <- randomForest(quantitySum ~ .-Date, data=dataTrain, ntree=2000, nodesize=25, do.trace=FALSE)

predictTrainRF1  <- predict(RF1, data=dataTrain)
predictTestRF1  <- predict(RF1, newdata=dataTest)

TrainPredictedRF1 <- cbind(dataTrain, predictTrainRF1)
TestPredictedRF1 <- cbind(dataTest, predictTestRF1)

head(TrainPredictedRF1[,c("Date", "quantitySum", "predictTrainRF1")],20)
head(TestPredictedRF1[,c("Date", "quantitySum", "predictTestRF1")],20)

# R-square for Random Forest
RfTrainSSE = sum((TrainPredictedRF1$quantitySum - TrainPredictedRF1$predictTrainRF1)^2)
RfTrainSST = sum((TrainPredictedRF1$quantitySum - mean(TrainPredictedRF1$quantitySum))^2)
RSquareRF <- (1 - RfTrainSSE/RfTrainSST)
RSquareRF

# R-square for Random Forest
RfTestSSE = sum((TestPredictedRF1$quantitySum - TestPredictedRF1$predictTestRF1)^2)
RfTestSST = sum((TestPredictedRF1$quantitySum - mean(TestPredictedRF1$quantitySum))^2)
RSquareRFTest <- (1 - RfTestSSE/RfTestSST)
RSquareRFTest

# Variable importance in the model:
varImpPlot(RF1)

# Others:
#varUsed(RF1)
#treesize(RF1)

plotDataRFTest1 <- TestPredictedRF1[,c("Date", "quantitySum", "predictTestRF1")]
names(plotDataRFTest1)[2] <- "ActualSales"
names(plotDataRFTest1)[3] <- "PredictedSales"

# Result plots:
library(ggplot2)
RFPlot1 <- ggplot(plotDataRFTest1, aes(Date)) + 
  geom_line(aes(y=ActualSales, colour="ActualSales")) +
  geom_line(aes(y=PredictedSales, colour="PredictedSales")) +
  ggtitle("Sales: Actual vs Predicted (for test data)") +
  scale_colour_manual("", 
                      breaks = c("ActualSales", "PredictedSales"),
                      values = c("ActualSales"="green", "PredictedSales"="blue"))
 
# ======================================================
# Tuning Random Forest and with cross validation - Generally this is computationally intensive (but depends on data point and other parameters)
# Seeding
set.seed(1729)
# install.packages("caret")
# install.packages("e1071")
library(caret)
library(e1071)


## Tuning Random Forest model:
## Using "e1071" package: RF with cross validation (tune.randomForest from package "e1071")
library(e1071)
set.seed(1729)
tunedRF2 <- tune.randomForest(quantitySum ~ .-Date, data=dataTrain, ntree=seq(200,1000,100), nodesize=seq(1,20,1))
summary(RF2)
# best parameters:
# nodesize ntree
#       12   300

## Tuning random forest model using "caret" package: Grid Search for optimal parameter
set.seed(1234)
library(caret)
library(e1071)
# define cross validation: 
fitControl = trainControl(method='cv', number=10)

# Can try the below one as well (can also try to tune the parameters which we can see from output of trainControl before training the model)
# fitControl = trainControl(method='repeatedcv', number=10, repeats=5, classProbs = TRUE, preProcess = "pca", )

rfGrid = expand.grid(.mtry = c(1:10))

# Tuning RF
begin = Sys.time()
tuningRF <- train(quantitySum ~ .-Date, data=dataTrain, method = "rf", trControl = fitControl, tuneGrid = rfGrid )
finish = Sys.time()
finish - begin

# Plot:
# Residuals for Test data:
denRFTest1 <- density(TestPredictedRF1$quantitySum - TestPredictedRF1$predictTestRF1)
plot(denRFTest1, main = "Density plot for residuals/errors")
polygon(denRFTest1, col="green", border="red")

# Line plot Actual vs Predicted over time:

# ======================================================
# Gradient Boosting:
library(gbm)
gbm1 <- gbm(quantitySum ~ .-Date, data=dataTrain,
		distribution="gaussian", 
		n.trees = 1000, 
		interaction.depth = 1,
		bag.fraction = 0.5,
		shrinkage = 0.01,
		n.cores = 4
		)

summary(gbm1)
bestGBMperf1 <- gbm.perf(gbm1, method="OOB")

# Partial Dependence plot:
plot.gbm(gbm1 , i.var=1, bestGBMperf1)
plot.gbm(gbm1 , i.var=2, bestGBMperf1)
plot.gbm(gbm1 , i.var=3, bestGBMperf1)
plot.gbm(gbm1 , i.var=4, bestGBMperf1)
plot.gbm(gbm1 , i.var=5, bestGBMperf1)
plot.gbm(gbm1 , i.var=6, bestGBMperf1)
plot.gbm(gbm1 , i.var=7, bestGBMperf1)
plot.gbm(gbm1 , i.var=8, bestGBMperf1)
plot.gbm(gbm1 , i.var=9, bestGBMperf1)
plot.gbm(gbm1 , i.var=10, bestGBMperf1)

salesPredictedGBMTrain <- predict(gbm1, dataTrain, n.trees = bestGBMperf1, type="response")
salesPredictedGBMTest <- predict(gbm1, dataTest, n.trees = bestGBMperf1, type="response")

summary(salesPredictedGBMTrain)
summary(salesPredictedGBMTest)

TrainPredicted <- cbind(dataTrain, salesPredictedGBMTrain)
TestPredicted <- cbind(dataTest, salesPredictedGBMTest)
#head(TrainPredicted[,c("quantitySum", "salesPredictedGBMTrain")],20)
#head(TestPredicted[,c("quantitySum", "salesPredictedGBMTest")],20)

# R-square for GBM
gbmTrainSSE = sum((TrainPredicted$quantitySum - TrainPredicted$salesPredictedGBMTrain)^2)
gbmTrainSST = sum((TrainPredicted$quantitySum - mean(TrainPredicted$quantitySum))^2)
RSquareTrain <- (1 - gbmTrainSSE/gbmTrainSST)
RSquareTrain

gbmTestSSE = sum((TestPredicted$quantitySum - TestPredicted$salesPredictedGBMTest)^2)
gbmTestSST = sum((TestPredicted$quantitySum - mean(TestPredicted$quantitySum))^2)
RSquareTestGBM1 <- (1 - gbmTestSSE/gbmTestSST)
RSquareTestGBM1

# Result plots:
library(ggplot2)
ggplot(TestPredicted, aes(Date)) + 
  geom_line(aes(y=quantitySum, colour="quantitySum")) +
  geom_line(aes(y=salesPredictedGBMTest, colour="salesPredictedGBMTest"))

#plot(quantitySum ~ salesPredictedGBMTest, data=TestPredicted, main="Actual vs Predicted: GBM Model")

# ======================================================
# ======================================================
# Copy of data for modelling:
dataTestTrain <- dataFinalizing[dataFinalizing$Date>"2010-01-01" & dataFinalizing$Date<"2015-08-31",]
names(dataTestTrain)

# This is because 53 factors are the max that could be easily handled
dataTestTrain <- dataTestTrain[dataTestTrain$weekNumber!="53",]

# Converting "character" class to "factor" class
i <- sapply(dataTestTrain, is.character)
dataTestTrain[i] <- lapply(dataTestTrain[i], as.factor) 

# ======================================================
# OOT validation:
library(caTools)
set.seed(1729)
#split = sample.split(dataTestTrain$quantitySum, SplitRatio = 0.8)

dataTrain = dataTestTrain[dataTestTrain$Date>="2013-01-01" & dataTestTrain$Date<"2014-03-15", ]
dataTest = dataTestTrain[dataTestTrain$Date>="2014-03-15" & dataTestTrain$Date<"2014-03-31", ]

# ======================================================
# The random forest model
#install.packages("randomForest")
library(randomForest)
RF1 <- randomForest(quantitySum ~ .-Date, data=dataTrain, ntree=2000, nodesize=25, do.trace=FALSE)

predictTrainRF1  <- predict(RF1, data=dataTrain)
predictTestRF1  <- predict(RF1, newdata=dataTest)

TrainPredictedRF1 <- cbind(dataTrain, predictTrainRF1)
TestPredictedRF1 <- cbind(dataTest, predictTestRF1)

head(TrainPredictedRF1[,c("Date", "quantitySum", "predictTrainRF1")],20)
head(TestPredictedRF1[,c("Date", "quantitySum", "predictTestRF1")],20)

# R-square for Random Forest
RfTrainSSE = sum((TrainPredictedRF1$quantitySum - TrainPredictedRF1$predictTrainRF1)^2)
RfTrainSST = sum((TrainPredictedRF1$quantitySum - mean(TrainPredictedRF1$quantitySum))^2)
RSquareRF <- (1 - RfTrainSSE/RfTrainSST)
RSquareRF

# R-square for Random Forest
RfTestSSE = sum((TestPredictedRF1$quantitySum - TestPredictedRF1$predictTestRF1)^2)
RfTestSST = sum((TestPredictedRF1$quantitySum - mean(TestPredictedRF1$quantitySum))^2)
RSquareRFTest <- (1 - RfTestSSE/RfTestSST)
RSquareRFTest

# Data:
plotDataRFTest1 <- TestPredictedRF1[,c("Date", "quantitySum", "predictTestRF1")]
names(plotDataRFTest1)[2] <- "ActualSales"
names(plotDataRFTest1)[3] <- "PredictedSales"

#Plots:
varImpPlot(RF1)

library(ggplot2)
RFPlotOOT <- ggplot(plotDataRFTest1, aes(Date)) + 
  geom_line(aes(y=ActualSales, colour="ActualSales")) +
  geom_line(aes(y=PredictedSales, colour="PredictedSales")) +
  ggtitle("Sales: Actual vs Predicted (for out of time validation data)") +
  scale_colour_manual("", 
                      breaks = c("ActualSales", "PredictedSales"),
                      values = c("ActualSales"="green", "PredictedSales"="blue"))

# ======================================================
# Plot
# Board Rate vs Sales:
dataPlot <- dataFinalizing[dataFinalizing$Date>"2013-01-01" & dataFinalizing$Date<"2015-06-30",]
## add extra space to right margin of plot within frame
par(mar=c(5, 4, 4, 6) + 0.1)

#dataFinalizing$Date <- as.Date(dataFinalizing$Date)
## Plot first set of data and draw its axis
plot(dataPlot$Date, dataPlot$goldUnitPrice, pch=16, axes=FALSE, ylim=c(0,3100), xlab="", ylab="", 
   type="b",col="black", main="Unit Price Vs Sales")
axis(2, ylim=c(0,3100),col="black",las=1)  ## las=1 makes horizontal labels
mtext("Gold Unit Price",side=2,line=2.5)
box()

## Allow a second plot on the same graph
par(new=TRUE)

## Plot the second plot and put axis scale on right
plot(dataPlot$Date, dataPlot$quantitySum, pch=15,  xlab="", ylab="", ylim=c(0,6000), 
    axes=FALSE, type="b", col="red")
## a little farther out (line=4) to make room for labels
mtext("Sales",side=4,col="red",line=4) 
axis(4, ylim=c(0,6000), col="red",col.axis="red",las=1)

## Draw the Date axis
#axis(1,pretty(range(dataPlot$Date),10))
#mtext("Date",side=1,col="black",line=2.5)
axis.Date(side=1, dataPlot$Date, format="%d/%m/%Y")

## Add Legend
legend("topleft",legend=c("Unit Price","Gold Sales"),
  text.col=c("black","red"),pch=c(16,15),col=c("black","red"))

