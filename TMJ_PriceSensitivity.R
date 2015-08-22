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

# Plotting the gold unit price:
library(ggplot2)
unitPriceGold$BoardRate <- as.numeric(unitPriceGold$BoardRate)
ggplot(unitPriceGold[unitPriceGold$Date>"2009-01-01" & unitPriceGold$Date<"2015-01-01",], aes(Date, BoardRate)) + geom_line()

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
# 7 days
DailySalesUnitPriceIncreaseDecrease$priceChange7D <- c(rep(0,2), diff(DailySalesUnitPriceIncreaseDecrease$goldUnitPrice, 2))
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


GData.MDU.Complete.Date <- as.data.frame(GData.MDU.Complete.Date)

# To get the dates where sales is an outlier: (cutoff based on the earlier plot)
GData.MDU.Complete.Date[GData.MDU.Complete.Date$Date>"2009-01-01" & GData.MDU.Complete.Date$Date<"2010-01-01" & GData.MDU.Complete.Date$quantitySum>8000, ]
GData.MDU.Complete.Date[GData.MDU.Complete.Date$Date>"2010-01-01" & GData.MDU.Complete.Date$Date<"2011-01-01" & GData.MDU.Complete.Date$quantitySum>6000, ]
GData.MDU.Complete.Date[GData.MDU.Complete.Date$Date>"2011-01-01" & GData.MDU.Complete.Date$Date<"2012-01-01" & GData.MDU.Complete.Date$quantitySum>8000, ]
GData.MDU.Complete.Date[GData.MDU.Complete.Date$Date>"2012-01-01" & GData.MDU.Complete.Date$Date<"2013-01-01" & GData.MDU.Complete.Date$quantitySum>6000, ]
GData.MDU.Complete.Date[GData.MDU.Complete.Date$Date>"2013-01-01" & GData.MDU.Complete.Date$Date<"2014-01-01" & GData.MDU.Complete.Date$quantitySum>5000, ]
GData.MDU.Complete.Date[GData.MDU.Complete.Date$Date>"2014-01-01" & GData.MDU.Complete.Date$Date<"2015-01-01" & GData.MDU.Complete.Date$quantitySum>5000, ]

# Distribution of quantity sold per day:
quantile(GData.MDU.Complete.Date[GData.MDU.Complete.Date$Date>"2009-01-01" & GData.MDU.Complete.Date$Date<"2010-01-01", "quantitySum"], prob=seq(0.95,1,length=11), type=5)
quantile(GData.MDU.Complete.Date[GData.MDU.Complete.Date$Date>"2010-01-01" & GData.MDU.Complete.Date$Date<"2011-01-01", "quantitySum"], prob=seq(0.95,1,length=11), type=5)
quantile(GData.MDU.Complete.Date[GData.MDU.Complete.Date$Date>"2011-01-01" & GData.MDU.Complete.Date$Date<"2012-01-01", "quantitySum"], prob=seq(0.95,1,length=11), type=5)
quantile(GData.MDU.Complete.Date[GData.MDU.Complete.Date$Date>"2012-01-01" & GData.MDU.Complete.Date$Date<"2013-01-01", "quantitySum"], prob=seq(0.95,1,length=11), type=5)
quantile(GData.MDU.Complete.Date[GData.MDU.Complete.Date$Date>"2013-01-01" & GData.MDU.Complete.Date$Date<"2014-01-01", "quantitySum"], prob=seq(0.95,1,length=11), type=5)
quantile(GData.MDU.Complete.Date[GData.MDU.Complete.Date$Date>"2014-01-01" & GData.MDU.Complete.Date$Date<"2015-01-01", "quantitySum"], prob=seq(0.95,1,length=11), type=5)


GData.MDU.Complete.Date[GData.MDU.Complete.Date$quantitySum>6000, ]

# Outlier Sales: Dates
outlierSalesDates <- c("2009-04-27", "2009-08-03", "2010-05-16", "2010-07-31", "2010-08-01", "2010-08-03", "2011-05-06", "2011-08-03", "2012-04-24", "2012-08-02", "2013-04-17", "2013-04-19", "2013-04-20", "2013-04-21", "2013-04-22", "2013-05-13", "2013-08-03", "2014-05-02", "2014-08-03")

DailySalesUnitPriceIncreaseDecrease$Date <- as.character(DailySalesUnitPriceIncreaseDecrease$Date)

DailySalesUnitPriceIncreaseDecreaseLessOutlier <- DailySalesUnitPriceIncreaseDecrease[!(DailySalesUnitPriceIncreaseDecrease$Date %in% outlierSalesDates), ]

DailySalesUnitPriceIncreaseDecreaseLessOutlier$Date <- as.Date(DailySalesUnitPriceIncreaseDecreaseLessOutlier$Date)

dataFinalizing <- DailySalesUnitPriceIncreaseDecreaseLessOutlier[,]
ggplot(dataFinalizing[dataFinalizing$Date>"2010-01-01" & dataFinalizing$Date<"2011-01-01",], aes(Date, quantitySum)) + geom_line()
# ======================================================
# previous days sales:
dataFinalizing$PreviousDaySales <- lag(dataFinalizing$quantitySum)
dataFinalizing$PreviousDaySales[1] <- 0

# To obtain moving/rolling average from rollmean function in "zoo" package :
library(zoo)
#Average Sales in the last:
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

# ======================================================
# Copy of data for modelling:
dataTestTrain <- dataFinalizing[dataFinalizing$Date>"2010-01-01" & dataFinalizing$Date<"2014-04-01",]
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

predictRF1  <- predict(RF1, data=dataTest)

TrainPredicted <- cbind(dataTest, predictRF1)

# R-square for Random Forest
RfTrainSSE = sum((dataTrain$quantitySum - dataTrain$predictRF1)^2)
RfTrainSST = sum((TrainPredicted$quantitySum - mean(TrainPredicted$quantitySum))^2)
R-Square <- (1 - RfTrainSSE/RfTrainSST)

# Tuning Random Forest and with cross validation - Generally this is computationally intensive
# Seeding
set.seed(2)
# install.packages("caret")
# install.packages("e1071")
library(caret)
library(e1071)

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

predictTest1 <- predict(gbm1, dataTest, n.trees = bestGBMperf1)

summary(predictTest1)

