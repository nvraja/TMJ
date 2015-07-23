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
#GData.MDU.dateQuantity$Year <- format(GData.MDU.dateQuantity$Date, "%Y")

# To check the number of days in each year for which data is available:
#table(GData.MDU.dateQuantity$Year)

# ggplot library for plotting:
require(ggplot2)

# Plot quantity of gold sold per day:
ggplot(GData.MDU.dateQuantity[GData.MDU.dateQuantity$Year=="2009",], aes(Date, quantity)) + geom_line()
ggplot(GData.MDU.dateQuantity, aes(Date, quantity)) + geom_line()

# To time series data
require(zoo)
GData.MDU.dateQuantity.Zoo <- read.zoo(GData.MDU.dateQuantity)
#index(GData.MDU.dateQuantity.Zoo) <- GData.MDU.dateQuantityZoo$Date

# Coercing to timeseries
# GData.MDU.dateQuantity.Ts <- ts(GData.MDU.dateQuantity.Zoo)

# Extensible timeseries
require(xts)

GData.MDU.dateQuantity.xts <- as.xts(GData.MDU.dateQuantity.Zoo)
GData.MDU.dateQuantity.Weekly <- apply.weekly(GData.MDU.dateQuantity.xts, sum)
GData.MDU.dateQuantity.Monthly <- apply.monthly(GData.MDU.dateQuantity.xts, sum)

#GData.MDU.dateQuantity.Weekly <- apply.weekly(GData.MDU.dateQuantity.xts, colMeans) # When weekly mean is required

# Plot 
plot(GData.MDU.dateQuantity.Weekly, main="Gold weight in grams for MDU - Weekly", xlab="Week", ylab="grams")
plot(GData.MDU.dateQuantity.Monthly, main="Gold weight in grams for MDU - Monthly", xlab="Month", ylab="grams")

# Seansonal plot
# epoints <- endpoints(GData.MDU.dateQuantity.xts, on = 'months')             
# GData.MDU.dateQuantity.Monthly <- period.apply(x = GData.MDU.dateQuantity.xts, INDEX = epoints, FUN = sum)

# seasonplot(GData.MDU.dateQuantity.Monthly, main="Gold weight in grams for MDU", xlab="week", ylab="grams", year.labels=TRUE, year.labels.left=TRUE, col=1:20, pch=19)

## Auto correlation function (Acf):
# For Monthly data
Acf(window(GData.MDU.dateQuantity.Monthly,start="2009-01-31", end="2015-03-31-.1"), main="The auto correlation function - Monthly data")
# For weekly data (note the end date)
Acf(window(GData.MDU.dateQuantity.Weekly,start="2009-01-31", end="2015-03-29-.1"), main="The auto correlation function - Weekly data")

## Forecasts:
# Average:
meanf(GData.MDU.dateQuantity.Weekly,3)
meanf(GData.MDU.dateQuantity.Monthly,3)

# Naive:
naive(GData.MDU.dateQuantity.Weekly,3)
naive(GData.MDU.dateQuantity.Monthly,3)

rwf(GData.MDU.dateQuantity.Weekly,3)
rwf(GData.MDU.dateQuantity.Monthly,3)

# Seasonal naive:
snaive(GData.MDU.dateQuantity.Weekly,3)
snaive(GData.MDU.dateQuantity.Monthly,3)

# Drift:
rwf(GData.MDU.dateQuantity.Weekly,3, drift=TRUE)
rwf(GData.MDU.dateQuantity.Monthly,3, drift=TRUE)

# Box-cox:
lambda <- BoxCox.lambda(GData.MDU.dateQuantity.Weekly)
plot(BoxCox(GData.MDU.dateQuantity.Weekly,lambda))

# Residual plots:
# Monthly data
residual.snaive.monthly <- residuals(snaive(window(GData.MDU.dateQuantity.Monthly,end="2014-12-31")))
Acf.snaive.monthly <- Acf(residual.snaive.monthly)
hist(residual.snaive.monthly, nclass="FD", main="Histogram of residuals for monthly data")

# Weekly data
plot(window(GData.MDU.dateQuantity.Weekly,end="2014-12-28"))

residual.snaive.weekly <- residuals(snaive(window(GData.MDU.dateQuantity.Weekly,end="2014-12-28")))
Acf.snaive.weekly <- Acf(residual.snaive.weekly)
hist(residual.snaive.weekly, nclass="FD", main="Histogram of residuals for monthly data")

# Box-pierce test: (similar to combined effect of lag in Acf)
Box.test(residual.snaive.monthly, lag=10, fitdf=0)
Box.test(residual.snaive.weekly, lag=10, fitdf=0)

## ==================================

# Unique categories for columns in newdf:
# Metals:
nrow(as.data.frame(table(newdf$itm_MType_vc)))

# Item categories:
nrow(as.data.frame(table(newdf$itm_L2Name_vc)))

# Dataframe ordered items:
x = as.data.frame(table(GData.MDU.Complete$itm_L2Name_vc))

x <- x[order(-x[,2]),]

## Gold - Madurai
# Item category (based on frequency):
GData.MDU.Complete.ItemClass <- GData.MDU.Complete[,c("Date", "itm_L2Name_vc", "quantity")]
GData.MDU.Complete.ItemClass$itm_L2Name_vc <- as.character(GData.MDU.Complete.ItemClass$itm_L2Name_vc)

GData.MDU.Complete.ItemClass$ItemClass <- ifelse(!(GData.MDU.Complete.ItemClass$itm_L2Name_vc %in% c("PLAIN STUD","CHAIN","RING", "DOLLAR", "COIN", "BANGLE", "STONE STUD", "NECKLACE", "BACK CHAIN", "MALAI", "BRACELET", "THALI", "ROUND", "BESARI")), "OTHERS",GData.MDU.Complete.ItemClass$itm_L2Name_vc)

# Group by month-year
require(zoo)

GData.MDU.Complete.ItemClass$MonthYear <- as.yearmon(GData.MDU.Complete.ItemClass$Date)
# GData.MDU.Complete.ItemClass$MonthYear <- as.factor(GData.MDU.Complete.ItemClass$MonthYear)

# Stacked area plot: 
library(dplyr)
# Date wise grouped:
GData.MDU.Complete.ItemClassSummed.Date  <-	GData.MDU.Complete.ItemClass%>%
						group_by(Date, ItemClass)%>%
						summarise(quantitySum =sum(quantity, na.rm=TRUE))
# Month wise grouped:
GData.MDU.Complete.ItemClassSummed.Month  <-	GData.MDU.Complete.ItemClass%>%
						group_by(MonthYear, ItemClass)%>%
						summarise(quantitySum =sum(quantity, na.rm=TRUE))

# Month wise grouped:
GData.MDU.Complete.ItemClassSummed.Month  <-	GData.MDU.Complete.ItemClass%>%
						group_by(MonthYear, ItemClass)%>%
						summarise(quantitySum =sum(quantity, na.rm=TRUE))

# To order by month-year (as such the month-year is character/factor and not time ordered):
GData.MDU.Complete.ItemClassSummed.Month$MonthYear <- as.Date(GData.MDU.Complete.ItemClassSummed.Month$MonthYear)

library(ggplot2)
par(mfrow=c(1,2))

ggplot(GData.MDU.Complete.ItemClassSummed.Date, aes(x=Date, y=quantitySum, group=ItemClass, fill=ItemClass)) + geom_area(position="fill")

ggplot(GData.MDU.Complete.ItemClassSummed.Month, aes(x=MonthYear, y=quantitySum, group=ItemClass, fill=ItemClass)) + geom_area(position="fill")

## Considering only the top items:
# GData.MDU.Complete.ItemClassSummed.Date.filter1 <- filter(GData.MDU.Complete.ItemClassSummed.Date, ItemClass %in% c("PLAIN STUD", "CHAIN", "RING", "DOLLAR", "COIN")) 
GData.MDU.Complete.ItemClassSummed.Month.filter1 <- filter(GData.MDU.Complete.ItemClassSummed.Month, ItemClass %in% c("PLAIN STUD", "CHAIN")) 
GData.MDU.Complete.ItemClassSummed.Month.filter2 <- filter(GData.MDU.Complete.ItemClassSummed.Month, ItemClass %in% c("RING", "DOLLAR", "COIN", "BANGLE", "STONE STUD")) 

ItemTrendGoldMdu1 <- ggplot(GData.MDU.Complete.ItemClassSummed.Month.filter1, aes(x=MonthYear, y=quantitySum, group=ItemClass, fill=ItemClass)) + geom_area(position="fill") + labs(x = "Month-Year", y = "Proportion (%)", title="Major gold items and its proportion over time: MADURAI")

ItemTrendGoldMdu <- ggplot(GData.MDU.Complete.ItemClassSummed.Month.filter2, aes(x=MonthYear, y=quantitySum, group=ItemClass, fill=ItemClass)) + geom_area(position="fill") + labs(x = "Month-Year", y = "Proportion (%)", title="Major gold items and its proportion over time: MADURAI")

pdf()
ItemTrendGoldMdu
dev.off()


### ===============
# Silver - Madurai:
SData.MDU <- newdf[newdf$itm_MType_vc=="SILVER" & newdf$StoreCode=="MDU", ]
SData.MDU.Complete <- SData.MDU[complete.cases(SData.MDU),]

## Items Ordered by frequency:
# Dataframe ordered items:
x = as.data.frame(table(SData.MDU.Complete$itm_L2Name_vc))

x <- x[order(-x[,2]),]

# Item category (based on frequency):
SData.MDU.Complete.ItemClass <- SData.MDU.Complete[,c("Date", "itm_L2Name_vc", "quantity")]
SData.MDU.Complete.ItemClass$itm_L2Name_vc <- as.character(SData.MDU.Complete.ItemClass$itm_L2Name_vc)

SData.MDU.Complete.ItemClass$ItemClass <- ifelse(!(SData.MDU.Complete.ItemClass$itm_L2Name_vc %in% c("BOMBAY KOLUSU", "KUSHBU", "BOMBAY METTI", "SALEM METTI", "ARADHANA", "JEWEL ACPL", "DIVINITI", "TUMBLER", "JEWEL RATE ITEMS", "SILVER COIN", "ORNAMENT WEIGHT", "KAMATCHI VILAKU", "KUTHIYAM KOLUSU", "BOWL")), "OTHERS", SData.MDU.Complete.ItemClass$itm_L2Name_vc)

# Group by month-year
require(zoo)

SData.MDU.Complete.ItemClass$MonthYear <- as.yearmon(SData.MDU.Complete.ItemClass$Date)

# Stacked area plot: 
library(dplyr)

# Month wise grouped:
SData.MDU.Complete.ItemClassSummed.Month  <- SData.MDU.Complete.ItemClass%>%
						group_by(MonthYear, ItemClass)%>%
						summarise(quantitySum =sum(quantity, na.rm=TRUE))

# To order by month-year (as such the month-year is not time ordered):
SData.MDU.Complete.ItemClassSummed.Month$MonthYear <- as.Date(SData.MDU.Complete.ItemClassSummed.Month$MonthYear)

## Plotting: Silver-Madurai
library(ggplot2)

ggplot(SData.MDU.Complete.ItemClassSummed.Date, aes(x=Date, y=quantitySum, group=ItemClass, fill=ItemClass)) + geom_area(position="fill")

ggplot(SData.MDU.Complete.ItemClassSummed.Month, aes(x=MonthYear, y=quantitySum, group=ItemClass, fill=ItemClass)) + geom_area(position="fill")

## Considering only the top items:
# SData.MDU.Complete.ItemClassSummed.Date.filter1 <- filter(SData.MDU.Complete.ItemClassSummed.Date, ItemClass %in% c("PLAIN STUD", "CHAIN", "RING", "DOLLAR", "COIN")) 
SData.MDU.Complete.ItemClassSummed.Month.filter1 <- filter(SData.MDU.Complete.ItemClassSummed.Month, ItemClass %in% c("PLAIN STUD", "CHAIN")) 
SData.MDU.Complete.ItemClassSummed.Month.filter2 <- filter(SData.MDU.Complete.ItemClassSummed.Month, ItemClass %in% c("RING", "DOLLAR", "COIN", "BANGLE", "STONE STUD")) 

ItemTrendSilverMdu1 <- ggplot(SData.MDU.Complete.ItemClassSummed.Month.filter1, aes(x=MonthYear, y=quantitySum, group=ItemClass, fill=ItemClass)) + geom_area(position="fill") + labs(x = "Month-Year", y = "Proportion (%)", title="Major Silver items and its proportion over time: MADURAI")

ItemTrendSilverMdu <- ggplot(SData.MDU.Complete.ItemClassSummed.Month.filter2, aes(x=MonthYear, y=quantitySum, group=ItemClass, fill=ItemClass)) + geom_area(position="fill") + labs(x = "Month-Year", y = "Proportion (%)", title="Major Silver items and its proportion over time: MADURAI")



### ==========================
library(dplyr)

d1 = data.frame(c1=rep(1:2,each=2), c2=rep(LETTERS[1:2],2), c3=rep(5:6, each=2))
d2 = data.frame(c1=rep(1:2,each=2), c2=rep(LETTERS[1:2],2), c3=c(1.4,2.7,3.2,7.7))
d0 <- rbind(d1,d2)

d0%>%
    group_by(c1,c2)%>%
    summarise(summation =sum(c3, na.rm=TRUE))

## ====== 
# Aggregating by month and year
x <- as.POSIXct(c("2011-02-01", "2011-02-01", "2011-02-01","2012-04-01", "2012-04-01", "2012-05-01"))
mo <- strftime(x, "%m")
yr <- strftime(x, "%Y")
amt <- runif(6)
dd <- data.frame(mo, yr, amt)

dd.agg <- aggregate(amt ~ mo + yr, dd, FUN = sum)
dd.agg$date <- as.POSIXct(paste(dd.agg$yr, dd.agg$mo, "01", sep = "-"))
