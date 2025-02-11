require(reshape2)
require(data.table)

dfile <- "repdata-data-StormData.csv"

DT <- read.table(dfile, header=TRUE, sep=",")

DT$BGN_DATE <- as.Date(DT$BGN_DATE, format="%m/%d/%Y")
DT$year <- format(DT$BGN_DATE, format="%Y")
# Exploratory analysis
names(DT)
sort(table(DT$PROPDMGEXP), decreasing=TRUE)
sort(table(DT$CROPDMGEXP), decreasing=TRUE)

# initialize three new variables to sum up $$ damage
DT$CROPS <- as.numeric(NA)
DT$PROP <- as.numeric(NA)
DT$TOTDMG <- as.numeric(NA)

# Convert property damage to absolute value, not the abbreviated value with the exponential symbol
irows <- which(DT$PROPDMGEXP=="" | DT$PROPDMGEXP=="0")
DT$PROP[irows] <- DT$PROPDMG[irows]*1e0
irows <- which(DT$PROPDMGEXP=="K" | DT$PROPDMGEXP=="3" | DT$PROPDMGEXP=="k")
DT$PROP[irows] <- DT$PROPDMG[irows]*1e3
irows <- which(DT$PROPDMGEXP=="m" | DT$PROPDMGEXP=="6" | DT$PROPDMGEXP=="M")
DT$PROP[irows] <- DT$PROPDMG[irows]*1e6
irows <- which(DT$PROPDMGEXP=="B" | DT$PROPDMGEXP=="9" | DT$PROPDMGEXP=="b")
DT$PROP[irows] <- DT$PROPDMG[irows]*1e9
irows <- which(DT$PROPDMGEXP=="H" | DT$PROPDMGEXP=="2")
DT$PROP[irows] <- DT$PROPDMG[irows]*1e2
irows <- which(DT$PROPDMGEXP=="1")
DT$PROP[irows] <- DT$PROPDMG[irows]*1e1
irows <- which(DT$PROPDMGEXP=="4")
DT$PROP[irows] <- DT$PROPDMG[irows]*1e4
irows <- which(DT$PROPDMGEXP=="5")
DT$PROP[irows] <- DT$PROPDMG[irows]*1e5
irows <- which(DT$PROPDMGEXP=="7")
DT$PROP[irows] <- DT$PROPDMG[irows]*1e7
irows <- which(DT$PROPDMGEXP=="8")
DT$PROP[irows] <- DT$PROPDMG[irows]*1e8

# Convert crops damage to absolute value, not the abbreviated value with the exponential symbol
irows <- which(DT$CROPDMGEXP=="" | DT$CROPDMGEXP=="0")
DT$CROPS[irows] <- DT$CROPDMG[irows]*1e0
irows <- which(DT$CROPDMGEXP=="K" | DT$CROPDMGEXP=="3" | DT$CROPDMGEXP=="k")
DT$CROPS[irows] <- DT$CROPDMG[irows]*1e3
irows <- which(DT$CROPDMGEXP=="m" | DT$CROPDMGEXP=="6" | DT$CROPDMGEXP=="M")
DT$CROPS[irows] <- DT$CROPDMG[irows]*1e6
irows <- which(DT$CROPDMGEXP=="B" | DT$CROPDMGEXP=="9" | DT$CROPDMGEXP=="b")
DT$CROPS[irows] <- DT$CROPDMG[irows]*1e9
irows <- which(DT$CROPDMGEXP=="H" | DT$CROPDMGEXP=="2" | DT$CROPDMGEXP=="h")
DT$CROPS[irows] <- DT$CROPDMG[irows]*1e2
irows <- which(DT$CROPDMGEXP=="1")
DT$CROPS[irows] <- DT$CROPDMG[irows]*1e1
irows <- which(DT$CROPDMGEXP=="4")
DT$CROPS[irows] <- DT$CROPDMG[irows]*1e4
irows <- which(DT$CROPDMGEXP=="5")
DT$CROPS[irows] <- DT$CROPDMG[irows]*1e5
irows <- which(DT$CROPDMGEXP=="7")
DT$CROPS[irows] <- DT$CROPDMG[irows]*1e7
irows <- which(DT$CROPDMGEXP=="8")
DT$CROPS[irows] <- DT$CROPDMG[irows]*1e8

# Sum up crops and property damage for all events
DT$TOTDMG <- DT$CROPS + DT$PROP

sum(DT$TOTDMG, na.rm=TRUE)/1e9
DT[which(DT$PROPDMGEXP=="B"),]

# # melt the data table by year and event type
# mDT <- melt(DT, id=c("year", "EVTYPE"), measure=c("FATALITIES", "INJURIES", "CROPS", "PROP", "TOTDMG"))
# 
# mDTtotdmg <- melt(DT, id=c("year", "EVTYPE"), measure="TOTDMG")
# dcast.data.table(mDTtotdmg, variable ~ year, fun=sum)
# dcast.data.table(mDTtotdmg, variable ~ year + EVTYPE, fun=sum)
# dcast.data.table(mDTtotdmg, variable ~ EVTYPE, fun=sum), decreasing=TRUE
# dcast.data.table(mDT, TOTDMG ~ year + EVTYPE, fun=sum)

#Summary of total damage (Crops & Property, in Billion $$) by event type
TDMGbyType <- aggregate(TOTDMG ~ EVTYPE, DT, sum)
TDMGbyType <- TDMGbyType[order(TDMGbyType$TOTDMG, decreasing=TRUE),]
row.names(TDMGbyType) <- NULL
TDMGbyType$TOTDMG <- round(TDMGbyType$TOTDMG/1e9, 2)
head(TDMGbyType, 10)

nlist <- as.character(TDMGbyType$EVTYPE[1:20])
nlist <- c(nlist, "ALL OTHER EVENT TYPES")
yvals <- c(TDMGbyType$TOTDMG[1:20], sum(TDMGbyType$TOTDMG[21:nrow(TDMGbyType)]))

par(mar=c(5, 15, 3, 1)+0.1)
barplot(yvals, names.arg=nlist, horiz=TRUE, las=1, xlab="$Billion", 
        main="Total Crops and Property Damage by Weather Event Type, 1950-2011",
        cex.main=0.8)

#Summary of property damage (in Billion $$) by event type
PDMGbyType <- aggregate(PROP ~ EVTYPE, DT, sum)
PDMGbyType <- PDMGbyType[order(PDMGbyType$PROP, decreasing=TRUE),]
row.names(PDMGbyType) <- NULL
PDMGbyType$PROP <- round(PDMGbyType$PROP/1e9, 2)
head(PDMGbyType, 10)

#Summary of crop damage (in Billion $$) by event type
CTDMGbyType <- aggregate(CROPS ~ EVTYPE, DT, sum)
CTDMGbyType <- CTDMGbyType[order(CTDMGbyType$CROPS, decreasing=TRUE),]
row.names(CTDMGbyType) <- NULL
CTDMGbyType$CROPS <- round(CTDMGbyType$CROPS/1e9, 2)
head(CTDMGbyType, 10)

#Summary of total damage by year, all events
YearlyDMG <- aggregate(TOTDMG ~ year, DT, sum)
YearlyDMG <- YearlyDMG[order(YearlyDMG$TOTDMG, decreasing=TRUE),]
row.names(YearlyDMG) <- NULL
YearlyDMG$year  <- as.Date(YearlyDMG$year, format="%Y")
YearlyDMG$TOTDMG <- YearlyDMG$TOTDMG/1e9

plot(YearlyDMG$year, YearlyDMG$TOTDMG, ylab="Total Yearly Damage, $Billion", xlab="Year", pch=16)


#Summary of effect on population health (deaths and injuries)
#Fatalities
EventFatalities <- aggregate(FATALITIES ~ EVTYPE, DT, sum)
EventFatalities <- EventFatalities[order(EventFatalities$FATALITIES, decreasing=TRUE),]
row.names(EventFatalities) <- NULL
head(EventFatalities, 10)

nlist <- as.character(EventFatalities$EVTYPE[1:20])
nlist <- c(nlist, "ALL OTHER EVENT TYPES")
yvals <- c(EventFatalities$FATALITIES[1:20], sum(EventFatalities$FATALITIES[21:nrow(EventFatalities)]))
barplot(yvals, names.arg=nlist, horiz=TRUE, las=1)

YearlyFatalities <- aggregate(FATALITIES ~ year, DT, sum)
YearlyFatalities <- YearlyFatalities[order(YearlyFatalities$FATALITIES, decreasing=TRUE),]
row.names(YearlyFatalities) <- NULL
YearlyFatalities$year  <- as.Date(YearlyFatalities$year, format="%Y")
plot(YearlyFatalities$year, YearlyFatalities$FATALITIES)

#Injuries
EventInjuries <- aggregate(INJURIES ~ EVTYPE, DT, sum)
EventInjuries <- EventInjuries[order(EventInjuries$INJURIES, decreasing=TRUE),]
row.names(EventInjuries) <- NULL
head(EventInjuries, 10)

nlist <- as.character(EventInjuries$EVTYPE[1:20])
nlist <- c(nlist, "ALL OTHER EVENT TYPES")
yvals <- c(EventInjuries$INJURIES[1:20], sum(EventInjuries$INJURIES[21:nrow(EventInjuries)]))
barplot(yvals, names.arg=nlist, horiz=TRUE, las=1)

YearlyInjuries <- aggregate(INJURIES ~ year, DT, sum)
YearlyInjuries <- YearlyInjuries[order(YearlyInjuries$INJURIES, decreasing=TRUE),]
row.names(YearlyInjuries) <- NULL
YearlyInjuries$year  <- as.Date(YearlyInjuries$year, format="%Y")
head(YearlyInjuries, 10)
plot(YearlyInjuries$year, YearlyInjuries$INJURIES)

#Sum injuries + fatalites for events
DT$TOTFI <- DT$FATALITIES + DT$INJURIES
EventTot <- aggregate(TOTFI ~ EVTYPE, DT, sum)
EventTot <- EventTot[order(EventTot$TOTFI, decreasing=TRUE),]
row.names(EventTot) <- NULL
head(EventTot, 10)

nlist <- as.character(EventTot$EVTYPE[1:20])
nlist <- c(nlist, "ALL OTHER EVENT TYPES")
yvals <- c(EventTot$TOTFI[1:20], sum(EventTot$TOTFI[21:nrow(EventTot)]))
barplot(yvals, names.arg=nlist, horiz=TRUE, las=1)
