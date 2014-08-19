require(reshape2)
require(data.table)

dfile <- "repdata_data_StormData.csv"

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

nr <- nrow(DT)
# for (i in 1:nr){
#   e1 <- DT$PROPDMGEXP[i]
#   e2 <- DT$CROPDMGEXP[i]
#   if(e1=="" | e1=="0"){
#     m1 <- 1e0
#   }else{
#    if(e1=="K" | e1=="3"){
#      m1 <- 1e3
#     }else{
#       if(e1=="m" | e1=="6" | e1=="M"){
#         m1 <- 1e6
#       }else{
#         if(e1=="B"){
#           m1 <- 1e9
#         }else{
#           m1 <- (e1=="1")*1e1 +
#             (e1=="h" | e1=="2" | e1=="H")*1e2 +
#             (e1=="4")*1e4 +
#             (e1=="5")*1e5 +
#             (e1=="7")*1e7 +
#             (e1=="8")*1e8
#         }
#       }
#     }
#   }
#   if(e2=="" | e2=="0"){
#     m2 <- 1e0
#   }else{
#     if(e2=="K" | e2=="3"){
#       m2 <- 1e3
#     }else{
#       if(e2=="m" | e2=="6" | e2=="M"){
#         m2 <- 1e6
#       }else{
#         if(e2=="B"){
#           m2 <- 1e9
#         }else{
#           m2 <- (e2=="1")*1e2 +
#             (e2=="h" | e2=="2" | e2=="H")*1e2 +
#             (e2=="4")*1e4 +
#             (e2=="5")*1e5 +
#             (e2=="7")*1e7 +
#             (e2=="8")*1e8
#         }
#       }
#     }
#   }
#   if(!is.null(DT$CRPDMG[i])){
#     DT$CROPS[i] <- m2*DT$CRPDMG[i]
#   } 
#   if(!is.null(DT$PROPDMG[i])){
#     DT$PROP[i] <- m1*DT$PROPDMG[i]
#   } 
#   DT$TOTDMG[i] <- sum(DT$CROPS[i], DT$PROP[i], na.rm=TRUE)
# }


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
barplot(yvals, names.arg=nlist, horiz=TRUE, las=1)

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

plot(YearlyDMG$year, YearlyDMG$TOTDMG)
