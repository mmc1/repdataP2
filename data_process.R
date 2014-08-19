library(data.table)

dfile <- "repdata_data_StormData.csv"

DT <- read.table(dfile, header=TRUE, sep=",")

# Exploratory analysis
names(DT)
sort(table(DT$PROPDMGEXP), decreasing=TRUE)
sort(table(DT$CROPDMGEXP), decreasing=TRUE)

# initialize three new variables to sum up $$ damage
DT$CROPS <- as.numeric(NA)
DT$PROP <- as.numeric(NA)
DT$TOTDMG <- as.numeric(NA)


nr <- nrow(DT)
for (i in 1:50){
  e1 <- DT$PROPDMGEXP[i]
  e2 <- DT$CROPDMGEXP[i]
  if(e1=="" | e1=="0"){
    m1 <- 1e0
  }else{
    if(e1=="K" | e1=="3"){
      m1 <- 1e3
    }else{
      if(e1=="m" | e1=="6" | e1=="M"){
        m1 <- 1e6
      }else{
        if(e1=="B"){
          m1 <- 1e9
        }else{
          m1 <- (e1=="1")*1e1 +
            (e1=="h" | e1=="2" | e1=="H")*1e2 +
            (e1=="4")*1e4 +
            (e1=="5")*1e5 +
            (e1=="7")*1e7 +
            (e1=="8")*1e8
        }
      }
    }
  }
  if(e2=="" | e2=="0"){
    m2 <- 1e0
  }else{
    if(e2=="K" | e2=="3"){
      m2 <- 1e3
    }else{
      if(e2=="m" | e2=="6" | e2=="M"){
        m2 <- 1e6
      }else{
        if(e2=="B"){
          m2 <- 1e9
        }else{
          m2 <- (e2=="1")*1e2 +
            (e2=="h" | e2=="2" | e2=="H")*1e2 +
            (e2=="4")*1e4 +
            (e2=="5")*1e5 +
            (e2=="7")*1e7 +
            (e2=="8")*1e8
        }
      }
    }
  }
                  

  if(!is.null(DT$CRPDMG[i])){
    DT$CROPS[i] <- m2*DT$CRPDMG[i]
  } 
  if(!is.null(DT$PROPDMG[i])){
    DT$PROP[i] <- m1*DT$PROPDMG[i]
  } 
  DT$TOTDMG[i] <- sum(DT$CROPS[i], DT$PROP[i], na.rm=TRUE)
}
