dfile <- "repdata_data_StormData.csv"
dfraw <- read.csv(dfile)

# Exploratory analysis
names(dfraw)
sort(table(dfraw$PROPDMGEXP), decreasing=TRUE)
sort(table(dfraw$CROPDMGEXP), decreasing=TRUE)

# initialize three new variables to sum up $$ damage
dfraw$CROPS <- as.numeric(NA)
dfraw$PROP <- as.numeric(NA)
dfraw$TOTDMG <- as.numeric(NA)


nr <- nrow(dfraw)
for (i in 1:50){
  e1 <- dfraw$PROPDMGEXP[i]
  e2 <- dfraw$CROPDMGEXP[i]
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
                  

  if(!is.null(dfraw$CRPDMG[i])){
    dfraw$CROPS[i] <- m2*dfraw$CRPDMG[i]
  } 
  if(!is.null(dfraw$PROPDMG[i])){
    dfraw$PROP[i] <- m1*dfraw$PROPDMG[i]
  } 
  dfraw$TOTDMG[i] <- sum(dfraw$CROPS[i], dfraw$PROP[i], na.rm=TRUE)
}
