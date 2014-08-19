e1 <- dfraw$PROPDMGEXP[1]
e2 <- dfraw$CROPDMGEXP[1]
m1 <- (1 +
         (e1=="1")*1e1 +
         (e1=="h" | e1=="2" | e1=="H")*1e2 +
         (e1=="K" | e1=="3")*1e3 +
         (e1=="4")*1e4 +
         (e1=="5")*1e5 +
         (e1=="m" | e1=="6" | e1=="M")*1e6 +
         (e1=="7")*1e7 +
         (e1=="8")*1e8 +
         (e1=="B")*1e9
)
m2 <- (1 +
         (e2=="1")*1e1 +
         (e2=="h" | e2=="2" | e2=="H")*1e2 +
         (e2=="K" | e2=="3" | e2=="k")*1e3 +
         (e2=="4")*1e4 +
         (e2=="5")*1e5 +
         (e2=="m" | e2=="6" | e2=="M")*1e6 +
         (e2=="7")*1e7 +
         (e2=="8")*1e8 +
         (e2=="B")*1e9         
)
dfraw$CROPS[1] <- m2*dfraw$CRPDMG[1]
dfraw$PROP[1] <- m1*dfraw$PROPDMG[1]
dfraw$TOTDMG[1] <- dfraw$CROPS[1] + dfraw$PROP[1]
dfraw$CRPDMG[1]
