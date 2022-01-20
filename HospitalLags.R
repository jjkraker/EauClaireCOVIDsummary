POS_daily <- WIdata2021$POS_20_29 - lag(WIdata2021$POS_20_29)
HOSP_daily <- WIdata2021$IP_Y_20_29_CP - lag(WIdata2021$IP_Y_20_29_CP)
DTH_daily <- WIdata2021$DTHS_20_29_CP - lag(WIdata2021$DTHS_20_29_CP)

POS_daily_14dayAVG = rollmean(POS_daily, 14, na.pad=TRUE)
POS_daily_14dayAVG
HOSP_daily_14dayAVG = rollmean(HOSP_daily, 14, na.pad=TRUE)
HOSP_daily_14dayAVG
DTH_daily_14dayAVG = rollmean(DTH_daily, 14, na.pad=TRUE)
DTH_daily_14dayAVG

laghosp = 5; multhosp =60
lagdeath = 21; multdeath =1000
plot(POS_daily_14dayAVG,type="l",lwd=2)
points((1:length(HOSP_daily_14dayAVG))-laghosp,HOSP_daily_14dayAVG*multhosp,col="blue",type="l")
points((1:length(DTH_daily_14dayAVG))-lagdeath,DTH_daily_14dayAVG*multdeath,col="red",type="l")

#twenties
laghosp = 5; multhosp =60
lagdeath = 21; multdeath =1000

#forties
laghosp = 4; multhosp =35
lagdeath = 15; multdeath =250

#sixties
laghosp = 4; multhosp =12
lagdeath = 15; multdeath =60

