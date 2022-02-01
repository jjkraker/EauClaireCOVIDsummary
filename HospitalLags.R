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



########################

# fix to go back to August 1
patternA = (-47:45)+240
patternB = (46:60)+240
patternC = (61:90)+240
patternD = (91:116)+240
patternE = (116:280)+240
patternF = (281:310)+240
patternG = (311:467)+240
  
hosplag=10

plot(WIdata$POS_7DAYAVG[241:707],WIdata$HOSP_7DAY_AVG[(241:707)+hosplag], xlim=c(0,25000))
# A
WIdata$DATE[patternA]
colused=rep(NA,707)
points(WIdata$POS_7DAYAVG[patternA],WIdata$HOSP_7DAY_AVG[(patternA)+hosplag],
       col="red",pch=20)

# B
WIdata$DATE[patternB]
colused=rep(NA,707)
points(WIdata$POS_7DAYAVG[patternB],WIdata$HOSP_7DAY_AVG[(patternB)+hosplag],
       col="orange",pch=20)

colused[patternA]="red" 
colused[patternB]="orange" 
colused[patternC]="yellow" 
colused[patternD]="green" 
colused[patternE]="turquoise" 
colused[patternF]="blue" 
colused[patternG] = "purple"


maxn = length(WIdata$DATE)
newpattern = (708:(maxn-hosplag))
WIdata$DATE[newpattern] 


plot(WIdata$POS_7DAYAVG[241:707],WIdata$HOSP_7DAY_AVG[(241:707)+hosplag], xlim=c(0,25000))
points(WIdata$POS_7DAYAVG[1:707],WIdata$HOSP_7DAY_AVG[(1:707)+hosplag],col=colused,pch=20)
points(WIdata$POS_7DAYAVG[(708:(maxn-hosplag))],WIdata$HOSP_7DAY_AVG[((708+hosplag):maxn)],col="violet",pch=20)



#plot(WIdata$POS_7DAYAVG[newpattern],WIdata$HOSP_7DAY_AVG[(newpattern)+hosplag])

plot(WIdata$POS_7DAYAVG[patternG],WIdata$HOSP_14DAY_AVG[(patternG)+hosplag],xlim=c(0,25000),ylim=c(0,200))
points(WIdata$POS_7DAYAVG[(611:650)],WIdata$HOSP_14DAY_AVG[(611:650)+hosplag],col="red",pch=20)
WIdata$DATE[(611:650)]  # ?? increased testing with schooling?
points(WIdata$POS_7DAYAVG[(681:695)],WIdata$HOSP_14DAY_AVG[(681:695)+hosplag],col="blue",pch=20)
WIdata$DATE[(681:695)]  # post thanksgiving weirdness
abline(60,.021)

points(WIdata$POS_7DAYAVG[(708:(maxn-hosplag))],WIdata$HOSP_14DAY_AVG[((708+hosplag):maxn)],col="purple",pch=20)


## hospitalization capacity
## look at deaths