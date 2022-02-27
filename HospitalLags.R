############### estimating changepoints ######################3

# 193, 286, 301, 331, 356, 521, 551, 708
WIdata$DATE[c(193, 286, 301, 331, 356, 521, 551, 708)] 
PatternChangePoints = c("August 01 2020",
                        "November 02 2020",
                        "November 17 2020",
                        "December 17 2020",
                        "January 11 2021",
                        "June 25 2021",
                        "July 25 2021",
                        "December 29 2021")
PatternStages = c("P0", 
                 "P1",
                 "P2",
                 "P3",
                 "P4")

whichChangeDATES = WIdata$DATE %in% PatternChangePoints
daily_data$DATE[whichVaxDATES]

n_daily = dim(daily_data)[1]
whereVaxChangePoints = (1:n_daily)[whichVaxDATES]
daily_data$Stage = rep(NA, n_daily)
for (i in 1:length(whereVaxChangePoints)) {
  daily_data$Stage[(whereVaxChangePoints[i]):n_daily] = VaxStages[i]
}
daily_data$Stage  # NA are BEFORE we had regularly-available testing

# fix to go back to August 1
patternA = (-47:45)+240
patternB = (46:60)+240
patternC = (61:90)+240
patternD = (91:116)+240
patternE = (116:280)+240
patternF = (281:310)+240
patternG = (311:467)+240
firstday <- min(patternA)
lastday <- max(patternG)
  
hosplag=7


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


plot(WIdata$POS_7DAYAVG[firstday:lastday],WIdata$HOSP_7DAY_AVG[(firstday:lastday)+hosplag], xlim=c(0,25000))
points(WIdata$POS_7DAYAVG[1:lastday],WIdata$HOSP_7DAY_AVG[(1:lastday)+hosplag],col=colused,pch=20)
points(WIdata$POS_7DAYAVG[((lastday+1):(maxn-hosplag))],WIdata$HOSP_7DAY_AVG[((lastday+1+hosplag):maxn)],col="violet",pch=20)


## hospitalization capacity
## look at deaths

#  weird October and November patterns


#plot(WIdata$POS_7DAYAVG[newpattern],WIdata$HOSP_7DAY_AVG[(newpattern)+hosplag])

plot(WIdata$POS_7DAYAVG[patternG],WIdata$HOSP_14DAY_AVG[(patternG)+hosplag],xlim=c(0,25000),ylim=c(0,200))
points(WIdata$POS_7DAYAVG[(611:650)],WIdata$HOSP_14DAY_AVG[(611:650)+hosplag],col="red",pch=20)
WIdata$DATE[(611:650)]  # ?? increased testing with schooling?
points(WIdata$POS_7DAYAVG[(681:695)],WIdata$HOSP_14DAY_AVG[(681:695)+hosplag],col="blue",pch=20)
WIdata$DATE[(681:695)]  # post thanksgiving weirdness
abline(60,.021)

points(WIdata$POS_7DAYAVG[(708:(maxn-hosplag))],WIdata$HOSP_14DAY_AVG[((708+hosplag):maxn)],col="purple",pch=20)









############### estimating lag ######################3

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



#
