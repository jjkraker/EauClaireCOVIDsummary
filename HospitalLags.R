############### estimating changepoints ######################3
daily_data = WIdata
# 193, 286, 301, 331, 356, 521, 551, 708
# WIdata$DATE[c(193, 286, 301, 331, 356, 521, 551, 708)] 
PatternChangePoints = c("August 01 2020",
                        "November 02 2020",
                        "November 17 2020",
                        "December 1 2020",
                        "December 17 2020",
                        "January 11 2021",
                        "January 19 2021",
                        "February 10 2021",
                        "June 25 2021",
                        "July 25 2021",
                        "August 05 2021",
                        "September 30 2021",
                        "October 31 2021",
                        "December 29 2021",
                        "January 20 2022",
                        "February 01 2022")
PatternStages = c("P0", 
                 "T1",
                 "P2",
                 "T3",
                 "P4",
                 "P5",
                 "P6",
                 "P7",
                 "P8",
                 "P9",
                 "P10",
                 "P11",
                 "P12",
                 "P13",
                 "P14",
                 "P15")

whichChangeDATES = WIdata$DATE %in% PatternChangePoints
daily_data$DATE[whichChangeDATES]

n_daily = dim(daily_data)[1]
wherePatternChangePoints = (1:n_daily)[whichChangeDATES]
daily_data$Stage = rep(NA, n_daily)
for (i in 1:length(wherePatternChangePoints)) {
  daily_data$Stage[(wherePatternChangePoints[i]):n_daily] = PatternStages[i]
}
daily_data$Stage  # NA are BEFORE we had regularly-available testing


hosplag=10  # 7-10 days

maxn = length(WIdata$DATE)
newpattern = (708:(maxn-hosplag))
WIdata$DATE[newpattern] 

firstday <- min(wherePatternChangePoints)
lastday <- maxn-hosplag # 707

colused = rep("white",maxn)
PatternColors = rainbow(length(PatternStages))
PatternColors[length(PatternStages)] = "blue1"
for (i in 1:length(PatternStages))  colused[daily_data$Stage == PatternStages[i]] <- PatternColors[i] 


plot(WIdata$POS_7DAYAVG[1:lastday],WIdata$HOSP_7DAY_AVG[(1:lastday)+hosplag], xlim=c(0,25000))
points(WIdata$POS_7DAYAVG[1:lastday],WIdata$HOSP_7DAY_AVG[(1:lastday)+hosplag],col=colused,pch=20)
points(WIdata$POS_7DAYAVG[((lastday+1):(maxn-hosplag))],WIdata$HOSP_7DAY_AVG[((lastday+1+hosplag):maxn)],col="violet",pch=20)

abline(0,.15)
abline(0,.075)
abline(0,.035)

abline(0,.015)
abline(0,.008)










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
