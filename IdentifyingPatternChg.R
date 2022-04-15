############### estimating changepoints ######################3
library(viridis)

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
                        "April 05 2021",                        
                        "June 25 2021",
                        "July 25 2021",
                        "September 30 2021",
                        "October 31 2021",
                        "December 29 2021",
                        "January 19 2022")
#,
#                        "February 01 2022")
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
                  "P15",
                  "P16")

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


plot(WIdata$POS_7DAYAVG[firstday:lastday],WIdata$HOSP_7DAY_AVG[(firstday:lastday)+hosplag], 
     xlim=c(0,25000),
     xlab="Positives:  7-day Centered Average",
     ylab="Hospitalization:  7-day Centered Average")
points(WIdata$POS_7DAYAVG[1:lastday],WIdata$HOSP_7DAY_AVG[(1:lastday)+hosplag],col=colused,pch=20)
points(WIdata$POS_7DAYAVG[((lastday+1):(maxn-hosplag))],WIdata$HOSP_7DAY_AVG[((lastday+1+hosplag):maxn)],col="violet",pch=20)

points(WIdata$POS_7DAYAVG[wherePatternChangePoints],
       WIdata$HOSP_7DAY_AVG[wherePatternChangePoints+hosplag],pch=8,lwd=2)



##########################

oneChgLogic = WIdata$DATE %in% c("April 05 2021",
                                 "June 25 2021",
                                 "July 25 2021")
oneChgLoc = (1:maxn)[oneChgLogic]
points(WIdata$POS_7DAYAVG[oneChgLoc], WIdata$HOSP_7DAY_AVG[oneChgLoc+hosplag],pch=8,cex=2,lwd=2)


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







