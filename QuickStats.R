usabledata = WIdata; usablepop = WIpop
usabledata$TOTAL_10Days[n]
usabledata$POS_NEW[n]

usablepop/usabledata$TOTAL_10Days[n]

usabledata$TOTAL_10Days[n-c(21,14,7,0)]
usabledata$POS_7DAY_AVG[n-c(21,14,7,3)]
usabledata$POS_7DAY_AVG[n-c(21,14,7,3)]/usabledata$POS_7DAY_AVG[n-c(28,21,14,7)]
usabledata$TOTAL_10Days[n-c(21,14,7,0)]/usabledata$TOTAL_10Days[n-c(28,21,14,7)]
usabledata$POS_7DAY_AVG[n-c(3)]/usabledata$POS_7DAY_AVG[n-c(21)]
usabledata$TOTAL_10Days[n-c(3)]/usabledata$TOTAL_10Days[n-c(21)]

usabledata$HOSP_YES[n-c(21,14,7,0)]-usabledata$HOSP_YES[n-c(28,21,14,7)]
usabledata$DEATHS[n-c(21,14,7,0)]-usabledata$DEATHS[n-c(28,21,14,7)]
