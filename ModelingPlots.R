
n = dim(CFdata)[1]
lagHosp = 12; lagDths = 25
plot(CFdata$POSITIVE,type="l")
points(1:(n-lagHosp),CFdata$HOSP_YES[(lagHosp+1):n]*16,col="red",type="l")
points(1:(n-lagDths),CFdata$DEATHS[(lagDths+1):n]*100,col="navy",type="l")
lagHosp = 21; lagDths = 28
plot(CFdata$POSITIVE[1:(n-lagHosp)],CFdata$HOSP_YES[(lagHosp+1):n])
abline(-5,1/15)
plot(CFdata$POSITIVE[1:(n-lagDths)],CFdata$DEATHS[(lagDths+1):n])
abline(1.5,1/90)


n = dim(ECdata)[1]
lagHosp = 21; lagDths = 28
plot(ECdata$POSITIVE[1:(n-lagHosp)],ECdata$HOSP_YES[(lagHosp+1):n])
abline(10,1/22)
plot(ECdata$POSITIVE[1:(n-lagDths)],ECdata$DEATHS[(lagDths+1):n])
abline(1.5,1/180)
plot(ECdata$POSITIVE,type="l")
points(1:(n-lagHosp),ECdata$HOSP_YES[(lagHosp+1):n]*20,col="red",type="l")
points(1:(n-lagDths),ECdata$DEATHS[(lagDths+1):n]*140,col="navy",type="l")
logPOSlagH <- log(ECdata$POSITIVE[1:(n-lagHosp)])
logHOSPlagH <- log(ECdata$HOSP_YES[(lagHosp+1):n])
logPOSlagD <- log(ECdata$POSITIVE[1:(n-lagDths)])
logDTHSlagD <- log(ECdata$DEATHS[(lagDths+1):n])
logDTHSlagD[logDTHSlagD< -100] = NA
plot(logPOSlagH,logHOSPlagH, type="l",col="red")
points(logPOSlagD,logDTHSlagD, type="l",col="navy")
HospFit <- lm(logHOSPlagH~logPOSlagH)
DthsFit <- lm(logDTHSlagD~logPOSlagD)
coefH <- coef(HospFit); coefH[1] = exp(coefH[1])
coefH[1]*ECdata$POSITIVE^coefH[2]
abline(HospFit,col="red4",lty=2)
abline(DthsFit,col="navy",lty=2)
abline(-3,0.71,col="navy",lty=2)
coefD = c(exp(-3),0.71)
coefD[1]*ECdata$POSITIVE^coefD[2]


# plot 7-day running averages of daily hosp vs daily cases?
# to find the lag


rollmean(ECdata$POS_NEW, 7, na.pad=TRUE)

lagHosp = 18
n = dim(WIdata)[1]
daily7POSlagH <- rollmean(WIdata$POS_NEW[1:(n-lagHosp)], 14, na.pad=TRUE)
daily7HOSPlagH <- rollmean(c(0,WIdata$HOSP_YES[2:n]-WIdata$HOSP_YES[1:(n-1)])[(lagHosp+1):n], 14, na.pad=TRUE)
plot(daily7POSlagH,daily7HOSPlagH)
abline(0,.034)

lagDths = 24
daily7POSlagD <- rollmean(WIdata$POS_NEW[1:(n-lagDths)], 14, na.pad=TRUE)
daily7DTHlagD <- rollmean(WIdata$DTH_NEW[(lagDths+1):n], 14, na.pad=TRUE)
plot(daily7POSlagD,daily7DTHlagD)
abline(1,.01)
