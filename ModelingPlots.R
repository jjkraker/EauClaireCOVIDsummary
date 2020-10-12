


plot(ECdata$POSITIVE,type="l")
points(1:(209-20),ECdata$HOSP_YES[21:209]*18,col="red",type="l")
points(1:(209-25),ECdata$DEATHS[26:209]*140,col="navy",type="l")
lagHosp = 20; lagDths = 25
logPOSlagH <- log(ECdata$POSITIVE[1:(209-lagHosp)])
logHOSPlagH <- log(ECdata$HOSP_YES[(lagHosp+1):209])
logPOSlagD <- log(ECdata$POSITIVE[1:(209-lagDths)])
logDTHSlagD <- log(ECdata$DEATHS[(lagDths+1):209])
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

