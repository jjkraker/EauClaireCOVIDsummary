
n = dim(ECdata)[1]; hosplag = 8; dthlag=18
plot(ECdata$POS_7DAY_AVG,type="l")  
points(ECdata$HOSP_7DAY_AVG[hosplag:n]*18,type="l",col="blue")
points(ECdata$DTH_14DAY_AVG[dthlag:n]*100,type="l",col="red")
ECdata$POS_7DAY_AVG[n-3]/22
ECdata$POS_7DAY_AVG[n-3]/100
plot(ECdata$POS_7DAY_AVG[1:(n-hosplag+1)],ECdata$HOSP_7DAY_AVG[hosplag:n])
# consider older groups avgs

n = dim(CFdata)[1]; hosplag = 10; dthlag=12
plot(CFdata$POS_7DAY_AVG,type="l")  # consider older groups avgs
points(CFdata$HOSP_7DAY_AVG[hosplag:n]*22,type="l",col="blue")
points(CFdata$DTH_14DAY_AVG[dthlag:n]*60,type="l",col="red")
CFdata$POS_7DAY_AVG[n-3]/22
CFdata$POS_7DAY_AVG[n-3]/60


n = dim(WIdata)[1]; hosplag = 10; dthlag=14
plot(WIdata$POS_7DAY_AVG,type="l")  
points(WIdata$HOSP_7DAY_AVG[hosplag:n]*20,type="l",col="blue")
points(WIdata$DTH_14DAY_AVG[dthlag:n]*120,type="l",col="red")
WIdata$POS_7DAY_AVG[n-3]/20
WIdata$POS_7DAY_AVG[n-3]/120
plot(WIdata$POS_7DAY_AVG[1:(n-hosplag+1)],WIdata$HOSP_7DAY_AVG[hosplag:n])
