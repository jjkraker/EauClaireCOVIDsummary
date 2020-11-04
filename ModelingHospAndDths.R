n = dim(ECdata)[1]; 

ECdata <- mutate(ECdata,POS_50plus = rowSums(cbind(POS_50_59, POS_60_69, POS_70_79, POS_80_89,  POS_90),na.rm=T))
ECdata <- mutate(ECdata,POS_50plus_DAILY = c(0,POS_50plus[-1] - POS_50plus[-n]))
ECdata <- mutate(ECdata,POS_50plus_7DAY = rollmean(POS_50plus_DAILY, 7, na.pad=TRUE))
#plot(ECdata$POS_7DAY_AVG,type="l")  
plot(ECdata$POS_50plus_7DAY,type="l")  
hosplag = 8; dthlag=12
hosprate = 5.5; dthrate=20
points(ECdata$HOSP_7DAY_AVG[hosplag:n]*hosprate,type="l",col="blue")
points(ECdata$DTH_7DAY_AVG[dthlag:n]*dthrate,type="l",col="red")
ECdata$POS_50plus_7DAY[n-3]/hosprate*7
ECdata$POS_50plus_7DAY[n-3]/dthrate*7
plot(ECdata$POS_50plus_7DAY[1:(n-hosplag+1)],ECdata$HOSP_7DAY_AVG[hosplag:n])
abline(0,1/hosprate)
# consider older groups avgs
plot(ECdata$POS_50plus_7DAY[1:(n-dthlag+1)],ECdata$DTH_7DAY_AVG[dthlag:n])
abline(0,1/dthrate)

plot(ECdata$HOSP_YES[1:(n-4)],ECdata$DEATHS[5:n])
abline(-1,1/7)

n = dim(CFdata)[1]; hosplag = 10; dthlag=8
plot(CFdata$POS_7DAY_AVG,type="l")  # consider older groups avgs
points(CFdata$HOSP_7DAY_AVG[hosplag:n]*22,type="l",col="blue")
points(CFdata$DTH_14DAY_AVG[dthlag:n]*50,type="l",col="red")
CFdata$POS_7DAY_AVG[n-3]/22*7
CFdata$POS_7DAY_AVG[n-3]/50*7
plot(CFdata$HOSP_YES[1:(n-10)],CFdata$DEATHS[11:n])
abline(-5,1/2)


n = dim(WIdata)[1]; hosplag = 8; dthlag=12
plot(WIdata$POS_7DAY_AVG,type="l")  
hosprate = 20; dthrate=110
points(WIdata$HOSP_7DAY_AVG[hosplag:n]*hosprate,type="l",col="blue")
points(WIdata$DTH_14DAY_AVG[dthlag:n]*dthrate,type="l",col="red")
WIdata$POS_7DAY_AVG[n-3]/hosprate
WIdata$POS_7DAY_AVG[n-3]/dthrate
plot(WIdata$POS_7DAY_AVG[1:(n-hosplag+1)],WIdata$HOSP_7DAY_AVG[hosplag:n])
abline(0,1/hosprate)
plot(WIdata$POS_7DAY_AVG[1:(n-dthlag+1)],WIdata$DTH_7DAY_AVG[dthlag:n])
abline(0,1/dthrate)
