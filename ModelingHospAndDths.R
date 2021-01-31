dataused = WIdata
n = dim(dataused)[1]; 
dataused <- mutate(dataused,POS_80plus = rowSums(cbind(POS_80_89,  POS_90),na.rm=T))
dataused <- mutate(dataused,IP_Y_80plus = rowSums(cbind(IP_Y_80_89,  IP_Y_90),na.rm=T))
dataused <- mutate(dataused,DTHS_80plus = rowSums(cbind(DTHS_80_89,  DTHS_90),na.rm=T))

dataused <- mutate(dataused,POS_50_59_7DAY = rollmean(c(0,POS_50_59[-1] - POS_50_59[-n]), 7, na.pad=TRUE))
dataused <- mutate(dataused,HOSP_50_59_7DAY = rollmean(c(0,IP_Y_50_59[-1] - IP_Y_50_59[-n]), 7, na.pad=TRUE))
dataused <- mutate(dataused,DTHS_50_59_7DAY = rollmean(c(0,DTHS_50_59[-1] - DTHS_50_59[-n]), 7, na.pad=TRUE))

dataused <- mutate(dataused,POS_60_69_7DAY = rollmean(c(0,POS_60_69[-1] - POS_60_69[-n]), 7, na.pad=TRUE))
dataused <- mutate(dataused,HOSP_60_69_7DAY = rollmean(c(0,IP_Y_60_69[-1] - IP_Y_60_69[-n]), 7, na.pad=TRUE))
dataused <- mutate(dataused,DTHS_60_69_7DAY = rollmean(c(0,DTHS_60_69[-1] - DTHS_60_69[-n]), 7, na.pad=TRUE))

dataused <- mutate(dataused,POS_70_79_7DAY = rollmean(c(0,POS_70_79[-1] - POS_70_79[-n]), 7, na.pad=TRUE))
dataused <- mutate(dataused,HOSP_70_79_7DAY = rollmean(c(0,IP_Y_70_79[-1] - IP_Y_70_79[-n]), 7, na.pad=TRUE))
dataused <- mutate(dataused,DTHS_70_79_7DAY = rollmean(c(0,DTHS_70_79[-1] - DTHS_70_79[-n]), 7, na.pad=TRUE))

dataused <- mutate(dataused,POS_80plus_7DAY = rollmean(c(0,POS_80plus[-1] - POS_80plus[-n]), 7, na.pad=TRUE))
dataused <- mutate(dataused,HOSP_80plus_7DAY = rollmean(c(0,IP_Y_80plus[-1] - IP_Y_80plus[-n]), 7, na.pad=TRUE))
dataused <- mutate(dataused,DTHS_80plus_7DAY = rollmean(c(0,DTHS_80plus[-1] - DTHS_80plus[-n]), 7, na.pad=TRUE))

dataused <- mutate(dataused,POS_less50_7DAY = rollmean(c(0,POS_80plus[-1] - POS_80plus[-n]), 7, na.pad=TRUE))
dataused <- mutate(dataused,HOSP_80plus_7DAY = rollmean(c(0,IP_Y_80plus[-1] - IP_Y_80plus[-n]), 7, na.pad=TRUE))
dataused <- mutate(dataused,DTHS_80plus_7DAY = rollmean(c(0,DTHS_80plus[-1] - DTHS_80plus[-n]), 7, na.pad=TRUE))

allhosprates <- rep(0,9)
alldthrates <- rep(0,9)
#### plots - 80plus ####
plot(dataused$POS_80plus_7DAY,type="l")
hosplag = 4; dthlag=4
hosprate = 4.3; dthrate=8.5
points(hosplag:(n+hosplag-1),dataused$HOSP_80plus_7DAY*hosprate,type="l",col="blue")
#plot(dataused$POS_80plus_7DAY[1:(n-hosplag+1)],dataused$HOSP_80plus_7DAY[hosplag:n])
points(-dthlag:(n-dthlag+1),dataused$DTHS_80plus_7DAY*dthrate,type="l",col="red")
#plot(dataused$POS_80plus_7DAY[1:(n-hosplag+1)],dataused$DTHS_80plus_7DAY[hosplag:n])
#points(dataused$POS_80plus_7DAY[186:(n-hosplag+1)],dataused$DTHS_80plus_7DAY[(hosplag+185):n],col="blue",pch=20)
allhosprates[9] = 1/hosprate
alldthrates[9] = 1/dthrate

#### plots - 70-79 ####
plot(dataused$POS_70_79_7DAY,type="l")
hosplag = 7; dthlag=14
hosprate = 6.5; dthrate=26
points(dataused$HOSP_70_79_7DAY[hosplag:n]*hosprate,type="l",col="blue")
points(dataused$DTHS_70_79_7DAY[dthlag:n]*dthrate,type="l",col="red")
allhosprates[8] = 1/hosprate
alldthrates[8] = 1/dthrate
plot(dataused$POS_70_79_7DAY[1:(n-hosplag+1)],dataused$HOSP_70_79_7DAY[hosplag:n])
abline(0,1/hosprate)
plot(dataused$POS_70_79_7DAY[1:(n-dthlag+1)],dataused$DTHS_70_79_7DAY[dthlag:n])
points(dataused$POS_70_79_7DAY[186:(n-dthlag+1)],dataused$DTHS_70_79_7DAY[(dthlag+185):n],col="blue",pch=20)
abline(0,1/dthrate)

#### plots - 60-69 ####
plot(dataused$POS_60_69_7DAY,type="l")
hosplag = 4; dthlag=8
hosprate = 15; dthrate=110
points(dataused$HOSP_60_69_7DAY[hosplag:n]*hosprate,type="l",col="blue")
points(dataused$DTHS_60_69_7DAY[dthlag:n]*dthrate,type="l",col="red")
allhosprates[7] = 1/hosprate
alldthrates[7] = 1/dthrate

#### plots - 50-59 ####
plot(dataused$POS_50_59_7DAY,type="l")
hosplag = 2; dthlag=8
hosprate = 30; dthrate=400
points(dataused$HOSP_50_59_7DAY[hosplag:n]*hosprate,type="l",col="blue")
points(dataused$DTHS_50_59_7DAY[dthlag:n]*dthrate,type="l",col="red")
allhosprates[7] = 1/hosprate
alldthrates[7] = 1/dthrate

#### end plots ###




n = dim(ECdata)[1]; 

ECdata <- mutate(ECdata,POS_50plus = rowSums(cbind(POS_50_59, POS_60_69, POS_70_79, POS_80_89,  POS_90),na.rm=T))
ECdata <- mutate(ECdata,POS_50plus_DAILY = c(0,POS_50plus[-1] - POS_50plus[-n]))
ECdata <- mutate(ECdata,POS_50plus_7DAY = rollmean(POS_50plus_DAILY, 7, na.pad=TRUE))
#plot(ECdata$POS_7DAY_AVG,type="l")  
plot(ECdata$POS_50plus_7DAY,type="l")  
hosplag = 8; dthlag=12
hosprate = 10; dthrate=20
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


n = dim(WIdata)[1]; hosplag = 9; dthlag=9
WIdata <- mutate(WIdata,POS_50plus = rowSums(cbind(POS_50_59, POS_60_69, POS_70_79, POS_80_89,  POS_90),na.rm=T))
WIdata <- mutate(WIdata,POS_50plus_DAILY = c(0,POS_50plus[-1] - POS_50plus[-n]))
WIdata <- mutate(WIdata,POS_50plus_7DAY = rollmean(POS_50plus_DAILY, 7, na.pad=TRUE))
#plot(WIdata$POS_7DAY_AVG,type="l")  
plot(WIdata$POS_50plus_7DAY,type="l")
hosprate = 8; dthrate=47
points(WIdata$HOSP_7DAY_AVG[hosplag:n]*hosprate,type="l",col="blue")
points(WIdata$DTH_14DAY_AVG[dthlag:n]*dthrate,type="l",col="red")
WIdata$POS_50plus_7DAY[n-3]/hosprate
WIdata$POS_50plus_7DAY[n-7]/dthrate
plot(WIdata$POS_7DAY_AVG[1:(n-hosplag+1)],WIdata$HOSP_7DAY_AVG[hosplag:n])
abline(0,1/hosprate)
plot(WIdata$POS_50plus_7DAY[1:(n-hosplag+1)],WIdata$HOSP_7DAY_AVG[hosplag:n])
abline(0,1/hosprate)
plot(WIdata$POS_7DAY_AVG[1:(n-dthlag+1)],WIdata$DTH_7DAY_AVG[dthlag:n])
abline(0,1/dthrate)
