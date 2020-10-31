
n = dim(ECdata)[1]; hosplag = 10; dthlag=12
plot(ECdata$POS_7DAY_AVG,type="l")  
points(ECdata$HOSP_7DAY_AVG[hosplag:n]*20,type="l",col="blue")
points(ECdata$DTH_7DAY_AVG[dthlag:n]*100,type="l",col="red")
ECdata$POS_7DAY_AVG[n-3]/20*7
ECdata$POS_7DAY_AVG[n-3]/100*7
plot(ECdata$POS_7DAY_AVG[1:(n-hosplag+1)],ECdata$HOSP_7DAY_AVG[hosplag:n])
abline(0,1/20)
# consider older groups avgs
plot(ECdata$HOSP_YES[1:(n-10)],ECdata$DEATHS[11:n])
abline(-1,1/3)

n = dim(CFdata)[1]; hosplag = 10; dthlag=8
plot(CFdata$POS_7DAY_AVG,type="l")  # consider older groups avgs
points(CFdata$HOSP_7DAY_AVG[hosplag:n]*22,type="l",col="blue")
points(CFdata$DTH_14DAY_AVG[dthlag:n]*50,type="l",col="red")
CFdata$POS_7DAY_AVG[n-3]/22*7
CFdata$POS_7DAY_AVG[n-3]/50*7
plot(CFdata$HOSP_YES[1:(n-10)],CFdata$DEATHS[11:n])
abline(-5,1/2)


n = dim(WIdata)[1]; hosplag = 10; dthlag=12
plot(WIdata$POS_7DAY_AVG,type="l")  
hosprate = 19; dthrate=115
points(WIdata$HOSP_7DAY_AVG[hosplag:n]*hosprate,type="l",col="blue")
points(WIdata$DTH_14DAY_AVG[dthlag:n]*dthrate,type="l",col="red")
WIdata$POS_7DAY_AVG[n-3]/hosprate
WIdata$POS_7DAY_AVG[n-3]/dthrate
plot(WIdata$POS_7DAY_AVG[1:(n-hosplag+1)],WIdata$HOSP_7DAY_AVG[hosplag:n])
abline(0,1/hosprate)
plot(WIdata$POS_7DAY_AVG[1:(n-dthlag+1)],WIdata$DTH_7DAY_AVG[dthlag:n])
abline(0,1/dthrate)
