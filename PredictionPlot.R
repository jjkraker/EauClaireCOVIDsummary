## To plot dth and hosp prediction graph

PlotMatching <- function(posCases,deathCases, hospCases, deathLag, deathRate, hospLag, hospRate){
 
  plot(posCases,type="l")
  points(hospLag:(n+hospLag-1), hospCases*hospRate,type="l",col="blue") #hosp
  points(deathLag:(n+deathLag-1),deathCases*deathRate,type="l",col="red") #dth
  
}

PlotMatching(dataused$POS_80plus_7DAY,dataused$DTHS_80plus_7DAY,dataused$HOSP_80plus_7DAY,4,8.5,4,4.3) 

h=8
WIdataLagged = WIdataTimeSeries[161:(dim(WIdataTimeSeries)[1]-h-3),]
WIdataLagged$HOSP_7DAY_AVGahead = WIdataTimeSeries$HOSP_7DAY_AVG[(161+h):(dim(WIdataTimeSeries)[1]-3)]
lmfitlag = lm(HOSP_7DAY_AVGahead ~ . - DTH_7DAY_AVG - HOSP_7DAY_AVG, data=WIdataLagged)
plot(lmfitlag$fitted, WIdataLagged$HOSP_7DAY_AVGahead)
cor(lmfitlag$fitted, WIdataLagged$HOSP_7DAY_AVGahead)
plot(lmfitlag$fitted)
points(WIdataLagged$HOSP_7DAY_AVGahead,col="red",type="l")

WIdataLagged = mutate(WIdataLagged,POS_30more_7DAY = (POS_30_39_7DAY+POS_40_49_7DAY+POS_50_59_7DAY+POS_60_69_7DAY+POS_70_79_7DAY+POS_80plus_7DAY))
WIdataLagged2 = select(WIdataLagged,HOSP_7DAY_AVGahead,POS_less30_7DAY,POS_30more_7DAY)
names(WIdataLagged2)
lmfitlag = lm(HOSP_7DAY_AVGahead ~ ., data=WIdataLagged2)
plot(lmfitlag$fitted, WIdataLagged$HOSP_7DAY_AVGahead)
cor(lmfitlag$fitted, WIdataLagged$HOSP_7DAY_AVGahead)
plot(lmfitlag$fitted)
points(c(9:(length(WIdataLagged$HOSP_7DAY_AVGahead)+8)),WIdataLagged$HOSP_7DAY_AVGahead,col="red",type="l")
