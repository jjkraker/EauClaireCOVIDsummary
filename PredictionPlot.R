## To plot dth and hosp prediction graph

PlotMatching <- function(posCases,deathCases, hospCases, deathLag, deathRate, hospLag, hospRate){
 
  plot(posCases,type="l")
  points(hospLag:(n+hospLag-1), hospCases*hospRate,type="l",col="blue") #hosp
  points(deathLag:(n+deathLag-1),deathCases*deathRate,type="l",col="red") #dth
  
}

PlotMatching(dataused$POS_80plus_7DAY,dataused$DTHS_80plus_7DAY,dataused$HOSP_80plus_7DAY,4,8.5,4,4.3) 
