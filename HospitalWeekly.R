names(WIdata)

WIbyAges <- WIdata %>% select(DATE,POS_0_9:POS_90,IP_Y_0_9_CP:IP_Y_90_CP)

WIbyAges <- WIbyAges %>% mutate(Hosp_7day_0_9 = rollmean(IP_Y_90_CP - dplyr::lag(IP_Y_90_CP), 7, na.pad=TRUE))
# first row is a Wed (Jan. 22, 2020)
n = dim(WIbyAges)[1]

whichrows <- 0:(floor(n/7))*7+1

WIbyAgesWeekly <- WIbyAges[whichrows,]
plot(WIbyAgesWeekly$Hosp_7day_0_9-lag(WIbyAgesWeekly$Hosp_7day_0_9),type="l")
