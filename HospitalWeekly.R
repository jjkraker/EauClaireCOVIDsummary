names(WIdata)

WIbyAges <- WIdata %>% select(DATE,POS_0_9:POS_90,IP_Y_0_9_CP:IP_Y_90_CP)

WIbyAges <- WIbyAges %>% mutate(
  Pos_7day_0_9 = rollmean(POS_0_9 - dplyr::lag(POS_0_9), 7, na.pad=TRUE),
  Hosp_7day_0_9 = rollmean(IP_Y_0_9_CP - dplyr::lag(IP_Y_0_9_CP), 7, na.pad=TRUE),
  Pos_7day_70_79 = rollmean(POS_70_79 - dplyr::lag(POS_70_79), 7, na.pad=TRUE),
  Hosp_7day_70_79 = rollmean(IP_Y_70_79_CP - dplyr::lag(IP_Y_70_79_CP), 7, na.pad=TRUE),
)
# first row is a Wed (Jan. 22, 2020)
n = dim(WIbyAges)[1]
whichrows <- 0:(floor(n/7))*7+1

WIbyAgesWeekly <- WIbyAges[whichrows,]
nweeks = dim(WIbyAgesWeekly)[1]

Pos_7day_agegroup = WIbyAgesWeekly$Pos_7day_70_79
Hosp_7day_agegroup = WIbyAgesWeekly$Hosp_7day_70_79
plot(Pos_7day_agegroup,type="l")
points(Hosp_7day_agegroup,type="l",col="red")

plot(Pos_7day_agegroup,Hosp_7day_agegroup)
plot(Pos_7day_agegroup[1:(nweeks-2)],Hosp_7day_agegroup[3:nweeks])
abline(0,.18)

plot(WIbyAges$Pos_7day_70_79,type="l")
points(WIbyAges$Hosp_7day_70_79,type="l",col="red")

plot(WIbyAges$Pos_7day_70_79,type="l",col="blue4")
points(WIbyAges$Pos_7day_0_9,type="l",col="red")


# reformat to make nice axes etc, but keep in base-R for purposes of communication / use by Anna