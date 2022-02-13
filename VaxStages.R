VaxChangePoints = c("August 01 2020",
                     "January 01 2021",
                     "February 15 2021",
                     "April 12 2021",
                     "April 26 2021",
                     "June 01 2021")
VaxStages = c("0", 
              "1A", 
              "1B", 
              "1C", 
              "2", 
              "3")

whichVaxDATES = daily_data$DATE %in% VaxChangePoints
daily_data$DATE[whichVaxDATES]

n_daily = dim(daily_data)[1]
whereVaxChangePoints = (1:n_daily)[whichVaxDATES]
daily_data$Stage = rep(NA, n_daily)
for (i in 1:length(whereVaxChangePoints)) {
  daily_data$Stage[(whereVaxChangePoints[i]):n_daily] = VaxStages[i]
}
daily_data$Stage  # NA are BEFORE we had regularly-available testing
