PatternChangePoints = c(#"August 01 2020",
                        "November 08 2020",
                        "November 23 2020",
                        "December 7 2020",
                        "December 23 2020",
                        "January 17 2021",
                        "January 25 2021",
                        "February 16 2021",
                        "April 11 2021",                        
                        "June 30 2021",
                        "July 31 2021",
                        "October 06 2021",
                        "November 06 2021",
                        "December 28 2021",
                        "January 18 2022")
PatternChangeDates = as.Date(PatternChangePoints,"%B %d %Y")

VaccPoints = c("January 01 2021",
               "February 15 2021",
#               "April 12 2021",  
               "April 26 2021",                        
               "June 01 2021",
               "November 24 2021")
VaccDates = as.Date(VaccPoints,"%B %d %Y")

BoosterPoints = c("September 27 2021",
#                  "November 24 2021",
                  "January 05 2022")
BoosterDates = as.Date(BoosterPoints,"%B %d %Y")

VariantPoints = c("January 31 2021",
                  "April 04 2021",
                  "May 16 2021",
                  "June 20 2021",
                  "December 05 2021",
                  "December 19 2021")
VariantDates = as.Date(VariantPoints,"%B %d %Y")

plotDaily <- ggplot(WIdata[-c(1:192),], aes(x=as.Date(DATE,"%B %d %Y"), y=PositivesDaily))+
  geom_line(aes(y=rollmean(POS_NEW, 7, na.pad=TRUE), color="Cases"),size=2) +
  scale_colour_manual(name='', values=c(  "Cases"='green')) +
  scale_x_date(breaks = date_breaks("28 days"))+
  theme_minimal()+
  theme(plot.title = element_text(hjust=0.5, lineheight = .8, face = "bold"),
        axis.text.x = element_text(angle=90))+
  ylab("Daily New Cases")+
  xlab("Date") 
plotDaily +
  ggtitle(label = "Wisconsin Changepoints across 7-Day Average", 
          subtitle = "Vaccinations/Boosters (purple) and Variants (blue)")+
  geom_vline(xintercept = PatternChangeDates, lwd = 2, col= rgb(0,0,0,.3)) +
  geom_vline(xintercept = VaccDates, lty = 2, lwd = 1.5, col= "#440154FF") +
#  geom_vline(xintercept = VaccDates-21, lty = 2, lwd = 1.2, col= "skyblue")  +
  geom_vline(xintercept = BoosterDates, lty = 2,lwd = 1.5,  col= "#440154FF") +
  geom_vline(xintercept = VariantDates, lty = 2, lwd = 1.2, col= "#21908CFF")   #FDE725FF

