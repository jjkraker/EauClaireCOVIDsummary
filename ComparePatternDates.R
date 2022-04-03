PatternChangePoints = c("August 01 2020",
                        "November 02 2020",
                        "November 17 2020",
                        "December 1 2020",
                        "December 17 2020",
                        "January 11 2021",
                        "January 19 2021",
                        "February 10 2021",
                        "April 05 2021",                        
                        "June 25 2021",
                        "July 25 2021",
                        "September 30 2021",
                        "October 31 2021",
                        "December 29 2021",
                        "January 19 2022")
PatternChangeDates = as.Date(PatternChangePoints,"%B %d %Y")

VaccPoints = c("January 01 2021",
               "February 15 2021",
               "April 12 2021",  
               "April 26 2021",                        
               "June 01 2021",
               "November 24 2021")
VaccDates = as.Date(VaccPoints,"%B %d %Y")

BoosterPoints = c("September 27 2021",
                  "November 24 2021",
                  "January 05 2022")
BoosterDates = as.Date(BoosterPoints,"%B %d %Y")

VariantPoints = c("January 31 2021",
                  "April 04 2021",
                  "May 16 2021",
                  "June 20 2021",
                  "December 05 2021",
                  "December 19 2021")
VariantDates = as.Date(VariantPoints,"%B %d %Y")

plotDaily <- ggplot(WIdata, aes(x=as.Date(DATE,"%B %d %Y"), y=PositivesDaily))+
  geom_line(aes(y=rollmean(POS_NEW, 7, na.pad=TRUE), color="daily"),size=2) +
  scale_colour_manual(name='', values=c(  "daily"='green')) +
  scale_x_date(breaks = date_breaks("28 days"))+
  theme_minimal()+
  theme(plot.title = element_text(hjust=0.5, lineheight = .8, face = "bold"),
        axis.text.x = element_text(angle=90))+
  ylab("Daily New Cases")+
  xlab("Date") 
plotDaily +
  ggtitle(label = paste("Wisconsin Break points for", "Vaccinations and Boosters (blue)"))+
  geom_vline(xintercept = PatternChangeDates, lwd = 2, col= "goldenrod") +
  geom_vline(xintercept = VaccDates, lty = 2, lwd = 1.5, col= "purple") +
#  geom_vline(xintercept = VaccDates-21, lty = 2, lwd = 1.2, col= "skyblue")  +
  geom_vline(xintercept = BoosterDates, lty = 2,lwd = 1.2,  col= "blue")
plotDaily +
  ggtitle(label = paste("Wisconsin Break points for", "Variants"))+
  geom_vline(xintercept = PatternChangeDates, lwd = 2, col= "goldenrod") +
  geom_vline(xintercept = VariantDates, lty = 2, lwd = 1.2, col= "darkgreen") 

