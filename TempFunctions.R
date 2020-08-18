plot(log(WIdata$POSITIVE),col="blue",type="l");
points(log(WIdata$DEATHS),col="red4",type="l");
points(1:length(WIdata$DEATHS) - 20,log(WIdata$DEATHS),col="red3",type="l")

points(21:length(WIdata$DEATHS) - 20,(log(WIdata$DEATHS[21:length(WIdata$DEATHS)])-2.28)/.4381,col="red",type="l")


plot(log(WIdata$DEATHS),col="red4",type="l");
points(log(WIdata$POSITIVE),col="blue4",type="l");
points(1:length(WIdata$POSITIVE) + 20,log(WIdata$POSITIVE),col="blue3",type="l")

points(21:length(WIdata$DEATHS) - 20,(log(WIdata$DEATHS[21:length(WIdata$DEATHS)])-2.28)/.4381,col="red",type="l")

#####################

plot(log(WIdata$POSITIVE),col="blue",type="l");
points(log(WIdata$DEATHS),col="red4",type="l");
points(1:length(WIdata$DEATHS) - 24,log(WIdata$DEATHS),col="red3",type="l")

points(1:length(WIdata$DEATHS) - 24,log(WIdata$DEATHS)/.46-4.5,col="red",type="l")


k=8; n=dim(WIdata)[1]
plot(WIdata$POSITIVE[1:(n-k)],WIdata$DEATHS[(k+1):n], 
     col="#EEEEEE", type="l", lwd=2)

colused = c("#AFEEEE","#40E0D0","#48D1CC","#00CED1")
for (i in 1:4) {
  k = (c(10, 12, 14, 16))[i]
  points(WIdata$POSITIVE[1:(n-k)],WIdata$DEATHS[(k+1):n], 
       col=colused[i], type="l", lwd=2)
}

k=8; n=dim(WIdata)[1]
plot(log(WIdata$POSITIVE[1:(n-k)]),log(WIdata$DEATHS[(k+1):n]), 
     col="#EEEEEE", type="l", lwd=2)

colused = c("#AFEEEE","#40E0D0","#48D1CC","#00CED1")
for (i in 1:4) {
  k = (c(16, 17, 18, 19))[i]
  points(log(WIdata$POSITIVE[1:(n-k)]),log(WIdata$DEATHS[(k+1):n]), 
         col=colused[i], type="l", lwd=2)
}

k=19; n=dim(WIdata)[1]
plot(log(WIdata$POSITIVE[11:(n-k)]),log(WIdata$DEATHS[(k+11):n]), 
     col="#00CED1", type="l", lwd=2)

coef = lm(log(WIdata$DEATHS[(k+11):n])~log(WIdata$POSITIVE[11:(n-k)]))$coef
abline(coef); coef
#log(y) = 2.2+.45*log(x)
#DTHS ~ exp(coef[1])*(CASES at lag 19)^coef[2]
# Area plot
ggplot(WIdata, aes(x=DATE, y=value, fill=group)) + 
  geom_area()

# density plot
data %>%
  filter( price<300 ) %>%
  ggplot( aes(x=price)) +
  geom_density(fill="#69b3a2", color="#e9ecef", alpha=0.8)


# Libraries
library(ggplot2)
library(dplyr)
library(babynames)
library(viridis)
library(hrbrthemes)
library(plotly)

# Load dataset from github
data <- babynames %>% 
  filter(name %in% c("Ashley", "Amanda", "Jessica",    "Patricia", "Linda", "Deborah",   "Dorothy", "Betty", "Helen")) %>%
  filter(sex=="F")

# Plot
p <- data %>% 
  ggplot( aes(x=year, y=n, fill=name, text=name)) +
  geom_area( ) +
  scale_fill_viridis(discrete = TRUE) +
  theme(legend.position="none") +
  ggtitle("Popularity of American names in the previous 30 years") +
  theme_ipsum() +
  theme(legend.position="none")

# Turn it interactive
p <- ggplotly(p, tooltip="text")
p

# save the widget
# library(htmlwidgets)
# saveWidget(p, file=paste0( getwd(), "/HtmlWidget/ggplotlyStackedareachart.html"))






















p1 <- ggplot(usabledata %>% mutate(Value = ifelse(Variable == "New.deaths", Value*secAxisConstant, Value)) %>% filter(Variable %in% c("New.deaths","New.cases")))+
  aes(Date, Value, fill = Variable, label = Value)+
  geom_col(position = "dodge", alpha = .5, width = .85)+
  geom_vline(xintercept = "2020-07-04" %>% as.Date(), lty = 2, alpha = .4)+
  geom_vline(xintercept = dataLongDailyTests %>%
               distinct(Date) %>% 
               filter(Date %in% vlineDf$Date) %>% 
               pull(Date), lty = 2, alpha = .4)+
  # n day moving average
  geom_path(data = dataLongAvg %>% mutate(movAvgValue = ifelse(Variable == "New.deaths", movAvgValue*secAxisConstant, movAvgValue)) %>% filter(Variable %in% c("New.deaths","New.cases")) , aes(Date, movAvgValue, color = Variable, group = Variable), size = 1.6, alpha = .6)+
  geom_path(data = dataLongAvg %>% mutate(movAvgValue2 = ifelse(Variable == "New.deaths", movAvgValue2*secAxisConstant, movAvgValue2)) %>% filter(Variable %in% c("New.deaths","New.cases")) , aes(Date, movAvgValue2, group = Variable, lty = "DailyTestsWeighted"), size = .5, alpha = .8, color = "blue")+
  #geom_path(data = dataLongAvg %>% mutate(movAvgValue = ifelse(Variable == "New.deaths", movAvgValue*secAxisConstant, movAvgValue)) %>% filter(Variable %in% c("New.deaths","New.cases")) , aes(Date, movAvgValue3, color = Variable, group = Variable), size = 3, alpha = .9)+
  #geom_text(data=dataLongDailyTests %>% filter(Variable %in% c("New.cases")) %>% filter(Value > 0), aes(x= Date, y = Value), nudge_y = 4, size = 2.5)+
  #geom_text(data=dataLongDailyTests %>% filter(Variable %in% c("New.deaths")) %>% filter(Value > 0), aes(x= Date, y = Value), nudge_y = 2, size = 2.5)+
  geom_text_repel(data=dataLongDailyTests %>% filter(Variable %in% c("New.cases")) %>% filter(Date == last(Date)), aes(x= Date, y = Value), segment.color = NA, direction = "y", box.padding = .05, nudge_x = 2, nudge_y = 1, size = 4, color = RColorBrewer::brewer.pal(3, "Dark2")[1], fontface = "bold")+
  geom_text_repel(data=dataLongDailyTests %>% filter(Variable %in% c("New.deaths")) %>% filter(Date == last(Date)), aes(x= Date, y = Value*secAxisConstant), segment.color = NA, direction = "y", box.padding = .05, nudge_x = 2.5, nudge_y = 1, size = 4, ylim = c(0, Inf),color = RColorBrewer::brewer.pal(3, "Dark2")[2],fontface = "bold")+
  annotate("label", x = vlineDf$Date, y = c(200, 300, 400, 850, 850, 1030, 950, 850, 750, 850), label = vlineDf$Label, lineheight = .75, size = 3, label.padding = unit(0.1, "lines"), label.size = .02)+
  annotate("rect", xmin = as.Date(today(),format='%d-%B-%Y')-7, xmax = today(), ymin = 0, ymax = Inf, alpha = .15)+
  labs(x = "", y = "New cases", title = "Daily new cases and deaths", fill = "")+
  guides(fill = guide_legend(order = 1))+
  scale_fill_brewer(name = "", palette = "Set2", labels = c("New case", "New death"))+
  scale_color_manual(name = moveAvg %>% as.character() %>% paste0("-day moving average"), values = RColorBrewer::brewer.pal(3, "Dark2"), labels = c("New case", "New death"))+
  scale_linetype_manual(name = moveAvg %>% as.character() %>% paste0("-day moving average weighted by daily tests"), values = c(1,1), labels = "")+
  scale_x_date(date_breaks = "7 days", date_labels = "%b %d")+
  scale_y_continuous(sec.axis = sec_axis(~ ./secAxisConstant, 		
                                         name = "New deaths"))+
  theme_minimal()+
  theme(panel.grid.major.x = element_blank(), legend.margin=margin(),legend.box="vertical",legend.position = "bottom", axis.text.x = element_text(size=10, angle = 50, hjust = 1), text=element_text(size=14), legend.text = element_text(size=12),
        axis.title.y.left = element_text(color = RColorBrewer::brewer.pal(3, "Dark2")[1]), axis.title.y.right = element_text(color = RColorBrewer::brewer.pal(3, "Dark2")[2]),
        axis.text.y.left = element_text(color = RColorBrewer::brewer.pal(3, "Dark2")[1]),axis.text.y.right = element_text(color = RColorBrewer::brewer.pal(3, "Dark2")[2]))

p1
}

############
checkdata=BAdata
n = dim(checkdata)[1]
diff = checkdata$POSITIVE[2:n]-checkdata$POSITIVE[1:(n-1)]
plot(diff,checkdata$POS_NEW[2:n])
whichmiss <- checkdata$DATE[diff != checkdata$POS_NEW[2:n]]
diff[whichmiss]; checkdata$POS_NEW[whichmiss]
diffn = checkdata$NEGATIVE[2:n]-checkdata$NEGATIVE[1:(n-1)]
plot(diffn,checkdata$NEG_NEW[2:n])
whichmissn <- checkdata$DATE[diffn != checkdata$NEG_NEW[2:n]]
diffn[whichmissn]; checkdata$NEG_NEW[whichmissn]
