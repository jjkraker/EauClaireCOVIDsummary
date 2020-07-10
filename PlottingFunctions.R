FullTotalTable = function(usabledata) {
  fulltable <-
    usabledata %>% 
    filter(DATE %in% tail(DATE, 7)) %>%
    select(DATE, POSITIVE, NEGATIVE, DEATHS, HOSP_YES, HOSP_UNK, IC_YES) %>%
    rename_at(vars(starts_with("HOSP_")), ~str_to_title(str_replace_all(., "HOSP_","Hospitalization "))) %>%
    rename_at(vars(starts_with("IC_")), ~str_to_title(str_replace_all(., "IC_","ICU "))) %>%   
    rename_at(vars(ends_with("Unk")), ~str_to_title(str_replace_all(., "Unk","Unknown "))) %>%   
    kable(digits = 3,booktabs = T, caption = "Wisconsin cumulative counts of COVID cases and outcomes",align = "c") %>%
    kable_styling(latex_options = c("hold_position", "scale_down"), font_size = 11) %>% 
    row_spec(7, color = "darkblue", background = "#00FFFF") 
  
  fulltable
}







# like that from https://i.redd.it/o0lnkamvpn951.png

DailyInfoPlot = function(usabledata) {
  ## plot daily new cases and deaths
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