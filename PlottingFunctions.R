CaseCheck = function(usabledata) {
  usabledata <- usabledata %>% 
  mutate(DATE = format(as_date(DATE), format = "%B %d %Y")) %>%   
  mutate(DTH_NEW = as.numeric(DTH_NEW))  %>%
  mutate(DTH_NEW = case_when(is.na(DTH_NEW) ~ 0,
                             DTH_NEW <0 ~ 0,   
                             c(DTH_NEW[-1],0) < 0 ~ +1,
                             TRUE ~ DTH_NEW
                             )
  ) %>%
  mutate(POS_NEW = as.numeric(POS_NEW))  %>%
  mutate(POS_NEW = case_when(is.na(POS_NEW) ~ 0,
                             POS_NEW <0 ~ 0,   
                             c(POS_NEW[-1],0) < 0 ~ +1,
                             TRUE ~ POS_NEW
                             )
  ) %>%
  mutate(TOTAL_10Days= rollsum(POS_NEW, 10, fill=NA,align="right")
  ) %>%
  mutate(TOTAL_15Days = rollsum(POS_NEW, 15, fill=NA,align="right")
  ) %>%
    mutate(TOTAL_12Days= rollsum(POS_NEW, 12, fill=NA,align="right")
  ) %>%
    mutate(TOTAL_14Days= rollsum(POS_NEW, 14, fill=NA,align="right")
    ) %>%
    mutate(TOTAL_8Days= rollsum(POS_NEW, 8, fill=NA,align="right")
  )
  
}


FullTotalTable = function(usabledata, location) {
  fulltable <-
    usabledata %>% 
    filter(DATE %in% tail(DATE, 7)) %>%
    select(DATE, POSITIVE, NEGATIVE, DEATHS, HOSP_YES, HOSP_UNK, IC_YES) %>%
    rename_at(vars(starts_with("HOSP_")), ~str_to_title(str_replace_all(., "HOSP_","Hospitalization "))) %>%
    rename_at(vars(starts_with("IC_")), ~str_to_title(str_replace_all(., "IC_","ICU "))) %>%   
    rename_at(vars(ends_with("Unk")), ~str_to_title(str_replace_all(., "Unk","Unknown "))) %>%   
    kable(digits = 3,booktabs = T, caption = paste(location, "cumulative counts of COVID cases and outcomes"),align = "c") %>%
    kable_styling(latex_options = c("hold_position", "scale_down"), font_size = 11) %>% 
    row_spec(7, color = "darkblue", background = "#00FFFF") 
  
  fulltable
}


PartialTotalTable = function(usabledata, location) {
  fulltable <-
    usabledata %>% 
    filter(DATE %in% tail(DATE, 10)) %>%
    select(DATE, POSITIVE, NEGATIVE, DEATHS, TOTAL_10Days, TOTAL_14Days) %>%
    rename_at(vars(starts_with("TOTAL_")), ~str_to_title(str_replace_all(., "TOTAL_","New in Last "))) %>%
    rename_at(vars(starts_with("HOSP_")), ~str_to_title(str_replace_all(., "HOSP_","Hospitalization "))) %>%
    kable(digits = 3,booktabs = T, caption = paste(location, "cumulative counts of COVID cases and outcomes"),align = "c") %>%
    kable_styling(latex_options = c("hold_position", "scale_down"), font_size = 11) %>% 
    row_spec(10, color = "darkblue", background = "#00FFFF") 
  
  fulltable
}


DailyCases = function(usabledata, location) {
  DCplot <- ggplot(usabledata, aes(x=as.Date(DATE,"%B %d %Y"), y=POS_NEW))+
  #  geom_line(aes(color=Month))+ 
  geom_line(aes(color="Daily"))+
  geom_line(aes(y=rollmean(POS_NEW, 7, na.pad=TRUE), color="7-day moving avg")) +
  geom_smooth(method = 'loess',aes(color="Loess smooth"))+
  scale_x_date(breaks = date_breaks("14 days"))+
  ggtitle(label = paste(location, "Daily new COVID cases"))+
  theme_minimal()+
  theme(plot.title = element_text(hjust=0.5, lineheight = .8, face = "bold"),
        axis.text.x = element_text(angle=90))+
  ylab("Daily New Cases")+
  xlab("Date") +
  geom_vline(xintercept = c("2020-03-25","2020-08-01") %>% as.Date(), lty = 2, col= "#00FFFF") +
  geom_vline(xintercept = c("2020-05-14","2020-05-25","2020-07-04") %>% as.Date(), lty = 2, col= "purple") +
  scale_colour_manual(name='', values=c('Daily'='green','Loess smooth'='grey','7-day moving avg'='navy')) 
  
#  DCplot <- DCplot %>%
#    
  DCplot
}



ActiveCases = function(usabledata, location) {
  DCplot <- ggplot(usabledata, aes(x=as.Date(DATE,"%B %d %Y"), y=TOTAL_12Days))+
    geom_line(aes(color="12days total"))+
    geom_line(aes(y=TOTAL_10Days, color="10days total")) +
    geom_line(aes(y=TOTAL_14Days, color="14days total"))+
    geom_line(aes(y=POS_NEW, color="Daily Cases")) +
    scale_x_date(date_minor_breaks = "1 week") +
    scale_x_date(breaks = date_breaks("14 days"))+
    ggtitle(label = paste(location, "Estimated Active COVID cases"))+
    theme_minimal()+
    theme(plot.title = element_text(hjust=0.5, lineheight = .8, face = "bold"),
          axis.text.x = element_text(angle=90))+
    ylab("Estimated Active Cases")+
    xlab("Date") +
    scale_colour_manual(name='', values=c('10days total'='navy',
                                          '12days total'='grey','14days total'='red4','15days total'='grey',
                                          'Daily Cases'='green')) 
  
  #  DCplot <- DCplot %>%
  #    
  DCplot
}





# like that from https://i.redd.it/o0lnkamvpn951.png

DailyInfoPlot = function(usabledata) {
  ## plot daily new cases and deaths
  usabledata %>%
    ggplot( aes(x=price)) +
    geom_density(fill="#69b3a2", color="#e9ecef", alpha=0.8)
  
  p1
}

