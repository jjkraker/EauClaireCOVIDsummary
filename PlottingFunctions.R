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
  partialtable <-
    usabledata %>% 
    filter(DATE %in% tail(DATE, 7)) %>%
    select(DATE, POSITIVE, NEGATIVE, DEATHS) %>%
    kable(digits = 3,booktabs = T, caption = paste(location, "cumulative counts of COVID cases and outcomes"),align = "c") %>%
    kable_styling(latex_options = c("hold_position", "scale_down"), font_size = 11) %>% 
    row_spec(7, color = "darkblue", background = "#00FFFF") 
  
  partialtable
}



DailyCases = function(usabledata, location) {
  DCplot <- ggplot(usabledata, aes(x=as.Date(DATE,"%B %d %Y"), y=POS_NEW))+
  #  geom_line(aes(color=Month))+ 
  geom_line(aes(color="Daily"))+
  geom_line(aes(y=rollmean(POS_NEW, 7, na.pad=TRUE), color="7-day moving avg")) +
  geom_smooth(method = 'loess',aes(color="Loess smooth"))+
  scale_x_date(breaks = date_breaks("7 days"))+
  ggtitle(label = paste(location, "Daily new COVID cases"))+
  theme_minimal()+
  theme(plot.title = element_text(hjust=0.5, lineheight = .8, face = "bold"),
        axis.text.x = element_text(angle=90))+
  ylab("Daily New Cases")+
  xlab("Date") +
  geom_vline(xintercept = c("2020-03-25","2020-07-20") %>% as.Date(), lty = 2, col= "green") +
  geom_vline(xintercept = c("2020-05-14","2020-05-25","2020-07-04") %>% as.Date(), lty = 2, col= "purple") +
  scale_colour_manual(name='', values=c('Daily'='navy','Loess smooth'='grey','7-day moving avg'='red')) 
  
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