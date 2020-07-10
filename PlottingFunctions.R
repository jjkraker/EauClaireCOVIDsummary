CaseCheck = function(usabledata) {
  usabledata <- usabledata %>% 
  mutate(DATE = format(as_date(DATE), format = "%B %d %Y")) %>%   
  mutate(DTH_NEW = as.numeric(DTH_NEW))  %>%
  mutate(DTH_NEW = case_when(is.na(DTH_NEW) ~ 0,
                             DTH_NEW <0 ~ 0,   
                             c(DTH_NEW[-1],0) < 0 ~ +1,
                             TRUE ~ DTH_NEW
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







# like that from https://i.redd.it/o0lnkamvpn951.png

DailyInfoPlot = function(usabledata) {
  ## plot daily new cases and deaths
  usabledata %>%
    ggplot( aes(x=price)) +
    geom_density(fill="#69b3a2", color="#e9ecef", alpha=0.8)
  
  p1
}