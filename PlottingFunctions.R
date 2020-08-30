CaseCheck = function(usabledata) {
  usabledata <- usabledata %>%
    mutate(TOTALTESTS = POSITIVE+NEGATIVE)%>% 
    mutate(POSRATE_NEW = POS_NEW/TEST_NEW)%>%
    mutate(log10TOTALTESTS = log10(POSITIVE+NEGATIVE))%>% 
    mutate(log10POSITIVE = log10(POSITIVE))%>% 
    mutate(log10DEATHS = log10(DEATHS)) %>%
    mutate(log10DEATHS = ifelse(log10DEATHS < -10^10,NA, log10DEATHS)) %>% 
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
  mutate(POS_20_29_NEW = c(NA,usabledata$POS_20_29[2:(dim(usabledata)[1])]-
                             usabledata$POS_20_29[1:(dim(usabledata)[1]-1)])) %>%
  mutate(TOTAL_10Days= rollsum(POS_NEW, 10, fill=NA,align="right")) %>%
  mutate(TOTAL_15Days = rollsum(POS_NEW, 15, fill=NA,align="right")) %>%
  mutate(TOTAL_12Days= rollsum(POS_NEW, 12, fill=NA,align="right")) %>%
  mutate(TOTAL_14Days= rollsum(POS_NEW, 14, fill=NA,align="right")) %>%
  mutate(TOTAL_8Days= rollsum(POS_NEW, 8, fill=NA,align="right")) %>%
  mutate(TOTAL_10Days20s= rollsum(POS_20_29_NEW, 10, fill=NA,align="right")) %>%
  mutate(TOTAL_15Days20s = rollsum(POS_20_29_NEW, 15, fill=NA,align="right")) %>%
  mutate(TOTAL_12Days20s= rollsum(POS_20_29_NEW, 12, fill=NA,align="right")) %>%
  mutate(TOTAL_14Days20s= rollsum(POS_20_29_NEW, 14, fill=NA,align="right")) %>%
  mutate(TOTAL_8Days20s= rollsum(POS_20_29_NEW, 8, fill=NA,align="right"))
  
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
    filter(DATE %in% tail(DATE, 7)) %>%
    select(DATE, POSITIVE, NEGATIVE, DEATHS, TOTAL_10Days, TOTAL_14Days) %>%
    rename_at(vars(starts_with("TOTAL_")), ~str_to_title(str_replace_all(., "TOTAL_","New in Last "))) %>%
    rename_at(vars(starts_with("HOSP_")), ~str_to_title(str_replace_all(., "HOSP_","Hospitalization "))) %>%
    kable(digits = 3,booktabs = T, caption = paste(location, "cumulative counts of COVID cases and outcomes"),align = "c") %>%
    kable_styling(latex_options = c("hold_position", "scale_down"), font_size = 11) %>% 
    row_spec(7, color = "darkblue", background = "#00FFFF") 
  
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



ActiveCases = function(usabledata, location, twenties) {
  if (twenties) {
    plotused <- ggplot(usabledata, aes(x=as.Date(DATE,"%B %d %Y"), y=TOTAL_12Days20s))+
      geom_line(aes(color="12days total"))+
      geom_line(aes(y=TOTAL_10Days20s, color="10days total")) +
      geom_line(aes(y=TOTAL_14Days20s, color="14days total"))+
      geom_line(aes(y=POS_20_29_NEW, color="Daily Cases")) +
      ylim(0, max(usabledata$TOTAL_14Days,na.rm=T)) +
      ggtitle(label = paste(location, "Estimated Active COVID cases in 20s"))
  } else {
    plotused <- ggplot(usabledata, aes(x=as.Date(DATE,"%B %d %Y"), y=TOTAL_12Days))+
      geom_line(aes(color="12days total"))+
      geom_line(aes(y=TOTAL_10Days, color="10days total")) +
      geom_line(aes(y=TOTAL_14Days, color="14days total"))+
      geom_line(aes(y=POS_NEW, color="Daily Cases")) +
      ylim(0, max(usabledata$TOTAL_14Days,na.rm=T)) +
      ggtitle(label = paste(location, "Estimated Active COVID cases"))
  }
  DCplot <- plotused + 
    scale_x_date(date_minor_breaks = "1 week") +
    scale_x_date(breaks = date_breaks("14 days"))+
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

DailyTesting = function(usabledata, location) {
  DTplot <- ggplot(usabledata, aes(x=as.Date(DATE,"%B %d %Y"), y=TEST_NEW))+
    #  geom_line(aes(color=Month))+ 
    geom_line(aes(color="Daily"))+
    geom_line(aes(y=rollmean(TEST_NEW, 7, na.pad=TRUE), color="7-day moving avg")) +
    geom_smooth(method = 'loess',aes(color="Loess smooth"))+
    scale_x_date(breaks = date_breaks("14 days"))+
    ggtitle(label = paste(location, "Daily tests"))+
    theme_minimal()+
    theme(plot.title = element_text(hjust=0.5, lineheight = .8, face = "bold"),
          axis.text.x = element_text(angle=90))+
    ylab("Daily Testing")+
    xlab("Date") +
    scale_colour_manual(name='', values=c('Daily'='#32FFFF','Loess smooth'='grey','7-day moving avg'='navy')) 
  DTplot
}

SafetyCheckTable20s = function(usabledata, loc, usablepop,usablepop20s) {
  ##
  n = dim(usabledata)[1]
  ###
  TodayTotal = usabledata$POSITIVE[n]
  Lag14Total = usabledata$POSITIVE[n-14]
  val2wk = round((TodayTotal - Lag14Total) / (usablepop/10000),1)
  ###
  TodayTotal20s = usabledata$POS_20_29[n]
  Lag14Total20s = usabledata$POS_20_29[n-14]
  val2wk20s = (TodayTotal20s - Lag14Total20s) / (usablepop20s/10000)
  usableval2wk20s = round(val2wk20s,1)
  ###
  valdaily7current = round(mean(usabledata$POS_NEW[(n-6):n]) ,1)
  ###
  valdaily7past = round(rollmean(usabledata$POS_NEW, 7, na.pad=T)[n-14],1)
  ###
  #  dailytestrate7 = round(100*mean(usabledata$POSRATE_NEW[(n-6):n]) ,1)
  dailytestrate7 = round(100*(usabledata$POSITIVE[n]-usabledata$POSITIVE[n-7])/
                           (usabledata$TOTALTESTS[n]-usabledata$TOTALTESTS[n-7]),1)
  ###
  
  locnames = paste(loc,c("2wk","2wk20s","current daily7","past daily7","posratedaily7"),sep=" ")
  
  Current = tibble(val2wk,usableval2wk20s,valdaily7current,valdaily7past,paste(dailytestrate7,"%",sep=""),.name_repair = ~locnames) 
  
  backcurrent = "white"; colcurrent = "black"
  if ((val2wk > 20)) {backcurrent = "#778899"; colcurrent = "white"}
  if (valdaily7current > valdaily7past-1) {backcurrent = "#778899"; colcurrent = "white"}
  if ((val2wk > 25) | (usableval2wk20s > 30)) {backcurrent = "darkslateblue"; colcurrent = "white"}
  if ((val2wk > 28) | (usableval2wk20s > 40)) {
    backcurrent = "purple"
    if (valdaily7current > valdaily7past-.5) {backcurrent = "#d100d1"}
  }
  if ((val2wk > 30) | (usableval2wk20s > 50)) {
    backcurrent = "#d100d1"
    if (valdaily7current > valdaily7past-.5) {backcurrent = "#ff0055"}
  }
  
  
  safetytable <-  Current %>%
    kable(escape = F) %>%
    column_spec(1, bold = T, border_left = T, border_right = T,include_thead = T) %>%
    column_spec(2, bold = T, border_right = T,include_thead = T) %>%
    column_spec(3, bold = T, border_right = T,include_thead = T) %>%
    column_spec(4, bold = T, border_right = T,include_thead = T) %>%
    column_spec(5, bold = T, border_right = T,include_thead = T) %>%
    kable_styling(full_width = F, position = "center") %>%
    row_spec(1,background=backcurrent,color=colcurrent) %>%
    row_spec(0, color="black",background="#D3D3D3") 

  safetytable
}



SafetyCheckTable = function(usabledata, loc, usablepop) {
  ##
  n = dim(usabledata)[1]
  ###
  TodayTotal = usabledata$POSITIVE[n]
  Lag14Total = usabledata$POSITIVE[n-14]
  val2wk = round((TodayTotal - Lag14Total) / (usablepop/10000),1)
  ###
  valdaily7current = round(mean(usabledata$POS_NEW[(n-6):n]) ,1)
  ###
  valdaily7past = round(rollmean(usabledata$POS_NEW, 7, na.pad=T)[n-14],1)
  ###
#  dailytestrate7 = round(100*mean(usabledata$POSRATE_NEW[(n-6):n]) ,1)
  dailytestrate7 = round(100*(usabledata$POSITIVE[n]-usabledata$POSITIVE[n-7])/
                           (usabledata$TOTALTESTS[n]-usabledata$TOTALTESTS[n-7]),1)
  ###
  
  locnames = paste(loc,c("2wk","current daily7","past daily7","posratedaily7"),sep=" ")
  
  Current = tibble(val2wk,valdaily7current,valdaily7past,paste(dailytestrate7,"%",sep=""),.name_repair = ~locnames) 


  backcurrent = "white"; colcurrent = "black"
  if ((val2wk > 20)) {backcurrent = "#778899"; colcurrent = "white"}
  if (valdaily7current > valdaily7past-1) {backcurrent = "#778899"; colcurrent = "white"}
  if ((val2wk > 25) ) {backcurrent = "darkslateblue"; colcurrent = "white"}
  if ((val2wk > 28)) {
    backcurrent = "purple"
    if (valdaily7current > valdaily7past-.5) {backcurrent = "#d100d1"}
  }
  if ((val2wk > 30) ) {
    backcurrent = "#d100d1"
    if (valdaily7current > valdaily7past-.5) {backcurrent = "#ff0055"}
  }
  
  
  safetytable <-  Current %>%
    kable(escape = F) %>%
    column_spec(1, bold = T, border_left = T, border_right = T,include_thead = T) %>%
    column_spec(2, bold = T, border_right = T,include_thead = T) %>%
    column_spec(3, bold = T, border_right = T,include_thead = T) %>%
    kable_styling(full_width = F, position = "center") %>%
    row_spec(1,background=backcurrent,color=colcurrent) %>%
    row_spec(0, color="black",background="#D3D3D3") 
  
  safetytable
}


# like that from https://i.redd.it/o0lnkamvpn951.png

DailyInfoPlot = function(usabledata) {
  ## plot daily new cases and deaths
  usabledata %>%
    ggplot( aes(x=price)) +
    geom_density(fill="#69b3a2", color="#e9ecef", alpha=0.8)
  
  p1
}


DailyOutcomes <- function(usabledata, location) {
  
  n=dim(usabledata)[1]
  stackDATE = c(usabledata$DATE,usabledata$DATE)
  stackCOUNTS = c(usabledata$NEG_NEW,usabledata$POS_NEW)
  stackOUTCOMES = c(rep("Negative",n),rep("Positive",n))
  stackratio = rep(usabledata$POS_NEW/usabledata$TEST_NEW,2); stackratio[stackratio<0] = NA
  stackdata = tibble(stackDATE,stackCOUNTS,stackOUTCOMES,stackratio)
  scalingval = max(stackCOUNTS*1.05,na.rm=T)
  # Grouped
  OutPlot <- ggplot(stackdata, aes(fill=stackOUTCOMES, y=stackCOUNTS, x=as.Date(stackDATE,"%B %d %Y"))) + 
    geom_bar(position="stack", stat="identity") +
    ggtitle(label = paste(location, "Daily Negative and Positive Tests"))+
    scale_x_date(breaks = date_breaks("14 days"))+
    theme_minimal()+
    theme(plot.title = element_text(hjust=0.5, lineheight = .8, face = "bold"), 
          axis.text.x = element_text(angle=90))+
    ylab("Tests")+
    xlab("Date") + 
    labs(fill = "Test Outcomes")+
    scale_fill_manual(values=c('lightgray','green')) 
  
  OutPlot
}



DailyChanges <- function(usabledata, location) {
  
  n=dim(usabledata)[1]
  newHOSP_YES = c(NA,usabledata$HOSP_YES[-1]-usabledata$HOSP_YES[-n])
  newHOSP_UNK = c(NA,usabledata$HOSP_UNK[-1]-usabledata$HOSP_UNK[-n])
  stackDATE = rep(usabledata$DATE,3)
  stackCOUNTS = c(usabledata$POS_NEW,
                  newHOSP_YES, usabledata$DTH_NEW)
  stackOUTCOMES = c(rep("Positives",n),
                    rep("Hospitalizations",n),
                    rep("Deaths",n))
  stackdata = tibble(stackDATE,stackCOUNTS,stackOUTCOMES)
  OutPlot <- ggplot(stackdata, aes(fill=stackOUTCOMES, y=stackCOUNTS, x=as.Date(stackDATE,"%B %d %Y"))) + 
    geom_bar(position="dodge", stat="identity") +
    ggtitle(label = paste(location, "Daily Outcomes"))+
    scale_x_date(breaks = date_breaks("14 days"))+
    theme_minimal()+
    theme(plot.title = element_text(hjust=0.5, lineheight = .8, face = "bold"), 
          axis.text.x = element_text(angle=90))+
    ylab("Daily Occurrences")+
    xlab("Date") + 
    labs(fill = "Test Outcomes")+
    scale_fill_manual(values=c(
      '#d100d1','#3399ff','green'
    )) 
  
  OutPlot
}




CumulativeOutcomes  <- function(usabledata, location) {
  
  n=dim(usabledata)[1]
  stackDATE = c(usabledata$DATE,usabledata$DATE)
  stackCOUNTS = c(usabledata$NEGATIVE,usabledata$POSITIVE)
  stackOUTCOMES = c(rep("Negative",n),rep("Positive",n))
  stackdata = tibble(stackDATE,stackCOUNTS,stackOUTCOMES)
  # Grouped
  OutPlot <- ggplot(stackdata, aes(fill=stackOUTCOMES, y=stackCOUNTS, x=as.Date(stackDATE,"%B %d %Y"))) + 
    geom_bar(position="stack", stat="identity") +
    ggtitle(label = paste(location, "Cumulative Negative and Positive Tests"))+
    scale_x_date(breaks = date_breaks("14 days"))+
    theme_minimal()+
    theme(plot.title = element_text(hjust=0.5, lineheight = .8, face = "bold"), 
          axis.text.x = element_text(angle=90))+
    ylab("Tests")+
    xlab("Date") + 
    labs(fill = "Test Outcomes")+
    scale_fill_manual(values=c('lightgray','green')) 
  
  OutPlot
}

CumulativeLines <- function(usabledata, location) {
  LinePlot <- ggplot(usabledata, aes(y=TOTALTESTS, x=as.Date(DATE,"%B %d %Y"))) + 
    geom_line(aes(color="Testing"))+
    geom_line(aes(y=POSITIVE, color="Cases")) +
    geom_line(aes(y=DEATHS, color="Deaths"))+
    geom_line(aes(y=HOSP_YES, color="Hospitalized"))+
    scale_x_date(date_minor_breaks = "1 week", breaks = date_breaks("14 days"))+
    ggtitle(label = paste(location,"cumulative data, log10-scale"))+
    theme_minimal()+
   theme(plot.title = element_text(hjust=0.5, lineheight = .8, face = "bold"),
        axis.text.x = element_text(angle=90))+
    ylab("Cumulative Counts, log10-scale axis")+
    xlab("Date") +
    scale_colour_manual(name='', values=c('Testing'='#32FFFF',
                                        'Cases'='green',
                                        'Hospitalized' = '#3399ff',
                                        'Deaths'='#d100d1')) +
    scale_y_continuous(trans = 'log10',labels = format_format(big.mark = " ", scientific = FALSE))

  LinePlot 
}
