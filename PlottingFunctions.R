UniversityDates = c("2020-08-31","2021-09-01","2022-09-02")
# adjust Date range?
MaskingDates = c()

###############################################
###########INITIAL DATA computations###########
###############################################

CaseCheck = function(usabledata) {
  usabledata <- usabledata %>%
    mutate(TOTALTESTS = POSITIVE+NEGATIVE)%>% 
    mutate(POSRATE_NEW = POS_NEW/TEST_NEW)%>%
    mutate(log10TOTALTESTS = log10(POSITIVE+NEGATIVE))%>% 
    mutate(log10POSITIVE = log10(POSITIVE))%>% 
    mutate(log10DEATHS = log10(DEATHS)) %>%
    mutate(log10DEATHS = ifelse(log10DEATHS < -10^10,NA, log10DEATHS)) %>% 
#    mutate(DATE = format(mdy(DATE), format = "%B %d %Y")) %>%   
#    mutate(DATE = format(DATE, format = "%B %d %Y")) %>%   
#    mutate(DATE = format(mdy(DATE), format = "%B %d %Y")) %>%   
    mutate(DATE = format(DATE, format = "%B %d %Y")) %>%   
    mutate(DTH_NEW = as.numeric(DTH_NEW))  %>%
  mutate(DTH_NEW = case_when(is.na(DTH_NEW) ~ 0,
                             DTH_NEW <0 ~ 0,   
                             c(DTH_NEW[-1],0) < 0 ~ +1,
                             TRUE ~ DTH_NEW
                             )
  ) %>%
  mutate(POS_NEW = c(NA,POSITIVE[2:length(POSITIVE)]-POSITIVE[1:(length(POSITIVE)-1)])) %>%   #  NEWLY ADDED LINE
  mutate(POS_NEW = as.numeric(POS_NEW))  %>%
  mutate(POS_NEW = case_when(is.na(POS_NEW) ~ 0,
                             POS_NEW <0 ~ 0,   
                             c(POS_NEW[-1],0) < 0 ~ +1,
                             TRUE ~ POS_NEW
                             )
  ) %>%
  mutate(POS_7DAY_AVG = rollmean(POS_NEW, 7, na.pad=TRUE)) %>%
  mutate(HOSP_NEW = c(NA,HOSP_YES[2:length(HOSP_YES)]-HOSP_YES[1:(length(HOSP_YES)-1)])) %>%
  mutate(HOSP_7DAY_AVG = rollmean(HOSP_NEW, 7, na.pad=TRUE)) %>%
  mutate(HOSP_14DAY_AVG = rollmean(HOSP_NEW, 14, na.pad=TRUE)) %>%
  mutate(DTH_7DAY_AVG = rollmean(DTH_NEW, 7, na.pad=TRUE)) %>%
  mutate(DTH_14DAY_AVG = rollmean(DTH_NEW, 14, na.pad=TRUE)) %>%
  mutate(TEST_7DAY_AVG = rollmean(TEST_NEW, 7, na.pad=TRUE)) %>%
  mutate(POS_0_9 = ifelse(POS_0_9 < 0, NA, POS_0_9)) %>%
  mutate(POS_10_19 = ifelse(POS_10_19 < 0, NA, POS_10_19)) %>%
  mutate(POS_20_29 = ifelse(POS_20_29 < 0, NA, POS_20_29)) %>%
  mutate(POS_30_39 = ifelse(POS_30_39 < 0, NA, POS_30_39)) %>%
  mutate(POS_40_49 = ifelse(POS_40_49 < 0, NA, POS_40_49)) %>%
  mutate(POS_50_59 = ifelse(POS_50_59 < 0, NA, POS_50_59)) %>%
  mutate(POS_60_69 = ifelse(POS_60_69 < 0, NA, POS_60_69)) %>%
  mutate(POS_70_79 = ifelse(POS_70_79 < 0, NA, POS_70_79)) %>%
  mutate(POS_80_89 = ifelse(POS_80_89 < 0, NA, POS_80_89)) %>%
  mutate(POS_90 = ifelse(POS_90 < 0, NA, POS_90)) %>%
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


###############################################
########FUNCTIONS in "Daily" tab - left########
###############################################


DailyCases = function(usabledata, location) {
  DCplot <- ggplot(usabledata, aes(x=as.Date(DATE,"%B %d %Y"), y=POS_NEW))+
  #  geom_line(aes(color=Month))+ 
  geom_line(aes(color="Daily"))+
  geom_line(aes(y=rollmean(POS_NEW, 7, na.pad=TRUE), color="7-day avg")) +
#  geom_smooth(method = 'loess',aes(color="Loess smooth"))+
  scale_x_date(breaks = date_breaks("28 days"))+
  ggtitle(label = paste(location, "Daily new COVID cases, confirmed+probable"))+
  theme_minimal()+
  theme(plot.title = element_text(hjust=0.5, lineheight = .8, face = "bold"),
        axis.text.x = element_text(angle=90))+
  ylab("Daily New Cases")+
  xlab("Date") +
  geom_vline(xintercept = UniversityDates %>% as.Date(), lty = 2, col= "purple") +
  geom_vline(xintercept = MaskingDates %>% as.Date(), lty = 2, col= "blue") +
  scale_colour_manual(name='', values=c('Daily'='green','Loess smooth'='grey','7-day avg'='navy')) 
  
  DCplot
}


DailybyAge <- function(usabledata, location,sumlag=1) {

  n=dim(usabledata)[1]
  usablesub <- select(usabledata, POS_0_9, POS_10_19)
  usabledata$POS_below20 = rowSums(usablesub,na.rm=T)
  usabledata$POS_NEW_below20 <- c(rep(NA,sumlag),
    usabledata$POS_below20[(1+sumlag):n]-
    usabledata$POS_below20[1:(n-sumlag)])

  usabledata$POS_NEW_20s <- c(rep(NA,sumlag),
                              usabledata$POS_20_29[(1+sumlag):n]-
                                usabledata$POS_20_29[1:(n-sumlag)])
  
  usablesub <- select(usabledata, POS_30_39, POS_40_49)
  usabledata$POS_30_49 = rowSums(usablesub,na.rm=T)
  usabledata$POS_NEW_30_49 <- c(rep(NA,sumlag),
                                usabledata$POS_30_49[(1+sumlag):n]-
                                  usabledata$POS_30_49[1:(n-sumlag)])

  usablesub <- select(usabledata, POS_50_59, POS_60_69, POS_70_79,POS_80_89,POS_90)
  usabledata$POS_50plus = rowSums(usablesub,na.rm=T)
  usabledata$POS_NEW_50plus <- c(rep(NA,sumlag),
                                 usabledata$POS_50plus[(1+sumlag):n]-
                                   usabledata$POS_50plus[1:(n-sumlag)])
  
  DAplot <- ggplot(usabledata, aes(x=as.Date(DATE,"%B %d %Y"), y=POS_NEW))+
    geom_line(aes(y=rollmean(POS_NEW_below20, 7, na.pad=TRUE), color="ages 0-19")) +
    geom_line(aes(y=rollmean(POS_NEW_20s, 7, na.pad=TRUE), color="ages 20-29")) +
    geom_line(aes(y=rollmean(POS_NEW_30_49, 7, na.pad=TRUE), color="ages 30-49")) +
    geom_line(aes(y=rollmean(POS_NEW_50plus, 7, na.pad=TRUE), color="ages 50+")) +
    scale_x_date(breaks = date_breaks("28 days"))+
    ggtitle(label = paste(location, "Daily new cases (7-day avg) by Ages, confirmed+probable"))+
    theme_minimal()+
    theme(plot.title = element_text(hjust=0.5, lineheight = .8, face = "bold"),
          axis.text.x = element_text(angle=90))+
    ylab("Daily New Cases")+
    xlab("Date") +
    geom_vline(xintercept = UniversityDates %>% as.Date(), lty = 2, col= "purple") +
    geom_vline(xintercept = MaskingDates %>% as.Date(), lty = 2, col= "blue") +
    scale_colour_manual(name='', values=c(  "ages 0-19"='#DAA520',
                                            "ages 20-29"='#6B8E23',
                                            "ages 30-49"='turquoise',
                                            "ages 50+"='orchid')) 
  
  DAplot
}


DailyTesting = function(usabledata, location) {
  DTplot <- ggplot(usabledata, aes(x=as.Date(DATE,"%B %d %Y"), y=TEST_NEW))+
    #  geom_line(aes(color=Month))+ 
    geom_line(aes(color="Daily"))+
    geom_line(aes(y=rollmean(TEST_NEW, 7, na.pad=TRUE), color="7-day moving avg")) +
    geom_smooth(method = 'loess',aes(color="Loess smooth"))+
    scale_x_date(breaks = date_breaks("28 days"))+
    ggtitle(label = paste(location, "Daily tests"))+
    theme_minimal()+
    theme(plot.title = element_text(hjust=0.5, lineheight = .8, face = "bold"),
          axis.text.x = element_text(angle=90))+
    ylab("Daily Testing")+
    xlab("Date") +
    scale_colour_manual(name='', values=c('Daily'='#32FFFF','Loess smooth'='grey','7-day moving avg'='navy')) 
  DTplot
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
    ggtitle(label = paste(location, "Daily NEW Negative and Positive Tests, confirmed+probable"))+
    scale_x_date(breaks = date_breaks("28 days"))+
    theme_minimal()+
    theme(plot.title = element_text(hjust=0.5, lineheight = .8, face = "bold"), 
          axis.text.x = element_text(angle=90))+
    ylab("Tests")+
    xlab("Date") + 
    labs(fill = "Test Outcomes")+
    scale_fill_manual(values=c('lightgray','green')) 
  
  OutPlot
}




##############################################
#######FUNCTIONS in "Daily" tab - middle#######
##############################################

Daily7day = function(usabledata, location) {
  D7plot <- ggplot(usabledata, aes(x=as.Date(DATE,"%B %d %Y"), y=TEST_7DAYAVG))+
    geom_line(aes(color="Testing"))+
    geom_line(aes(y=POS_7DAYAVG, color="New-Cases")) +    
    geom_line(aes(y=DTH_7DAY_AVG, color="Deaths"))+
    geom_line(aes(y=HOSP_7DAY_AVG, color="Hospitalized"))+
    scale_x_date(breaks = date_breaks("28 days"))+
    ggtitle(label = paste(location, "7-day averages,testing & positives"))+
    theme_minimal()+
    theme(plot.title = element_text(hjust=0.5, lineheight = .8, face = "bold"),
          axis.text.x = element_text(angle=90))+
    ylab("Daily 7day Average")+
    xlab("Date") +
    scale_colour_manual(name='', values=c('New-Cases'='green4',
                                          'Testing'='#32FFFF',
                                          'Hospitalized'='#3399ff',
                                          'Deaths'='#d100d1')) 
  D7plot
}

Daily7Cases = function(usabledata, location) {
  D7Cplot <- ggplot(usabledata, aes(x=as.Date(DATE,"%B %d %Y"), y=POS_7DAYAVG))+
    geom_line(aes(color="New-Cases")) +    
    geom_line(aes(y=DTH_7DAY_AVG, color="Deaths"))+
    geom_line(aes(y=HOSP_7DAY_AVG, color="Hospitalized"))+
    scale_x_date(breaks = date_breaks("28 days"))+
    ggtitle(label = paste(location, "7-day averages, positives & hosp"))+
    theme_minimal()+
    theme(plot.title = element_text(hjust=0.5, lineheight = .8, face = "bold"),
          axis.text.x = element_text(angle=90))+
    ylab("Daily 7day Average")+
    xlab("Date") +
    scale_colour_manual(name='', values=c('New-Cases'='green4',
                                          'Testing'='#32FFFF',
                                          'Hospitalized'='#3399ff',
                                          'Deaths'='#d100d1')) 
  D7Cplot
}

Daily7Hosp = function(usabledata, location) {
  D7Hplot <- ggplot(usabledata, aes(x=as.Date(DATE,"%B %d %Y"), y=HOSP_7DAY_AVG))+
    geom_line(aes(color="Hospitalized")) +    
    geom_line(aes(y=DTH_7DAY_AVG, color="Deaths"))+
    scale_x_date(breaks = date_breaks("28 days"))+
    ggtitle(label = paste(location, "7-day averages, hospitalizations & deaths"))+
    theme_minimal()+
    theme(plot.title = element_text(hjust=0.5, lineheight = .8, face = "bold"),
          axis.text.x = element_text(angle=90))+
    ylab("Daily 7day Average")+
    xlab("Date") +
    scale_colour_manual(name='', values=c('New-Cases'='green4',
                                          'Testing'='#32FFFF',
                                          'Hospitalized'='#3399ff',
                                          'Deaths'='#d100d1')) 
  D7Hplot
}

Daily14Hosp = function(usabledata, location) {
  D14Hplot <- ggplot(usabledata, aes(x=as.Date(DATE,"%B %d %Y"), y=HOSP_14DAY_AVG))+
    geom_line(aes(color="Hospitalized")) +    
    geom_line(aes(y=DTH_14DAY_AVG, color="Deaths"))+
    scale_x_date(breaks = date_breaks("28 days"))+
    ggtitle(label = paste(location, "14-day averages, hospitalizations & deaths"))+
    theme_minimal()+
    theme(plot.title = element_text(hjust=0.5, lineheight = .8, face = "bold"),
          axis.text.x = element_text(angle=90))+
    ylab("Daily 14day Average")+
    xlab("Date") +
    scale_colour_manual(name='', values=c('New-Cases'='green4',
                                          'Testing'='#32FFFF',
                                          'Hospitalized'='#3399ff',
                                          'Deaths'='#d100d1')) 
  D14Hplot
}

POSRATE = function(usabledata, location) {
  usabledata$POS_RATE_7DAY <- pmax(0,rollmean(rollmean(usabledata$POS_NEW,3,na.pad=TRUE)/
                                         rollmean(usabledata$TEST_NEW,3,na.pad=TRUE),7,na.pad=TRUE))
  POSplot <- ggplot(usabledata, aes(x=as.Date(DATE,"%B %d %Y"), y=POS_RATE_7DAY))+
    geom_line(aes(color="Pos. Rate  "))+
    scale_x_date(breaks = date_breaks("28 days"))+
    ggtitle(label = paste(location, "Smoothed Positivity Rate"))+
    theme_minimal()+
    theme(plot.title = element_text(hjust=0.5, lineheight = .8, face = "bold"),
          axis.text.x = element_text(angle=90))+
    ylab("Pos. Rate (%) 7day Avg")+
    xlab("Date") +
    scale_colour_manual(name='', values=c('Pos. Rate  '='navy')) + 
    scale_y_continuous(labels = scales::percent)
  POSplot
}





##############################################
#######FUNCTIONS in "Daily" tab - right#######
##############################################


ActiveSumbyAge <- function(usabledata, location,sumlag) {
  
  n=dim(usabledata)[1]
  usablesub <- select(usabledata, POS_70_79,POS_80_89,POS_90)
  POS_70plus = rowSums(usablesub,na.rm=T)
  stackDATE = rep(usabledata$DATE[(1+sumlag):n],8)
  stackCOUNTS = c(usabledata$POS_0_9[(1+sumlag):n]-usabledata$POS_0_9[1:(n-sumlag)], 
                  usabledata$POS_10_19[(1+sumlag):n]-usabledata$POS_10_19[1:(n-sumlag)],
                  usabledata$POS_20_29[(1+sumlag):n]-usabledata$POS_20_29[1:(n-sumlag)],
                  usabledata$POS_30_39[(1+sumlag):n]-usabledata$POS_30_39[1:(n-sumlag)],
                  usabledata$POS_40_49[(1+sumlag):n]-usabledata$POS_40_49[1:(n-sumlag)],
                  usabledata$POS_50_59[(1+sumlag):n]-usabledata$POS_50_59[1:(n-sumlag)],
                  usabledata$POS_60_69[(1+sumlag):n]-usabledata$POS_60_69[1:(n-sumlag)],
                  POS_70plus[(1+sumlag):n]-POS_70plus[1:(n-sumlag)])
  ageGroup = c(rep("Ages 0-9",n-sumlag),
                    rep("Ages 10-19",n-sumlag),
                    rep("Ages 20-29",n-sumlag),
                    rep("Ages 30-39",n-sumlag),
                    rep("Ages 40-49",n-sumlag),
                    rep("Ages 50-59",n-sumlag),
                    rep("Ages 60-69",n-sumlag),
                    rep("Ages 70 or more",n-sumlag))
  stackdata = tibble(stackDATE,stackCOUNTS,ageGroup)
  graphlag <- ifelse(sumlag < 14,"(conservative estimate)","(MN-Safety-Plan)")
  OutPlot <- ggplot(stackdata, aes(fill=ageGroup, y=stackCOUNTS, x=as.Date(stackDATE,"%B %d %Y"))) + 
    geom_area() +
    ggtitle(label = paste(location, "Active Cases",graphlag))+
    scale_x_date(breaks = date_breaks("28 days"))+
    theme_minimal()+
    theme(plot.title = element_text(hjust=0.5, lineheight = .8, face = "bold"), 
          axis.text.x = element_text(angle=90))+
    geom_vline(xintercept = UniversityDates %>% as.Date(), lty = 2, col= "purple") +
    geom_vline(xintercept = MaskingDates %>% as.Date(), lty = 2, col= "blue") +
    ylab(paste("Active Cases as sum of", sumlag,"days"))+
    xlab("Date") 
  
  OutPlot
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
    scale_x_date(breaks = date_breaks("28 days"))+
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

##############################################
###FUNCTIONS in "By the Numbers" tab - left###
##############################################

FullTotalTable = function(usabledata, location) {
  fulltable <-
    usabledata %>% 
    filter(DATE %in% tail(DATE, 7)) %>%
    mutate(DATEnew = format(as.Date(DATE, format = "%b %d"),"%b %d")) %>%
    select(DATEnew, POSITIVE, NEGATIVE, DEATHS, HOSP_YES, HOSP_UNK, IC_YES) %>%
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
    mutate(DATEnew = format(as.Date(DATE, format = "%b %d"),"%b %d")) %>%
    select(DATEnew, POSITIVE, NEGATIVE, HOSP_YES, DEATHS, TOTAL_10Days) %>%
    rename_at(vars(starts_with("TOTAL_")), ~str_to_title(str_replace_all(., "TOTAL_","New in Last "))) %>%
    rename_at(vars(starts_with("HOSP_")), ~str_to_title(str_replace_all(., "HOSP_","Hospitalization "))) %>%
    kable(digits = 3,booktabs = T, caption = paste(location, "cumulative counts of COVID cases and outcomes"),align = "c") %>%
    kable_styling(latex_options = c("hold_position", "scale_down"), font_size = 11) %>% 
    row_spec(7, color = "darkblue", background = "#00FFFF") 
  
  fulltable
}

TablebyAge <- function(usabledata,location,usableprops,usablepop) {
  HeadertoUse <- c("Age Group",
                   "Daily New Cases",
                   "Daily New Cases 7-day Average",
                   "Percent of All Active Cases",
                   "Actives (conservative estimate)",
                   "Percent of Subpopulation (conservative)")
  n=dim(usabledata)[1]
  usablesub <- select(usabledata, POS_0_9, POS_10_19, POS_20_29, 
                      POS_30_39, POS_40_49, POS_50_59, POS_60_69,
                      POS_70_79,POS_80_89,POS_90,POSITIVE)
  usablesub$POS_70plus = rowSums(usablesub[,8:10],na.rm=T)
  usabletotable <- select(usablesub,POS_0_9, POS_10_19, POS_20_29, 
                          POS_30_39, POS_40_49, POS_50_59, POS_60_69,
                          POS_70plus,POSITIVE)
  ageGroup = c("0-9","10-19","20-29",
               "30-39","40-49","50-59","60-69",
               "70+","All ages")
  DailyNew = usabletotable[n,]-usabletotable[n-1,]
  DailyNewAvg = round((usabletotable[n,]-usabletotable[n-7,])/7,1)
  Actives10day = usabletotable[n,]-usabletotable[n-10,]
  PropAll10day = round(Actives10day/Actives10day[1,9],3)*100
  Prop10day = round(Actives10day/(c(usableprops,1)*usablepop),4)*100
  Actives14day = usabletotable[n,]-usabletotable[n-14,]
  Prop14day = round(Actives14day/(c(usableprops,1)*usablepop),4)*100
  
  DecisionTable <- tibble(ageGroup,
                          DailyNew=as.numeric(DailyNew),
                          DailyNewAvg=as.numeric(DailyNewAvg),
                          PropAll10day=paste(as.numeric(PropAll10day),"%",sep=""),
                          Actives10day=as.numeric(Actives10day),
                          Prop10day=paste(as.numeric(Prop10day),"%",sep=""),
                          .name_repair = ~HeadertoUse)
  
  AgeTable <-  kable(DecisionTable) %>%
    kable_styling(full_width = F) %>%
    row_spec(1,background = "#FF6347") %>%
    row_spec(2,background = "#DAA520",color="black") %>%
    row_spec(3,background = "#6B8E23",color = "black") %>%
    row_spec(4, background = "#32CD32") %>%
    row_spec(5,background = "turquoise") %>%
    row_spec(6,background = "#45b3e0") %>%
    row_spec(7,background = "orchid") %>%
    row_spec(8,background = "hotpink") %>%
    row_spec(9,background = "white",color = "black") %>%
    row_spec(0, color="black")

  AgeTable
}





###############################################
###FUNCTIONS in "By the Numbers" tab - right###
###############################################

SafetyCheckTablecollege = function(usabledata, loc, usablepop,usablepop10s, usablepop20s, lagdays) {
  ##
  n = dim(usabledata)[1]
  ###
  TodayTotal = usabledata$POSITIVE[n]
  LagTotal = usabledata$POSITIVE[n-lagdays]
  val2wk = round((TodayTotal - LagTotal) / (usablepop/10000),1)
  ###
  TodayTotal20s = usabledata$POS_20_29[n]
  LagTotal20s = usabledata$POS_20_29[n-lagdays]
  val2wk20s = (TodayTotal20s - LagTotal20s) / (usablepop20s/10000)
  usableval2wk20s = round(val2wk20s,1)
  ###
  TodayTotal10s = usabledata$POS_10_19[n]
  LagTotal10s = usabledata$POS_10_19[n-lagdays]
  val2wk10s = (TodayTotal10s - LagTotal10s) / (usablepop10s/10000)
  usableval2wk10s = round(val2wk10s,1)
  ###
  valdaily7current = round(mean(usabledata$POS_NEW[(n-6):n]) ,1)
  ###
  valdaily7past = round(rollmean(usabledata$POS_NEW, 7, na.pad=T)[n-14],1)
  ###
  #  dailytestrate7 = round(100*mean(usabledata$POSRATE_NEW[(n-6):n]) ,1)
  dailytestrate7 = round(100*(usabledata$POSITIVE[n]-usabledata$POSITIVE[n-7])/
                           (usabledata$TOTALTESTS[n]-usabledata$TOTALTESTS[n-7]),1)
  ###
  
  locnames = paste(loc,c("2wk","2wk10s","2wk20s","current daily7","past daily7","posratedaily7"),sep=" ")
  if (lagdays < 14)  locnames[1:2] = paste(loc,paste(lagdays,c("daystotal","daystotal20s"),sep=""),sep=" ")
  Current = tibble(val2wk,usableval2wk10s,usableval2wk20s,valdaily7current,valdaily7past,paste(dailytestrate7,"%",sep=""),.name_repair = ~locnames) 
  
  backcurrent = "white"; colcurrent = "black"
  if ((val2wk > 20)) {backcurrent = "#778899"; colcurrent = "white"}
  if (valdaily7current > valdaily7past-1) {backcurrent = "#778899"; colcurrent = "white"}
  if ((val2wk > 25) | (usableval2wk20s > 30)) {backcurrent = "darkslateblue"; colcurrent = "white"}
  if ((val2wk > 28) | (usableval2wk20s > 45)) {
    backcurrent = "purple"
    if (valdaily7current > valdaily7past-.5) {backcurrent = "#d100d1"}
  }
  if ((val2wk > 30) | (usableval2wk20s > 50)) {
    backcurrent = "#d100d1"
    if (valdaily7current > valdaily7past-.5) {backcurrent = "#ff0055"}
  }
  
  
  safetytable <-  Current %>%
    kable(escape = F) %>%
#    column_spec(1, bold = T, border_left = T, border_right = T,include_thead = T) %>%
#    column_spec(2, bold = T, border_right = T,include_thead = T) %>%
#    column_spec(3, bold = T, border_right = T,include_thead = T) %>%
#    column_spec(4, bold = T, border_right = T,include_thead = T) %>%
#    column_spec(5, bold = T, border_right = T,include_thead = T) %>%
#    column_spec(6, bold = T, border_right = T,include_thead = T) %>%
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



###############################################
#########FUNCTIONS in "Cumulative" tab#########
###############################################

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
    scale_x_date(breaks = date_breaks("28 days"))+
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
    scale_x_date(date_minor_breaks = "1 week", breaks = date_breaks("28 days"))+
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

CumulativebyAge <- function(usabledata, location) {
  
  n=dim(usabledata)[1]
  usablesub <- select(usabledata, POS_70_79,POS_80_89,POS_90)
  POS_70plus = rowSums(usablesub,na.rm=T)
  stackDATE = rep(usabledata$DATE,8)
  stackCOUNTS = c(usabledata$POS_0_9, 
                  usabledata$POS_10_19,
                  usabledata$POS_20_29,
                  usabledata$POS_30_39,
                  usabledata$POS_40_49,
                  usabledata$POS_50_59,
                  usabledata$POS_60_69,
                  POS_70plus)
  ageGroup = c(rep("Ages 0-9",n),
                    rep("Ages 10-19",n),
                    rep("Ages 20-29",n),
                    rep("Ages 30-39",n),
                    rep("Ages 40-49",n),
                    rep("Ages 50-59",n),
                    rep("Ages 60-69",n),
                    rep("Ages 70 or more",n))
  stackdata = tibble(stackDATE,stackCOUNTS,ageGroup)
  OutPlot <- ggplot(stackdata, aes(fill=ageGroup, y=stackCOUNTS, x=as.Date(stackDATE,"%B %d %Y"))) + 
    geom_area() +
    ggtitle(label = paste(location, "Cumulative Cases by Age"))+
    scale_x_date(breaks = date_breaks("28 days"))+
    theme_minimal()+
    theme(plot.title = element_text(hjust=0.5, lineheight = .8, face = "bold"), 
          axis.text.x = element_text(angle=90))+
    ylab("Cumulative Cases")+
    xlab("Date") 
  
  OutPlot
}


###############################################
##########FUNCTIONS in "Modeling" tab##########
###############################################




##############################################
###############UNUSED FUNCTIONS###############
##############################################

AvgDayWeekTable = function(usabledata, location) {
  n <- dim(usabledata)[1]; k <- floor(n/7)
  weekdayshort = c("Sun","Mon", "Tues", "Wed","Thurs", "Fri", "Sat")
  addon <- (n %% 7) > 0
  dayadjust <- rep(weekdayshort, k)
  if (addon)    dayadjust <- c(dayadjust,weekdayshort[1:(n%%7)])
  usableadjust <- usabledata; usableadjust$WEEKDAY = dayadjust
  fulltable <-
    usableadjust %>% 
    select(TEST_NEW, POS_NEW,WEEKDAY) %>%
    group_by(WEEKDAY)  %>%
    summarise(MedianDailyTest = round(median(TEST_NEW,na.rm=T),0),
              MedianDailyPos = round(median(POS_NEW,na.rm=T),0)) %>%
    kable()
  
  fulltable
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
    scale_x_date(breaks = date_breaks("28 days"))+
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


DailyInfoPlot = function(usabledata) {
  ## plot daily new cases and deaths
  usabledata %>%
    ggplot( aes(x=price)) +
    geom_density(fill="#69b3a2", color="#e9ecef", alpha=0.8)
  
  p1
}

