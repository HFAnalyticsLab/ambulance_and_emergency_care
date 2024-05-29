#Ambulance handover times 


# Housekeeping ------------------------------------------------------------

rm(list=ls())

#Library
library(aws.s3)
library(tidyverse)
library(ggplot2)
library(ISOweek)
library(ggh4x)
library(tsibble)
library(lubridate)

# Data load ---------------------------------------------------------------
buck<-'thf-dap-tier0-projects-iht-067208b7-projectbucket-1mrmynh0q7ljp/ambulance/clean'

#amball <- readRDS("amball201722.rds")

amball<-s3read_using(read.csv # Which function are we using to read
                      , object = 'amball201724.csv' # File to open
                      , bucket = buck) # Bucket name defined above



# Format data -------------------------------------------------------------

#Set dates for the different years
date_1<-format(as.Date(seq(ymd('2017-11-20'),ymd('2018-03-04'),by='1 day')),"%Y-%m-%d")
date_2<-format(as.Date(seq(ymd('2018-12-03'),ymd('2019-03-03'), by='1 day')),"%Y-%m-%d")
date_3<-format(as.Date(seq(ymd('2019-12-02'),ymd('2020-03-01'), by='1 day')),"%Y-%m-%d")
date_4<-format(as.Date(seq(ymd('2020-11-30'),ymd('2021-04-04'), by='1 day')),"%Y-%m-%d")
date_5<-format(as.Date(seq(ymd('2021-11-29'),ymd('2022-04-03'), by='1 day')),"%Y-%m-%d")
date_6<-format(as.Date(seq(ymd('2022-11-14'),ymd('2023-04-02'), by='1 day')),"%Y-%m-%d")
date_7<-format(as.Date(seq(ymd('2023-11-20'),ymd('2024-03-31'), by='1 day')),"%Y-%m-%d") 



amball <- amball %>%
  mutate(monthyear=format(as.Date(date), "%Y-%m")) %>% 
  mutate(start_week=ISOweek2date(str_c(date2,"-1"))) %>% 
  mutate(year=case_when(date %in% date_1~ "Winter 2017/18",
                        date %in% date_2~ "Winter 2018/19",
                        date %in% date_3~ "Winter 2019/20",
                        date %in% date_4~ "Winter 2020/21",
                        date %in% date_5~ "Winter 2021/22", 
                        date %in% date_6~ "Winter 2022/23", 
                        date %in% date_7~ "Winter 2023/24"))



# Data for flourish  -------------------------------------------------------------
summamb <- amball %>%
  drop_na(pctdelay60plus) %>%
  drop_na(pctdelay3060) %>%
  group_by(date2) %>%
  mutate(mean60plus=mean(pctdelay60plus), mean3060=mean(pctdelay3060), n=n()) %>% 
  mutate(mean30plus=mean3060+mean60plus) %>% 
  select(c(start_week,date2, year,mean60plus,mean3060,mean30plus, n)) %>% 
  distinct() %>% 
  mutate(week=str_sub(date2,6,8)) %>% 
  ungroup() %>% 
  mutate(start_week_lab=format(as.Date(start_week), "%d %b %y")) %>% 
  mutate(monthyear=format(start_week, "%b %y")) 
         
#mutate(start_week=format(as.Date(start_week), "%d %b %y")) %>% 
write.csv(summamb,'flourish_amb_handovers.csv')

# Calculations- summary of handover delays by years -----------------------

calcs<-amball %>% 
  drop_na(pctdelay60plus) %>%
  drop_na(pctdelay3060) %>%
  group_by(year) %>%
  summarise_if(is.numeric, sum, na.rm = TRUE) %>% 
  mutate(numdelay30plus=numdelays60plus+numdelays3060, pctdelay30plus=(numdelay30plus/denom)*100,
         pctdelay60plus=(numdelays60plus/denom)*100,
         pctdelay3060=(numdelays3060/denom)*100)

write.csv(calcs, 'calcs.csv')


# Visualise data ----------------------------------------------------------

plot_data<-summamb %>% 
  select(monthyear,start_week, start_week_lab, year, mean60plus, mean3060, week) %>% 
  pivot_longer(mean60plus:mean3060,names_to="metric", values_to="prop") %>% 
  mutate(met_lab=factor(metric, levels=c("mean3060", "mean60plus"), 
                        labels=c("Handovers within 30-60 mins(%)", "Handovers taking more than 60 mins(%)"))) 

plot_data %>% 
  ggplot(.,aes(x=start_week, y=as.numeric(prop), fill=met_lab,  group=met_lab, order=met_lab))+
  geom_area(position=position_stack(reverse=TRUE))+
  # scale_x_yearmonth(breaks = '1 month',date_labels = "%b %g")+
  # scale_x_date(date_breaks = '1 month', date_labels ="%b %g")
  # scale_x_discrete(labels=unique(plot_data$monthyear))+
  theme_THF()+
  facet_grid(cols=vars(year), scales="free")+
  scale_fill_manual(values=c('#53a9cd','#dd0031'))+
  labs(x = "", y="Handovers over 30 mins (%)", caption = "NHS England, Urgent and Emergency Care Daily Situation Reports")+
  theme(legend.text=element_text(size=11),
        legend.title = element_blank(),
        # axis.text.x = element_blank(),
        axis.text.x=element_text(size=11, angle=60),
        axis.text.y=element_text(size=11),
        plot.caption = element_markdown(hjust=0, size=9),
        plot.margin = unit(c(1,1.5,0.5,0.5), "cm"),
        legend.margin=margin(0,0,0,0),
        legend.box.margin=margin(-10,-10,-10,-10))


plot_data

t<-summamb %>% 
  pivot_longer(mean60plus:mean3060,names_to="metric", values_to="prop") %>% 
  mutate(met_lab=factor(metric, levels=c("mean3060", "mean60plus"), 
                        labels=c("Handovers within 30-60 mins(%)", "Handovers taking more than 60 mins(%)"))) %>% 
  group_by(year) %>% 
  mutate(row_number = row_number()) %>% 
  arrange(row_number) %>% 
  mutate(week_label=factor(week, levels=unique(week[order(row_number)]))) %>% 
  ggplot(.,aes(x=week_label, y=as.numeric(prop), fill=year,  group=year, order=year, colour=year))+
  geom_line()+
  # scale_x_yearmonth(breaks = '1 month',date_labels = "%b %g")+
  # scale_x_date(date_breaks = '1 month', date_labels ="%b %g")
  # scale_x_discrete(labels=unique(plot_data$monthyear))+
  theme_THF()+
  facet_grid(cols=vars(met_lab), scales="free")+
  #scale_fill_manual(values=c('#53a9cd','#dd0031'))+
  #labs(x = "", y="Handovers over 30 mins (%)", caption = "NHS England, Urgent and Emergency Care Daily Situation Reports")+
  theme(legend.text=element_text(size=11),
        legend.title = element_blank(),
        # axis.text.x = element_blank(),
        axis.text.x=element_text(size=11, angle=60),
        axis.text.y=element_text(size=11),
        plot.caption = element_markdown(hjust=0, size=9),
        plot.margin = unit(c(1,1.5,0.5,0.5), "cm"),
        legend.margin=margin(0,0,0,0),
        legend.box.margin=margin(-10,-10,-10,-10))


t<-summamb %>% 
  #rename("Handovers within 30-60 mins(%)"=mean3060,  "Handovers taking more than 60 mins(%)"=mean60plus)
  group_by(year) %>% 
  mutate(row_number = row_number()) %>% 
  arrange(row_number) %>% 
  mutate(week_label=factor(week, levels=unique(week[order(row_number)]))) %>% 
    pivot_wider(id_cols=week_label, names_from=year, values_from=mean30plus)

write.csv(t, "30plus.csv")
