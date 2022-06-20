#Visualising workforce data

rm(list=ls())

#Library
library(data.table)
library(aws.s3)
library(readr)
library(tidyverse)
library(ggplot2)
library(THFstyle)
library(ggtext)
library(lubridate)


#Data load

buck<-'thf-dap-tier0-projects-iht-067208b7-projectbucket-1mrmynh0q7ljp/ambulance/clean'

wf_eng<-s3read_using(read.csv # Which function are we using to read
                            , object = 'workforce_eng.csv' # File to open
                            , bucket = buck) # Bucket name defined above


turnover<-s3read_using(read.csv # Which function are we using to read
                       , object = 'turnover_clean.csv' # File to open
                       , bucket = buck) # Bucket name defined above

sick_ab<-s3read_using(read.csv # Which function are we using to read
                      , object = 'sick_ab_clean.csv' # File to open
                      , bucket = buck) # Bucket name defined above



# Counts --------------------------------------------------------------

list_dates<-format(as.Date(seq(ymd('2017-08-31'),ymd('2022-02-08'),by='4 weeks')),"%Y-%m")



wf_eng<-wf_eng %>% 
  mutate(date2=format(as.Date(date), "%Y-%m")) 



wf_eng %>%
  filter(group=="England" & met=="headcount") %>% 
  filter(date2 %in% list_dates) %>% 
  ggplot(.,aes(x=date, y=val, group=group))+
  geom_point()+
  geom_line(linetype='solid')+
  scale_x_yearmonth( breaks = 'months',date_labels = "%b %g")+
  theme_THF()+
  labs(x = "", y="Headcount", caption = "NHS Digital-workforce statistics")+
  theme(legend.text=element_text(size=11),
        legend.title = element_blank(),
        axis.text.x=element_text(size=8, angle=60), 
        axis.text.y=element_text(size=11),
        plot.caption = element_markdown(hjust=0, size=9),
        plot.margin = unit(c(1,1.5,0.5,0.5), "cm"),
        legend.margin=margin(0,0,0,0),
        legend.box.margin=margin(-10,-10,-10,-10))

t<-wf_eng %>%
  filter(group=="Ambulance staff" & met=="headcount") %>% 
  filter(date2 %in% list_dates) %>%
  ggplot(.,aes(x=date, y=val, group=group))+
  geom_point()+
  geom_line(linetype='solid')+
  scale_x_yearmonth( breaks = 'months',date_labels = "%b %g")+
  theme_THF()+
  labs(x = "", y="Headcount", caption = "NHS Digital-workforce statistics")+
  theme(legend.text=element_text(size=11),
        legend.title = element_blank(),
        axis.text.x=element_text(size=8, angle=60), 
        axis.text.y=element_text(size=11),
        plot.caption = element_markdown(hjust=0, size=9),
        plot.margin = unit(c(1,1.5,0.5,0.5), "cm"),
        legend.margin=margin(0,0,0,0),
        legend.box.margin=margin(-10,-10,-10,-10))

ggplotly(t)


wf_eng %>%
  filter(group=="England" & met=="fte_count") %>% 
  mutate(date2=format(as.Date(date), "%Y-%m")) %>% 
  arrange(date2) %>% 
  ggplot(.,aes(x=date, y=val, group=group))+
  geom_point()+
  geom_line(linetype='solid')+
  scale_x_yearmonth( breaks = '6 months',date_labels = "%b %g")+
  theme_THF()+
  labs(x = "", y="FTE count", caption = "NHS Digital-workforce statistics")+
  theme(legend.text=element_text(size=11),
        legend.title = element_blank(),
        axis.text.x=element_text(size=8, angle=60), 
        axis.text.y=element_text(size=11),
        plot.caption = element_markdown(hjust=0, size=9),
        plot.margin = unit(c(1,1.5,0.5,0.5), "cm"),
        legend.margin=margin(0,0,0,0),
        legend.box.margin=margin(-10,-10,-10,-10))




wf_eng %>%
  filter(group=="Ambulance staff" & met=="fte_count") %>% 
  mutate(date2=format(as.Date(date), "%Y-%m")) %>% 
  arrange(date2) %>% 
  ggplot(.,aes(x=date, y=val, group=group))+
  geom_point()+
  geom_line(linetype='solid')+
  scale_x_yearmonth( breaks = '6 months',date_labels = "%b %g")+
  theme_THF()+
  labs(x = "", y="FTE count", caption = "NHS Digital-workforce statistics")+
  theme(legend.text=element_text(size=11),
        legend.title = element_blank(),
        axis.text.x=element_text(size=8, angle=60), 
        axis.text.y=element_text(size=11),
        plot.caption = element_markdown(hjust=0, size=9),
        plot.margin = unit(c(1,1.5,0.5,0.5), "cm"),
        legend.margin=margin(0,0,0,0),
        legend.box.margin=margin(-10,-10,-10,-10))

ggplotly(t)



# Turnover rates ----------------------------------------------------------

#plotting year end to make it simpler 

turnover<-turnover %>% 
  mutate(date2=paste0(year_end,"-",ifelse(period==12,period,paste0(0,period)),"-",ifelse(period %in% c(9,6),30,31))) %>% 
  mutate(date=as.Date(date2, origin = "1899-12-30")) %>% 
  mutate(date=lubridate::date(date))         



list_dates<-format(as.Date(seq(ymd('2017-09-30'),ymd('2021-12-31'),by='3 months')),"%Y-%b")
           
turnover %>%
  mutate(filter_date=format(as.Date(date), "%Y-%b"))%>% 
  filter(filter_date %in% list_dates) %>% 
  ggplot(., aes(x=date, y=stability_index*100, group=staff_group, colour=staff_group))+
  geom_line(linetype='solid')+
  scale_x_yearmonth( breaks = '3 months',date_labels = "%b %g")+
  theme_THF()+
  labs(x = "Year end", y="Stability Index (%)", caption = "NHS Digital-workforce statistics")+
  theme(legend.text=element_text(size=11),
        legend.title = element_blank(),
        axis.text.x=element_text(size=8, angle=60), 
        axis.text.y=element_text(size=11),
        plot.caption = element_markdown(hjust=0, size=9),
        plot.margin = unit(c(1,1.5,0.5,0.5), "cm"),
        legend.margin=margin(0,0,0,0),
        legend.box.margin=margin(-10,-10,-10,-10))




# Staff sickness and absence ----------------------------------------------

list_dates<-format(as.Date(seq(ymd('2017-09-01'),ymd('2022-04-01'),by='months')),"%Y-%m")

sick_ab %>% 
  mutate(filter_date=format(as.Date(date2), "%Y-%m"))%>% 
  filter(filter_date %in% list_dates) %>%
  arrange(filter_date) %>% 
  ggplot(., aes(x=date2, y=sa_rate, group=1))+
  geom_point()+
  geom_line()+
  scale_x_yearmonth( breaks = '3 months',date_labels = "%b %g")+
  theme_THF()+
  labs(x = "Year end", y="Sickness and Absence rate (%)", caption = "NHS Digital-workforce statistics")+
  theme(legend.text=element_text(size=11),
        legend.title = element_blank(),
        axis.text.x=element_text(size=8, angle=60), 
        axis.text.y=element_text(size=11),
        plot.caption = element_markdown(hjust=0, size=9),
        plot.margin = unit(c(1,1.5,0.5,0.5), "cm"),
        legend.margin=margin(0,0,0,0),
        legend.box.margin=margin(-10,-10,-10,-10))
