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


#Data load

buck<-'thf-dap-tier0-projects-iht-067208b7-projectbucket-1mrmynh0q7ljp/ambulance/clean'

wf_eng<-s3read_using(read.csv # Which function are we using to read
                            , object = 'workforce_eng.csv' # File to open
                            , bucket = buck) # Bucket name defined above


wf_eng<-wf_eng %>% 
  mutate(date2=format(as.Date(date), "%Y-%m")) 


list_dates<-format(as.Date(seq(ymd('2017-08-31'),ymd('2022-02-08'),by='4 weeks')),"%Y-%m")

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
