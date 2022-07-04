library(data.table)
library(aws.s3)
library(readr)
library(tidyverse)
library(rio)
library(lubridate)
library(ggplot2)
library(hms)
library(tidyverse)
library(THFstyle)
library(tsibble)
library(ggtext)

rm(list=ls())

# Data load -----------------------------------------------------------------


buck<-'thf-dap-tier0-projects-iht-067208b7-projectbucket-1mrmynh0q7ljp/ambulance/clean'


amb_dta<-s3read_using(read.csv # Which function are we using to read
                      , object = 'amb_calls.csv' # File to open
                      , bucket = buck) # Bucket name defined above

amb_dta_plot<-amb_dta %>% 
  pivot_longer(c(answered_times_mean:answered_times_90), names_to = 'metric', values_to = 'answer_time')

amb_dta_plot<-amb_dta_plot %>% 
  mutate(answer_time2=as.POSIXct(as.numeric(answer_time),origin = "1970-01-01", tz="GMT")) %>% 
  mutate(answer_time2=format(answer_time2, format="%H:%M:%S")) %>% 
  mutate(answer_time2=as_hms(answer_time2)) %>% 
  mutate(date2=yearmonth(date)) %>% 
  mutate(org_lab=factor(org_name, levels=c("England","North East and Yorkshire","North West",
                                           "Midlands","East of England","London","South East","South West"))) %>% 
  mutate(contact_count=as.numeric(contact_count)) %>% 
  mutate(calls_answered=as.numeric(calls_answered)) %>% 
  select(c(contact_count:calls_answered,date:org_lab))

##so demand is going up in recent months for the whole system
amb_dta_plot %>%
  filter(org_lab=="England") %>%
  ggplot(.,aes(x=date2, y=answer_time2, group=metric, colour=metric))+
  geom_line(linetype='solid')+
  # geom_point(size=0.25)+
  # geom_hline(yintercept = as_hms("00:07:00"), colour = '#524c48', linetype='dashed' )+
  # geom_hline(yintercept = as_hms("00:15:00"), colour = '#524c48', linetype='dashed')+
  scale_x_yearmonth( breaks = '6 months',date_labels = "%b %g")+
  theme_THF()+
  facet_grid(cols=vars(org_lab))+
  scale_colour_THF()+
  labs(x = "", y="Call answer times (seconds)", caption = "NHS England, Ambulance Quality Indicators")+
  theme(legend.text=element_text(size=11),
        legend.title = element_blank(),
        axis.text.x=element_text(size=8, angle=60), 
        axis.text.y=element_text(size=11),
        plot.caption = element_markdown(hjust=0, size=9),
        plot.margin = unit(c(1,1.5,0.5,0.5), "cm"),
        legend.margin=margin(0,0,0,0),
        legend.box.margin=margin(-10,-10,-10,-10))

##so demand is going up in recent months for the whole system
amb_dta_plot %>%
  filter(org_lab=="England") %>%
  ggplot(.,aes(x=date2,))+
  geom_line(aes(y=contact_count),linetype='solid', colour="red")+
  geom_line(aes(y=calls_answered),linetype='solid', colour="blue")+
  # geom_point(size=0.25)+
  # geom_hline(yintercept = as_hms("00:07:00"), colour = '#524c48', linetype='dashed' )+
  # geom_hline(yintercept = as_hms("00:15:00"), colour = '#524c48', linetype='dashed')+
  scale_x_yearmonth( breaks = '6 months',date_labels = "%b %g")+
  theme_THF()+
  facet_grid(cols=vars(org_lab))+
  scale_colour_THF()+
  labs(x = "", y="Counts", caption = "NHS England, Ambulance Quality Indicators")+
  theme(legend.text=element_text(size=11),
        legend.title = element_blank(),
        axis.text.x=element_text(size=8, angle=60), 
        axis.text.y=element_text(size=11),
        plot.caption = element_markdown(hjust=0, size=9),
        plot.margin = unit(c(1,1.5,0.5,0.5), "cm"),
        legend.margin=margin(0,0,0,0),
        legend.box.margin=margin(-10,-10,-10,-10))



