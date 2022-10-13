#Visualising workforce data


# Housekeeping ------------------------------------------------------------

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
library(tsibble)
library(plotly)


# Data load ---------------------------------------------------------------

buck<-'thf-dap-tier0-projects-iht-067208b7-projectbucket-1mrmynh0q7ljp/ambulance/clean'

sick_ab<-s3read_using(read.csv # Which function are we using to read
                      , object = 'sick_ab_clean.csv' # File to open
                      , bucket = buck) # Bucket name defined above


# Format data ----------------------------------------------

list_dates<-format(as.Date(seq(ymd('2017-09-01'),ymd('2022-07-01'),by='months')),"%Y-%m")


flourish_sick_ab<-sick_ab %>% 
  mutate(filter_date=format(as.Date(date2), "%Y-%m"))%>% 
  filter(filter_date %in% list_dates) %>%
  arrange(filter_date) %>% 
  select(-c(X, fte_days_sick, fte_days_available)) %>% 
  pivot_wider(., names_from=org_type_new, values_from=sa_rate) %>% 
  mutate(monthyear=format(as.Date(date2), "%b %y")) 
  
write.csv(flourish_sick_ab, 'flourish_staff_sick_ab.csv')


sick_ab %>% 
  mutate(filter_date=format(as.Date(date2), "%Y-%m"))%>% 
  filter(filter_date %in% list_dates) %>%
  arrange(filter_date) %>% 
  ggplot(., aes(x=date2, y=sa_rate, group=org_type, colour=org_type))+
  geom_point()+
  geom_line()+
  scale_x_yearmonth( breaks = '3 months',date_labels = "%b %g")+
  theme_THF()+
  labs(x = "", y="Sickness and Absence rate (%)", caption = "NHS Digital-workforce statistics")+
  theme(legend.text=element_text(size=11),
        legend.title = element_blank(),
        axis.text.x=element_text(size=8, angle=60), 
        axis.text.y=element_text(size=11),
        plot.caption = element_markdown(hjust=0, size=9),
        plot.margin = unit(c(1,1.5,0.5,0.5), "cm"),
        legend.margin=margin(0,0,0,0),
        legend.box.margin=margin(-10,-10,-10,-10))
