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
library(plotly)

#Function
#Does not contain to be used for filter 
`%notin%` <- Negate(`%in%`)
# Data load ---------------------------------------------------------------

buck<-'thf-dap-tier0-projects-iht-067208b7-projectbucket-1mrmynh0q7ljp/ambulance/clean'

sick_ab<-s3read_using(read.csv # Which function are we using to read
                      , object = 'sick_ab_clean.csv' # File to open
                      , bucket = buck) # Bucket name defined above


# Format data ----------------------------------------------

filter_dates<-as.Date(seq(ymd('2009-04-01'),ymd('2017-08-01'),by='months'), format="%Y-%m-%d")


flourish_sick_ab<-sick_ab %>% 
  mutate(date2=as.Date(date2, format="%Y-%m-%d")) %>% 
  filter(date2 %notin% filter_dates) %>%
  arrange(date2) %>% 
  select(-c(X, fte_days_sick, fte_days_available)) %>% 
  pivot_wider(., names_from=org_type_new, values_from=sa_rate) %>% 
  mutate(monthyear=format(as.Date(date2), "%b %y")) 
  
write.csv(flourish_sick_ab, 'flourish_staff_sick_ab.csv')

THF_light_grey <- '#e2dfd8'
THF_50pct_dark_grey <- '#948E8E' 
THF_50pct_light_grey <- '#EEEDE8'
THF_red <- '#dd0031'
THF_50pct_light_blue <- '#53a9cd'

sick_ab %>% 
  mutate(date2=as.Date(date2, format="%Y-%m-%d")) %>% 
  filter(date2 %notin% filter_dates) %>%
  arrange(date2) %>%  
  ggplot(., aes(x=date2, y=sa_rate, group=org_type_new, colour=org_type_new))+
  geom_line()+
  scale_x_yearmonth( breaks = '3 months',date_labels = "%b %g")+
  theme_THF()+
  scale_colour_manual(values=c(THF_50pct_dark_grey, THF_red, THF_50pct_light_blue, THF_light_grey, THF_50pct_light_grey))+
  labs(x = "", y="Sickness and Absence rate (%)", caption = "NHS Digital-workforce statistics")+
  theme(legend.text=element_text(size=11),
        legend.title = element_blank(),
        axis.text.x=element_text(size=8, angle=60), 
        axis.text.y=element_text(size=11),
        plot.caption = element_markdown(hjust=0, size=9),
        plot.margin = unit(c(1,1.5,0.5,0.5), "cm"),
        legend.margin=margin(0,0,0,0),
        legend.box.margin=margin(-10,-10,-10,-10))
#Need to find a way to annotate the data
