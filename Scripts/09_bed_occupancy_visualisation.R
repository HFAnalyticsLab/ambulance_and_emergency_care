#Bed occupancy 


# Housekeeping ------------------------------------------------------------

rm(list=ls())

#Library
library(aws.s3)
library(tidyverse)
library(ggplot2)
library(tsibble)



# Load data ---------------------------------------------------------------
buck<-'thf-dap-tier0-projects-iht-067208b7-projectbucket-1mrmynh0q7ljp/ambulance/clean'

England_overnightbeds<-s3read_using(read.csv # Which function are we using to read
                      , object = 'bedoccup_monthly_clean.csv' # File to open
                      , bucket = buck) # Bucket name defined above



# Visualisation -----------------------------------------------------------
#Monthly data 

plot<-England_overnightbeds %>% 
  ggplot(.,aes(x=date2, y=unoccup, group=1))+
  geom_line(linetype='solid', colour='#dd0031')+
  # geom_point(size=0.25)+
  # geom_hline(yintercept = as_hms("00:07:00"), colour = '#524c48', linetype='dashed' )+
  # geom_hline(yintercept = as_hms("00:15:00"), colour = '#524c48', linetype='dashed')+
  scale_x_yearmonth( breaks = '6 months',date_labels = "%b %g")+
  theme_THF()+
  scale_y_continuous(labels= scales::comma )+
  # facet_grid(cols=vars(org_lab))+
  labs(x = "", y="Number of unoccupied beds", caption = "NHS England, Critical care and General & Acute Beds – Urgent and Emergency Care Daily Situation Reports ")+
  theme(legend.text=element_text(size=11),
        legend.title = element_blank(),
        axis.text.x=element_text(size=8, angle=60), 
        axis.text.y=element_text(size=11),
        plot.caption = element_markdown(hjust=0, size=9),
        plot.margin = unit(c(1,1.5,0.5,0.5), "cm"),
        legend.margin=margin(0,0,0,0),
        legend.box.margin=margin(-10,-10,-10,-10))

plot


