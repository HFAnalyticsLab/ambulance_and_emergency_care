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

THF_dark_grey <- '#524c48'
THF_50pct_dark_grey <- '#948E8E' 
THF_light_grey <- '#e2dfd8'
THF_red <- '#dd0031'
THF_50pct_light_blue <- '#53a9cd'

sick_ab %>% 
  mutate(date2=as.Date(date2, format="%Y-%m-%d")) %>% 
  mutate(line_lab=ifelse(date2==max(sick_ab$date2), org_type_new,NA_character_)) %>% 
  filter(date2 %notin% filter_dates) %>%
  arrange(date2) %>%  
  ggplot(., aes(x=date2, y=sa_rate, group=org_type_new, colour=org_type_new))+
  geom_line()+
  scale_x_yearmonth(breaks = '3 months',date_labels = "%b %g")+
  annotate("rect", xmin=as.Date("2020-03-01"), xmax=as.Date("2021-05-01"), 
           ymin=0, ymax=10,fill="grey20", alpha=.1)+
  annotate("richtext",x=as.Date("2020-03-01"), y=9.5, 
           label= "First two waves <br> of COVID-19", size=3, colour="black",hjust=0, fill=NA, label.color=NA)+
  ggrepel::geom_text_repel(aes(label=line_lab),
                           fontface="bold",
                           size = 3,
                           direction = "y",
                           xlim = c(as.Date(max(sick_ab$date2))%m+% months(6), NA),
                           hjust = 0,
                           segment.size = .7,
                           segment.linetype = "solid",
                           segment.alpha = .5,
                           box.padding = .4,
                           segment.curvature = -0.1,
                           segment.ncp = 3,
                           segment.angle = 20)+
  coord_cartesian(
    clip = "off",
    ylim = c(0, 10)
  ) +
  theme_THF()+
  scale_colour_manual(values=c(THF_50pct_dark_grey, THF_red, THF_50pct_light_blue, THF_dark_grey, THF_light_grey))+
  labs(x = "", y="Sickness Absence rate (%)", caption = "NHS Digital, NHS Staff Sickness Absence Rates")+
  theme(legend.text=element_blank(),
        legend.position="none",
        legend.title = element_blank(),
        axis.text.x=element_text(size=8, angle=60), 
        axis.text.y=element_text(size=11),
        plot.caption = element_markdown(hjust=0, size=9),
        plot.margin = unit(c(1,1.5,0.5,0.5), "cm"),
        legend.margin=margin(0,0,0,0),
        legend.box.margin=margin(-10,-10,-10,-10))

