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
                      , object = 'amb_resources.csv' # File to open
                      , bucket = buck) # Bucket name defined above

amb_incidents<-amb_dta %>% 
  select(date, org_name,c1:c4) %>% 
  pivot_longer(c(c1:c4), names_to = 'type', values_to = 'incidents')

type<-c("c1", "c1t", "c2", "c3", "c4")

amb_alloc<-amb_dta %>% 
  select(date, org_name,paste0(type,"_alloc")) %>% 
  pivot_longer(c(c1_alloc:c4_alloc), names_to = 'type', values_to = 'alloc') %>% 
  mutate(type=ifelse(str_detect(type, "t"),substr(type, 0,3), substr(type,0,2)))

amb_arrive<-amb_dta %>% 
  select(date, org_name,paste0(type,"_arrive")) %>% 
  pivot_longer(c(c1_arrive:c4_arrive), names_to = 'type', values_to = 'arrive') %>% 
  mutate(type=ifelse(str_detect(type, "t"),substr(type, 0,3), substr(type,0,2)))


amb_dta_plot<-amb_incidents %>% 
  left_join(amb_alloc, by=c("date", "org_name", "type")) %>% 
  left_join(amb_arrive, by=c("date", "org_name", "type")) %>% 
  mutate(incidents=as.numeric(incidents),
         alloc=as.numeric(alloc),
         arrive=as.numeric(arrive)) %>% 
  mutate(mean_alloc=alloc/incidents) %>% 
  mutate(mean_arrive=arrive/incidents) %>% 
  mutate(date2=yearmonth(date)) %>% 
  mutate(org_lab=factor(org_name, levels=c("England","North East and Yorkshire","North West",
                                           "Midlands","East of England","London","South East","South West")))

amb_dta_plot %>%
  filter(org_lab=="England") %>%
  ggplot(.,aes(x=date2, y=mean_alloc, group=type, colour=type))+
  geom_line(linetype='solid')+
  # geom_point(size=0.25)+
  # geom_hline(yintercept = as_hms("00:07:00"), colour = '#524c48', linetype='dashed' )+
  # geom_hline(yintercept = as_hms("00:15:00"), colour = '#524c48', linetype='dashed')+
  scale_x_yearmonth( breaks = '6 months',date_labels = "%b %g")+
  theme_THF()+
  facet_grid(cols=vars(org_lab))+
  scale_colour_THF()+
  labs(x = "", y="Mean resources allocated", caption = "NHS England, Ambulance Quality Indicators")+
  theme(legend.text=element_text(size=11),
        legend.title = element_blank(),
        axis.text.x=element_text(size=8, angle=60), 
        axis.text.y=element_text(size=11),
        plot.caption = element_markdown(hjust=0, size=9),
        plot.margin = unit(c(1,1.5,0.5,0.5), "cm"),
        legend.margin=margin(0,0,0,0),
        legend.box.margin=margin(-10,-10,-10,-10))

amb_dta_plot %>%
  filter(org_lab=="England") %>%
  pivot_longer(c(mean_alloc, mean_arrive), names_to="metric", values_to="count") %>% 
  ggplot(.,aes(x=date2, y=count, group=type, colour=type))+
  geom_line(linetype='solid')+
  # geom_point(size=0.25)+
  # geom_hline(yintercept = as_hms("00:07:00"), colour = '#524c48', linetype='dashed' )+
  # geom_hline(yintercept = as_hms("00:15:00"), colour = '#524c48', linetype='dashed')+
  scale_x_yearmonth( breaks = '6 months',date_labels = "%b %g")+
  theme_THF()+
  facet_grid(cols=vars(metric))+
  scale_colour_THF()+
  labs(x = "", y="Mean resources arriving on scene", caption = "NHS England, Ambulance Quality Indicators")+
  theme(legend.text=element_text(size=11),
        legend.title = element_blank(),
        axis.text.x=element_text(size=8, angle=60), 
        axis.text.y=element_text(size=11),
        plot.caption = element_markdown(hjust=0, size=9),
        plot.margin = unit(c(1,1.5,0.5,0.5), "cm"),
        legend.margin=margin(0,0,0,0),
        legend.box.margin=margin(-10,-10,-10,-10))
