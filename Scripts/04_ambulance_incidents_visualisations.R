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


rm(list=ls())

# Incidents by region -----------------------------------------------------------------

#Data load

buck<-'thf-dap-tier0-projects-iht-067208b7-projectbucket-1mrmynh0q7ljp/ambulance/clean'

amb_incidents<-s3read_using(read.csv # Which function are we using to read
                      , object = 'amb_incidents.csv' # File to open
                      , bucket = buck) # Bucket name defined above


# Regions-------------------------------------------------------------------------

list_org_codes_region<-c("Y63", "Y62","Y60", "Y61", "Y56", "Y59", "Y58")

amb_dta_plot<-amb_incidents %>% 
  filter(org_code %in% c(list_org_codes_region, "Eng")) %>% 
  select(c(year:c4,date)) %>% 
  pivot_longer(c(all_incidents:c4), names_to = 'metric', values_to = 'incidents')

amb_dta_plot<-amb_dta_plot %>% 
  mutate(date2=yearmonth(date)) %>% 
  mutate(org_lab=factor(org_name, levels=c("England","North East and Yorkshire","North West",
                                           "Midlands","East of England","London","South East","South West")))



amb_dta_plot %>%
  filter(org_code != "Eng" & metric != "all_incidents") %>% 
  ggplot(.,aes(x=date2, y=as.numeric(incidents), group=metric, colour=metric))+
  geom_line(linetype='solid')+
  # geom_point(size=0.25)+
  # geom_bar(aes(fill=metric),position="stack", stat="identity")+
  scale_x_yearmonth( breaks = '6 months',date_labels = "%b %g")+
  theme_THF()+
  facet_grid(cols=vars(org_lab))+
  scale_colour_THF()+
  labs(x = "", y="Response time (minutes)", caption = "NHS England, Ambulance Quality Indicators")+
  theme(legend.text=element_text(size=11),
        legend.title = element_blank(),
        axis.text.x=element_text(size=8, angle=60), 
        axis.text.y=element_text(size=11),
        plot.caption = element_markdown(hjust=0, size=9),
        plot.margin = unit(c(1,1.5,0.5,0.5), "cm"),
        legend.margin=margin(0,0,0,0),
        legend.box.margin=margin(-10,-10,-10,-10))


plot<-amb_dta_plot %>%
  filter(org_code=="Eng" & metric=="all_incidents")%>% 
  ggplot(.,aes(x=date2, y=as.numeric(incidents), group=metric, colour=metric))+
  geom_line(linetype='solid')+
  # geom_point(size=0.25)+
  # geom_bar(aes(fill=metric),position="stack", stat="identity")+
  scale_x_yearmonth( breaks = '6 months',date_labels = "%b %g")+
  theme_THF()+
  # facet_grid(cols=vars(org_lab))+
  scale_colour_THF()+
  labs(x = "", y="Response time (minutes)", caption = "NHS England, Ambulance Quality Indicators")+
  theme(legend.text=element_text(size=11),
        legend.title = element_blank(),
        axis.text.x=element_text(size=8, angle=60), 
        axis.text.y=element_text(size=11),
        plot.caption = element_markdown(hjust=0, size=9),
        plot.margin = unit(c(1,1.5,0.5,0.5), "cm"),
        legend.margin=margin(0,0,0,0),
        legend.box.margin=margin(-10,-10,-10,-10))

ggplotly(plot) %>% 
  layout(legend = list(orientation = 'v',valign="top", font=list(size=8))) %>% 
  layout(legend=list(title=list(text=''))) 


# Trusts ------------------------------------------------------------------

list_org_codes_trust<-c("RX9", "RYC", "R1F", "RRU", "RX6", "RX7", "RYE", "RYD", "RYF", "RYA", "RX8") 

amb_dta_plot<-amb_incidents %>% 
  filter(org_code %in% c(list_org_codes_trust, "Eng")) %>% 
  select(c(year:c4, date)) %>% 
  pivot_longer(c(all_incidents:c4), names_to = 'metric', values_to = 'incidents')

amb_dta_plot<-amb_dta_plot %>% 
  mutate(date2=yearmonth(date)) %>% 
  mutate(incidents=as.numeric(incidents)) %>% 
  filter(org_name != "WEST MIDLANDS AMBULANCE SERVICE NHS FOUNDATION TRUST")


long_names<-c("England", "EAST MIDLANDS AMBULANCE SERVICE NHS TRUST", "EAST OF ENGLAND AMBULANCE SERVICE NHS TRUST" ,"ISLE OF WIGHT NHS TRUST",                                        
              "LONDON AMBULANCE SERVICE NHS TRUST", "NORTH EAST AMBULANCE SERVICE NHS FOUNDATION TRUST", "NORTH WEST AMBULANCE SERVICE NHS TRUST",                         
              "SOUTH CENTRAL AMBULANCE SERVICE NHS FOUNDATION TRUST", "SOUTH EAST COAST AMBULANCE SERVICE NHS FOUNDATION TRUST", "SOUTH WESTERN AMBULANCE SERVICE NHS FOUNDATION TRUST",
              "WEST MIDLANDS AMBULANCE SERVICE UNIVERSITY NHS FOUNDATION TRUST", "YORKSHIRE AMBULANCE SERVICE NHS TRUST") 


label_names<-c("England", "East Midlands", "East of England" ,"Isle of Wight",                                        
               "London", "North East", "North West",                         
               "South Central", "South East Coast", "South Western",
               "West Midlands", "Yorkshire") 

amb_dta_plot<-amb_dta_plot %>% 
  mutate(org_lab=factor(org_name, levels=long_names, labels=label_names)) 


amb_dta_plot %>%
  filter(org_code!="Eng" & metric=="all_incidents")%>% 
  ggplot(.,aes(x=date2, y=as.numeric(incidents), group=metric, colour=metric))+
  geom_line(linetype='solid')+
  # geom_point(size=0.25)+
  # geom_bar(aes(fill=metric),position="stack", stat="identity")+
  scale_x_yearmonth( breaks = '6 months',date_labels = "%b %g")+
  theme_THF()+
  facet_grid(cols=vars(org_lab))+
  scale_colour_THF()+
  labs(x = "", y="Response time (minutes)", caption = "NHS England, Ambulance Quality Indicators")+
  theme(legend.text=element_text(size=11),
        legend.title = element_blank(),
        axis.text.x=element_text(size=8, angle=60), 
        axis.text.y=element_text(size=11),
        plot.caption = element_markdown(hjust=0, size=9),
        plot.margin = unit(c(1,1.5,0.5,0.5), "cm"),
        legend.margin=margin(0,0,0,0),
        legend.box.margin=margin(-10,-10,-10,-10))


amb_dta_plot %>%
  filter(org_code!="Eng" & metric!="all_incidents")%>% 
  ggplot(.,aes(x=date2, y=as.numeric(incidents), group=metric, colour=metric))+
  geom_line(linetype='solid')+
  # geom_point(size=0.25)+
  # geom_bar(aes(fill=metric),position="stack", stat="identity")+
  scale_x_yearmonth( breaks = '6 months',date_labels = "%b %g")+
  theme_THF()+
  facet_grid(cols=vars(org_lab))+
  scale_colour_THF()+
  labs(x = "", y="Response time (minutes)", caption = "NHS England, Ambulance Quality Indicators")+
  theme(legend.text=element_text(size=11),
        legend.title = element_blank(),
        axis.text.x=element_text(size=8, angle=60), 
        axis.text.y=element_text(size=11),
        plot.caption = element_markdown(hjust=0, size=9),
        plot.margin = unit(c(1,1.5,0.5,0.5), "cm"),
        legend.margin=margin(0,0,0,0),
        legend.box.margin=margin(-10,-10,-10,-10))



# Incidents by outcome ----------------------------------------------------


# Regions -----------------------------------------------------------------
list_org_codes_region<-c("Y63", "Y62","Y60", "Y61", "Y56", "Y59", "Y58")

amb_dta_regions<-amb_incidents %>% 
  filter(org_code %in% c(list_org_codes_region, "Eng")) %>% 
  select(c(year:all_incidents,hear_treat:date)) %>% 
  pivot_longer(c(all_incidents:see_treat), names_to = 'metric', values_to = 'incidents')

amb_dta_regions<-amb_dta_regions %>% 
  mutate(date2=yearmonth(date)) %>% 
  mutate(incidents=as.numeric(incidents)) %>% 
  mutate(org_lab=factor(org_name, levels=c("England","North East and Yorkshire","North West",
                                           "Midlands","East of England","London","South East","South West")))


amb_dta_regions %>%
  filter(org_code=="Eng" & metric!="all_incidents")%>% 
  ggplot(.,aes(x=date2, y=as.numeric(incidents), group=metric, colour=metric))+
  geom_line(linetype='solid')+
  # # geom_point(size=0.25)+
  # geom_bar(aes(fill=metric),position="fill", stat="identity")+
  scale_x_yearmonth( breaks = '6 months',date_labels = "%b %g")+
  theme_THF()+
  facet_grid(cols=vars(org_lab))+
  scale_colour_THF()+
  labs(x = "", y="Response time (minutes)", caption = "NHS England, Ambulance Quality Indicators")+
  theme(legend.text=element_text(size=11),
        legend.title = element_blank(),
        axis.text.x=element_text(size=8, angle=60), 
        axis.text.y=element_text(size=11),
        plot.caption = element_markdown(hjust=0, size=9),
        plot.margin = unit(c(1,1.5,0.5,0.5), "cm"),
        legend.margin=margin(0,0,0,0),
        legend.box.margin=margin(-10,-10,-10,-10))


amb_dta_regions %>%
  filter(org_code!="Eng" & metric!="all_incidents")%>% 
  ggplot(.,aes(x=date2, y=as.numeric(incidents), group=metric, colour=metric))+
  geom_line(linetype='solid')+
  # # geom_point(size=0.25)+
  # geom_bar(aes(fill=metric),position="fill", stat="identity")+
  scale_x_yearmonth( breaks = '6 months',date_labels = "%b %g")+
  theme_THF()+
  facet_grid(cols=vars(org_lab))+
  scale_colour_THF()+
  labs(x = "", y="Response time (minutes)", caption = "NHS England, Ambulance Quality Indicators")+
  theme(legend.text=element_text(size=11),
        legend.title = element_blank(),
        axis.text.x=element_text(size=8, angle=60), 
        axis.text.y=element_text(size=11),
        plot.caption = element_markdown(hjust=0, size=9),
        plot.margin = unit(c(1,1.5,0.5,0.5), "cm"),
        legend.margin=margin(0,0,0,0),
        legend.box.margin=margin(-10,-10,-10,-10))


# Trusts ------------------------------------------------------------------

list_org_codes_trust<-c("RX9", "RYC", "R1F", "RRU", "RX6", "RX7", "RYE", "RYD", "RYF", "RYA", "RX8") 

long_names<-c("England", "EAST MIDLANDS AMBULANCE SERVICE NHS TRUST", "EAST OF ENGLAND AMBULANCE SERVICE NHS TRUST" ,"ISLE OF WIGHT NHS TRUST",                                        
              "LONDON AMBULANCE SERVICE NHS TRUST", "NORTH EAST AMBULANCE SERVICE NHS FOUNDATION TRUST", "NORTH WEST AMBULANCE SERVICE NHS TRUST",                         
              "SOUTH CENTRAL AMBULANCE SERVICE NHS FOUNDATION TRUST", "SOUTH EAST COAST AMBULANCE SERVICE NHS FOUNDATION TRUST", "SOUTH WESTERN AMBULANCE SERVICE NHS FOUNDATION TRUST",
              "WEST MIDLANDS AMBULANCE SERVICE UNIVERSITY NHS FOUNDATION TRUST", "YORKSHIRE AMBULANCE SERVICE NHS TRUST") 


label_names<-c("England", "East Midlands", "East of England" ,"Isle of Wight",                                        
               "London", "North East", "North West",                         
               "South Central", "South East Coast", "South Western",
               "West Midlands", "Yorkshire") 



amb_dta_incidents_trusts<-amb_incidents %>% 
  filter(org_code %in% c(list_org_codes_trust, "Eng")) %>% 
  select(c(year:all_incidents,hear_treat:date)) %>% 
  pivot_longer(c(all_incidents:see_treat), names_to = 'metric', values_to = 'incidents')

amb_dta_incidents_trusts<-amb_dta_incidents_trusts %>% 
  mutate(date2=yearmonth(date)) %>% 
  mutate(incidents=as.numeric(incidents)) %>% 
  filter(org_name != "WEST MIDLANDS AMBULANCE SERVICE NHS FOUNDATION TRUST") %>% 
  mutate(org_lab=factor(org_name, levels=long_names, labels=label_names)) 


amb_dta_incidents_trusts %>%
  filter(org_code!="Eng" & metric!="all_incidents")%>% 
  ggplot(.,aes(x=date2, y=as.numeric(incidents), group=metric, colour=metric))+
  geom_line(linetype='solid')+
  # # geom_point(size=0.25)+
  # geom_bar(aes(fill=metric),position="fill", stat="identity")+
  scale_x_yearmonth( breaks = '6 months',date_labels = "%b %g")+
  theme_THF()+
  facet_grid(cols=vars(org_lab))+
  scale_colour_THF()+
  labs(x = "", y="Response time (minutes)", caption = "NHS England, Ambulance Quality Indicators")+
  theme(legend.text=element_text(size=11),
        legend.title = element_blank(),
        axis.text.x=element_text(size=8, angle=60), 
        axis.text.y=element_text(size=11),
        plot.caption = element_markdown(hjust=0, size=9),
        plot.margin = unit(c(1,1.5,0.5,0.5), "cm"),
        legend.margin=margin(0,0,0,0),
        legend.box.margin=margin(-10,-10,-10,-10))

