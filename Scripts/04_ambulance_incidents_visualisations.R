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

amb_dta<-s3read_using(read.csv # Which function are we using to read
                      , object = 'amb_incidents.csv' # File to open
                      , bucket = buck) # Bucket name defined above


# Regions-------------------------------------------------------------------------

list_org_codes_region<-c("Y63", "Y62","Y60", "Y61", "Y56", "Y59", "Y58")

amb_dta_plot<-amb_incidents %>% 
  filter(org_code %in% c(list_org_codes_region, "Eng")) %>% 
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
        legend.box.margin=margin(-10,-10,-10,-10)


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
  layout(legend=list(title=list(text=''))) %>% 
  layout(autosize = F, width = 1200, height = 500, margin = m)


