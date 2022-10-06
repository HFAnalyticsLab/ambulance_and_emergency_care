library(data.table)
library(aws.s3)
library(readr)
library(tidyverse)
library(rio)
library(lubridate)
library(ggplot2)
library(hms)
library(tsibble)
library(tidyverse)
library(THFstyle)
library(fpp2)
library(zoo)
library(scales)
library(ggtext)
library(plotly)


rm(list=ls())

# Incidents by region -----------------------------------------------------------------

#Data load

buck<-'thf-dap-tier0-projects-iht-067208b7-projectbucket-1mrmynh0q7ljp/ambulance/clean'

amb_incidents<-s3read_using(read.csv # Which function are we using to read
                      , object = 'amb_incidents.csv' # File to open
                      , bucket = buck) # Bucket name defined above



# Make variables numeric --------------------------------------------------

amb_incidents[7:16] = lapply(amb_incidents[7:16], FUN = function(y){as.numeric(y)})

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



# Incidents by type ----------------------------------------------------


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
  labs(x = "", y="Incidents", caption = "NHS England, Ambulance Quality Indicators")+
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
  # geom_point(size=0.25)+
  # geom_bar(aes(fill=metric),position="fill", stat="identity")+
  scale_x_yearmonth( breaks = '6 months',date_labels = "%b %g")+
  theme_THF()+
  facet_grid(cols=vars(org_lab))+
  scale_colour_THF()+
  labs(x = "", y="Incidents", caption = "NHS England, Ambulance Quality Indicators")+
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
  labs(x = "", y="Incidents", caption = "NHS England, Ambulance Quality Indicators")+
  theme(legend.text=element_text(size=11),
        legend.title = element_blank(),
        axis.text.x=element_text(size=8, angle=60), 
        axis.text.y=element_text(size=11),
        plot.caption = element_markdown(hjust=0, size=9),
        plot.margin = unit(c(1,1.5,0.5,0.5), "cm"),
        legend.margin=margin(0,0,0,0),
        legend.box.margin=margin(-10,-10,-10,-10))



#Calculations for write up ----------------------------------------

pre_dates<-format(as.Date(seq(ymd('2018-04-01'),ymd('2019-03-01'),by='1 month')),"%Y-%m-%d")
post_dates<-format(as.Date(seq(ymd('2021-04-01'),ymd('2022-03-01'),by='1 month')),"%Y-%m-%d")
list_dates<-c(pre_dates, post_dates)


#Categories 
amb_incidents2<-amb_incidents %>% 
  filter(org_code=="Eng") %>% 
  select(c(year:c4,date)) %>% 
  mutate(date=as.Date(date, format="%Y-%m-%d")) %>% 
  mutate(date2=yearmonth(date)) %>% 
  select(all_incidents:date2) %>% 
  mutate(total_cat=c1+c2+c3+c4) %>% 
  mutate(monthyear=format(as.Date(date), "%b %y"))


calcs<-amb_incidents2 %>%
  filter(as.character(date) %in% list_dates) %>%
  mutate(time=case_when(as.character(date)  %in% pre_dates ~ "2018/19",
                        as.character(date) %in% post_dates ~ "2021/22",
                        TRUE~"NA")) %>%
  group_by(time) %>%
  summarise(across(where(is.numeric), ~ sum(.x, na.rm = TRUE))) %>% 
  mutate(c1_prop=(c1/total_cat)*100,
         c2_prop=(c2/total_cat)*100,
         c3_prop=(c3/total_cat)*100,
         c4_prop=(c4/total_cat)*100)


# Data for flourish -------------------------------------------------------



prop_incidents<-amb_incidents2 %>% 
  mutate(c1_prop=(as.numeric(c1)/as.numeric(total_cat)*100),
         c2_prop=(as.numeric(c2)/as.numeric(total_cat)*100),
         c3_prop=(as.numeric(c3)/as.numeric(total_cat)*100),
         c4_prop=(as.numeric(c4)/as.numeric(total_cat)*100))


prop_incidents_v1<-prop_incidents %>% 
  select(date, date2, monthyear, c1_prop:c4_prop)

prop_incidents_v2<-prop_incidents %>% 
  select(date, date2, monthyear, all_incidents)

prop_incidents %>% 
  select(-c(all_incidents, c1t)) %>% 
  pivot_longer(c1:c4, names_to='metric', values_to='val') %>% 
  ggplot(.,aes(x=date2, y=as.numeric(val), colour=metric))+
  geom_area(position=position_fill(reverse=TRUE), fill='white', alpha=0)+
  # # geom_point(size=0.25)+
  # geom_bar(aes(fill=metric),position="fill", stat="identity")+
  scale_x_yearmonth( breaks = '6 months',date_labels = "%b %g")+
  theme_THF()+
  # facet_grid(cols=vars(org_lab))+
  scale_colour_THF()+
  scale_y_continuous(labels = scales::percent)+
  labs(x = "", y="Incidents", caption = "NHS England, Ambulance Quality Indicators")+
  theme(legend.text=element_text(size=11),
        legend.title = element_blank(),
        axis.text.x=element_text(size=8, angle=60), 
        axis.text.y=element_text(size=11),
        plot.caption = element_markdown(hjust=0, size=9),
        plot.margin = unit(c(1,1.5,0.5,0.5), "cm"),
        legend.margin=margin(0,0,0,0),
        legend.box.margin=margin(-10,-10,-10,-10))



coeff=max(prop_incidents$all_incidents)
ggp1 <- ggplot(prop_incidents) +
  geom_bar(aes(date2, all_incidents), color="gray69", stat="identity", fill="gray69", alpha=.4) +
  scale_y_continuous(labels= comma)+
  xlab("") +
  theme(axis.text.x=element_text(angle=90, hjust=1))

ggp1
ggp2 <- ggp1 +
  geom_line(aes(date2, c1_prop*coeff, group=1), color="red", lwd=0.5, linetype='longdash')
ggp3 <- ggp2 +
  scale_y_continuous(name="All incidents", labels=comma, sec.axis=sec_axis(~./coeff, name="Percentage of C1 incidents (%)")) +
  geom_hline(yintercept = coeff*0.1, colour = '#524c48', linetype='solid', alpha=.4)+
  scale_x_yearmonth( breaks = '6 months',date_labels = "%b %g")+
  ggtitle("Incidents") +
  theme_THF()
ggp3

#Types
amb_incidents_type<-amb_incidents %>% 
  filter(org_code=="Eng") %>% 
  select(c(year:all_incidents,hear_treat:date)) %>% 
  mutate(date=as.Date(date, format="%Y-%m-%d")) %>% 
  mutate(date2=yearmonth(date)) %>% 
  select(all_incidents:date2)

prop_incidents_types<-amb_incidents_type %>% 
mutate(total=hear_treat+convey_ED+convey_elsewhere+see_treat) %>% 
  mutate(hear_treat_prop=(hear_treat/total)*100,
         convey_ED_prop=(convey_ED/total)*100,
         convey_elsewhere_prop=(convey_elsewhere/total)*100,
         see_treat_prop=(see_treat/total)*100) %>% 
  mutate(monthyear=format(as.Date(date), "%b %y"))

prop_incidents_v3<-prop_incidents_types %>% 
  mutate(monthyear=format(as.Date(date), "%b %y")) %>% 
  select(date, date2, monthyear, hear_treat_prop:see_treat_prop)

incidents_flourish<-prop_incidents_v2 %>% 
  full_join(prop_incidents_v1) %>% 
  full_join(prop_incidents_types)

write.csv(incidents_flourish, "incidents_flourish_full.csv")



