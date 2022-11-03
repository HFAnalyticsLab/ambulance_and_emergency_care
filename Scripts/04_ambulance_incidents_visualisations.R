#Number of Incidents 


# Housekeeping ------------------------------------------------------------

rm(list=ls())

#Library
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
library(fpp2)
library(zoo)
library(ggtext)
library(tsibble)

#Functions

#Does not contain to be used for filter 
`%notin%` <- Negate(`%in%`)


# Data load ---------------------------------------------------------------

buck<-'thf-dap-tier0-projects-iht-067208b7-projectbucket-1mrmynh0q7ljp/ambulance/clean'

amb_incidents<-s3read_using(read.csv # Which function are we using to read
                      , object = 'amb_incidents.csv' # File to open
                      , bucket = buck) # Bucket name defined above

# Formatting for charts --------------------------------------------------

amb_incidents[7:16] = lapply(amb_incidents[7:16], FUN = function(y){as.numeric(y)})

#Visualising incidents  -------------------------------------------------------

#Number of incidents, figures 3a 
amb_incidents_type<-amb_incidents %>% 
  filter(org_code=="Eng") %>% 
  select(c(year:all_incidents,hear_treat:date)) %>% 
  mutate(date=as.Date(date, format="%Y-%m-%d")) %>% 
  select(all_incidents:date)

filter_dates<-as.Date(seq(ymd('2017-08-01'),ymd('2018-03-01'),by='1 month'), format="%Y-%m-%d")

amb_incidents_type<-amb_incidents_type %>% 
  dplyr::filter(date %notin% filter_dates)


plot<-amb_incidents_type %>% 
  select(-c(all_incidents)) %>% 
  pivot_longer(hear_treat:see_treat, names_to='metric', values_to='val') %>% 
  mutate(metric=factor(metric, levels=c("hear_treat","convey_ED","convey_elsewhere","see_treat"),
                       labels=c("Hear and treat","Convey to emergency department",
                                "Convey elsewhere", "See and treat"))) %>% 
  ggplot(.,aes(x=date, y=as.numeric(val), fill=metric, order=val))+
  geom_area(position=position_stack(reverse=TRUE), alpha=0.6)+
  scale_x_yearmonth( breaks = '6 months',date_labels = "%b %y")+
  annotate("rect", xmin=as.Date("2020-03-01"), xmax=as.Date("2021-05-01"), 
           ymin=0, ymax=900000,fill="grey20", alpha=.1)+
  annotate("richtext",x=as.Date("2020-03-01"), y=850000, 
           label= "First two waves <br> of COVID-19", size=3, colour="black",hjust=0, fill=NA, label.color=NA)+
  theme_THF()+
  # facet_grid(cols=vars(org_lab))+
  scale_fill_THF()+
  scale_y_continuous(labels= scales::comma, limits=c(0,900000), breaks=seq(0,900000, by=100000))+
  labs(x = "", y="Number of incidents", caption = "NHS England, Ambulance Quality Indicators")+
  theme(legend.text=element_text(size=11),
        legend.title = element_blank(),
        axis.text.x=element_text(size=11, angle=60), 
        axis.text.y=element_text(size=11),
        plot.caption = element_markdown(hjust=0, size=9),
        plot.margin = unit(c(1,1.5,0.5,0.5), "cm"),
        legend.margin=margin(0,0,0,0),
        legend.box.margin=margin(-10,-10,-10,-10))

plot

ggsave('plot_incidents_by_type.png', dpi=300, height=6.5, width=10)

#Proportion of category 1-4 incidents, figure 3b

amb_incidents2<-amb_incidents %>% 
  filter(org_code=="Eng") %>% 
  select(c(year:c4,date)) %>% 
  mutate(date=as.Date(date, format="%Y-%m-%d")) %>% 
  select(all_incidents:date) %>% 
  mutate(total_cat=c1+c2+c3+c4) %>% 
  mutate(monthyear=format(as.Date(date), "%b %y")) %>% 
  dplyr::filter(date %notin% filter_dates)




prop_incidents<-amb_incidents2 %>% 
  mutate(c1_prop=(as.numeric(c1)/as.numeric(total_cat)*100),
         c2_prop=(as.numeric(c2)/as.numeric(total_cat)*100),
         c3_prop=(as.numeric(c3)/as.numeric(total_cat)*100),
         c4_prop=(as.numeric(c4)/as.numeric(total_cat)*100))


prop_incidents_v1<-prop_incidents %>% 
  select(date, monthyear, c1_prop:c4_prop)

prop_incidents_v2<-prop_incidents %>% 
  select(date, monthyear, all_incidents)



plot<-prop_incidents %>% 
  select(-c(all_incidents, c1t)) %>% 
  pivot_longer(c1_prop:c4_prop, names_to='metric', values_to='val') %>% 
  mutate(met_lab=factor(metric, levels=c("c1_prop", "c2_prop", "c3_prop", "c4_prop"),
                           labels=c("Category 1", "Category 2", "Category 3", "Category 4"))) %>% 
  ggplot(.,aes(x=date, y=as.numeric(val), fill=met_lab))+
  geom_area(position=position_fill(reverse=TRUE), alpha=.7)+
  # # geom_point(size=0.25)+
  # geom_bar(aes(fill=metric),position="fill", stat="identity")+
  scale_x_yearmonth( breaks = '6 months',date_labels = "%b %y")+
  theme_THF()+
  # facet_grid(cols=vars(org_lab))+
  scale_fill_THF()+
  scale_y_continuous(labels = scales::percent)+
  labs(x = "", y="Proportion of incidents", caption = "NHS England, Ambulance Quality Indicators")+
  theme(legend.text=element_text(size=11),
        legend.title = element_blank(),
        axis.text.x=element_text(size=8, angle=60), 
        axis.text.y=element_text(size=11),
        plot.caption = element_markdown(hjust=0, size=9),
        plot.margin = unit(c(1,1.5,0.5,0.5), "cm"),
        legend.margin=margin(0,0,0,0),
        legend.box.margin=margin(-10,-10,-10,-10))

plot

ggsave('plot_incidents_by_cat.png', dpi=300, height=6.5, width=10)

# Combining counts and proportions -------------------------------------------------------

prop_incidents_types<-amb_incidents_type %>% 
mutate(total=hear_treat+convey_ED+convey_elsewhere+see_treat) %>% 
  mutate(hear_treat_prop=(hear_treat/total)*100,
         convey_ED_prop=(convey_ED/total)*100,
         convey_elsewhere_prop=(convey_elsewhere/total)*100,
         see_treat_prop=(see_treat/total)*100) %>% 
  mutate(monthyear=format(as.Date(date), "%b %y"))

prop_incidents_v3<-prop_incidents_types %>% 
  mutate(monthyear=format(as.Date(date), "%b %y")) %>% 
  select(date, monthyear, hear_treat_prop:see_treat_prop)

incidents_flourish<-prop_incidents_v2 %>% 
  full_join(prop_incidents_v1) %>% 
  full_join(prop_incidents_types) %>% 
  mutate(monthyear=format(as.Date(date), "%b %y"))

write.csv(incidents_flourish, "incidents_flourish_full.csv")

#Calculations for proportion of c1-c4 incidents ----------------------------------------

pre_dates<-format(as.Date(seq(ymd('2018-04-01'),ymd('2019-03-01'),by='1 month')),"%Y-%m-%d")
post_dates<-format(as.Date(seq(ymd('2021-04-01'),ymd('2022-03-01'),by='1 month')),"%Y-%m-%d")
list_dates<-c(pre_dates, post_dates)

amb_incidents2<-amb_incidents %>% 
  filter(org_code=="Eng") %>% 
  select(c(year:c4,date)) %>% 
  mutate(date=as.Date(date, format="%Y-%m-%d")) %>% 
  select(all_incidents:date) %>% 
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


calcs


