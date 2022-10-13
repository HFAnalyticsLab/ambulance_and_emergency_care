#Ambulance response times visualisation 


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
library(tsibble)
library(ggtext)
library(plotly)

#Functions

trends_graph <-  function(data=amb_dta_plot,var.x="North East and Yorkshire"){
  aaa <- ggplot2::enquo(var.x)
  # bbb <- ggplot2::enquo(var.y)
  plot <-  data %>%
    filter(org_lab==!!aaa) %>%
    mutate(met_group=substr(metric,0,3)) %>% 
    mutate(met_cat=ifelse(str_detect(metric,'mean'),'Mean','90th centile')) %>% 
    mutate(met_cat=factor(met_cat, levels=c('Mean', '90th centile'))) %>% 
    mutate(met_lab=ifelse(str_detect(metric, 'mean'), paste(met_group,"_Mean (hours:min:sec)")
                          ,paste(met_group,"_90th centile (hours:min:sec)"))) %>% 
    ggplot(.,aes(x=date2, y=resp_time2, group=met_group, colour=met_group))+
    geom_line(aes(linetype=met_cat))+
    # geom_point(size=0.25)+
    # geom_hline(yintercept = as_hms("00:07:00"), colour = '#524c48', linetype='dashed' )+
    # geom_hline(yintercept = as_hms("00:15:00"), colour = '#524c48', linetype='dashed')+
    scale_x_yearmonth( breaks = '6 months',date_labels = "%b %g")+
    theme_THF()+
    facet_grid(cols=vars(met_cat))+
    scale_colour_THF()+
    labs(x = "", y="Response time (minutes)", caption = "NHS England, Ambulance Quality Indicators")+
    ggtitle(var.x)+
    theme(legend.text=element_text(size=11),
          legend.title = element_blank(),
          axis.text.x=element_text(size=8, angle=60), 
          axis.text.y=element_text(size=11),
          plot.caption = element_markdown(hjust=0, size=9),
          plot.margin = unit(c(1,1.5,0.5,0.5), "cm"),
          legend.margin=margin(0,0,0,0),
          legend.box.margin=margin(-10,-10,-10,-10))
  
  plot
}

# Data load ---------------------------------------------------------------

buck<-'thf-dap-tier0-projects-iht-067208b7-projectbucket-1mrmynh0q7ljp/ambulance/clean'

amb_dta<-s3read_using(read.csv # Which function are we using to read
                   , object = 'amb_RT_regions.csv' # File to open
                   , bucket = buck) # Bucket name defined above


# Format data --------------------------------------------------

#Make columns numeric
amb_dta[7:16] = lapply(amb_dta[7:16], FUN = function(y){as.numeric(y)})

amb_dta_plot<-amb_dta %>% 
  pivot_longer(c(c1_mean:c4_90thcent), names_to = 'metric', values_to = 'resp_time')

amb_dta_plot<-amb_dta_plot %>% 
  mutate(resp_time2=as.POSIXct(as.numeric(resp_time),origin = "1970-01-01", tz="GMT")) %>% 
  mutate(resp_time2=format(resp_time2, format="%H:%M:%S")) %>% 
  mutate(resp_time2=as_hms(resp_time2)) %>% 
  mutate(date2=yearmonth(date)) %>% 
  mutate(org_lab=factor(org_name, levels=c("England","North East and Yorkshire","North West",
                                           "Midlands","East of England","London","South East","South West")))

# Visualise response times by regions -------------------------------------

vars<-list() # create empty list to add to
for (j in seq_along(unique(amb_dta_plot$org_lab))) {
  vars[[j]] <-unique(amb_dta_plot$org_lab)[j]
}

g<-lapply(vars[1:length(vars)],trends_graph,data=amb_dta_plot)

g


# Calculations for average response times ---------------------------------

#Date ranges
pre_dates<-format(as.Date(seq(ymd('2018-04-01'),ymd('2019-03-01'),by='1 month')),"%Y-%m-%d")
post_dates<-format(as.Date(seq(ymd('2021-04-01'),ymd('2022-03-01'),by='1 month')),"%Y-%m-%d")
list_dates<-c(pre_dates, post_dates)

#Load data 
amb_dta<-read_csv(here::here('data', "ambsys.csv"))

#Select and format relevant columns 
amb_dta_clean<-amb_dta %>% 
  clean_names() %>% 
  select(year:org_name, paste0("a",c(8,10:12,24,30,33,36))) %>% 
  filter(region=="Eng") %>% 
  mutate(date=as.Date(paste0(year,"/",ifelse (month<10, paste0(0,month),month),"/",01))) %>% 
  filter(as.character(date) %in% list_dates) %>% 
  mutate(time=case_when(as.character(date) %in% pre_dates ~ "2018/19",
                        as.character(date) %in% post_dates ~ "2021/22",
                        TRUE ~ "NA"))

amb_dta_clean[6:13] = lapply(amb_dta_clean[6:13], FUN = function(y){as.numeric(y)})

calcs<-amb_dta_clean %>% 
  group_by(time) %>% 
  summarise(across(where(is.numeric), ~ sum(.x, na.rm = TRUE))) %>% 
  mutate(total_incidents=a8+a10+a11+a12) %>% 
  mutate(total_hours=a24+a30+a33+a36) %>% 
  select(c(time, total_incidents, total_hours)) %>% 
  mutate(mean_resptime=total_hours/total_incidents) %>% 
  mutate(resp_time2=as.POSIXct(as.numeric(mean_resptime),origin = "1970-01-01", tz="GMT")) %>% 
  mutate(resp_time2=format(resp_time2, format="%H:%M:%S")) %>% 
  mutate(resp_time2=as_hms(resp_time2)) 

calcs 

  
  
  
