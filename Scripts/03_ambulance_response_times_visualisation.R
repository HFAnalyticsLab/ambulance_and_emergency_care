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


rm(list=ls())




#Data load

buck<-'thf-dap-tier0-projects-iht-067208b7-projectbucket-1mrmynh0q7ljp/ambulance/clean'


amb_dta<-s3read_using(read.csv # Which function are we using to read
                   , object = 'amb_RT_regions.csv' # File to open
                   , bucket = buck) # Bucket name defined above

# Category x Regions -----------------------------------------------------------------


#reformat for plots
# vars<-c(paste0("c1_",c("mean", "90thcent")),c(paste0("c1T_",c("mean", "90thcent"))),
# c(paste0("c2_",c("mean", "90thcent"))),c(paste0("c3_",c("mean", "90thcent"))),
# c(paste0("c4_",c("mean", "90thcent"))))

amb_dta_plot<-amb_dta %>% 
 pivot_longer(c(c1_mean:c4_90thcent), names_to = 'metric', values_to = 'resp_time')

amb_dta_plot<-amb_dta_plot %>% 
  mutate(resp_time2=as.POSIXct(as.numeric(resp_time),origin = "1970-01-01", tz="GMT")) %>% 
  mutate(resp_time2=format(resp_time2, format="%H:%M:%S")) %>% 
  mutate(resp_time2=as_hms(resp_time2)) %>% 
  mutate(date2=yearmonth(date)) %>% 
  mutate(org_lab=factor(org_name, levels=c("England","North East and Yorkshire","North West",
                                              "Midlands","East of England","London","South East","South West")))

# 
# amb_ts_plot= as_tsibble(amb_ts_plot,
#                         index= date,
#                         key= metric, 
#                         validate=FALSE)


##C1 response times 
c1<-amb_dta_plot %>%
  filter(str_detect(metric, 'c1_')) %>%
  mutate(met_lab=ifelse(metric=="c1_mean", "Mean (min:sec)", "90th centile (min:sec)")) %>% 
  ggplot(.,aes(x=date2, y=resp_time2, group=met_lab, colour=met_lab))+
  geom_line(linetype='solid')+
  # geom_point(size=0.25)+
  geom_hline(yintercept = as_hms("00:07:00"), colour = '#524c48', linetype='dashed' )+
  geom_hline(yintercept = as_hms("00:15:00"), colour = '#524c48', linetype='dashed')+
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

ggplotly(c1) %>% 
  layout(legend = list(orientation = 'h')) %>% 
  layout(legend=list(title=list(text='')))

##C1T response times (not working)
c1T<-amb_dta_plot %>%
  filter(str_detect(metric, 'c1T_')) %>%
  mutate(met_lab=ifelse(metric=="c1T_mean", "Mean (min:sec)", "90th centile (min:sec)")) %>% 
  ggplot(.,aes(x=date2, y=resp_time2, group=met_lab, colour=met_lab))+
  geom_line(linetype='solid')+
  # geom_point(size=0.25)+
  geom_hline(yintercept = as_hms("00:07:00"), colour = '#524c48', linetype='dashed' )+
  geom_hline(yintercept = as_hms("00:15:00"), colour = '#524c48', linetype='dashed')+
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

ggplotly(c1T) %>% 
  layout(legend = list(orientation = 'h')) %>% 
  layout(legend=list(title=list(text='')))


##C2 response times 
c2<-amb_dta_plot %>%
  filter(str_detect(metric, 'c2_')) %>%
  mutate(met_lab=ifelse(metric=="c2_mean", "Mean (min:sec)", "90th centile (min:sec)")) %>% 
  ggplot(.,aes(x=date2, y=resp_time2, group=met_lab, colour=met_lab))+
  geom_line(linetype='solid')+
  # geom_point(size=0.25)+
  geom_hline(yintercept = as_hms("00:07:00"), colour = '#524c48', linetype='dashed' )+
  geom_hline(yintercept = as_hms("00:15:00"), colour = '#524c48', linetype='dashed')+
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

ggplotly(c2) %>% 
  layout(legend = list(orientation = 'h')) %>% 
  layout(legend=list(title=list(text='')))

##C3 
c3<-amb_dta_plot %>%
  filter(str_detect(metric, 'c3_')) %>%
  mutate(met_lab=ifelse(metric=="c3_mean", "Mean (min:sec)", "90th centile (min:sec)")) %>% 
  ggplot(.,aes(x=date2, y=resp_time2, group=met_lab, colour=met_lab))+
  geom_line(linetype='solid')+
  # geom_point(size=0.25)+
  geom_hline(yintercept = as_hms("00:07:00"), colour = '#524c48', linetype='dashed' )+
  geom_hline(yintercept = as_hms("00:15:00"), colour = '#524c48', linetype='dashed')+
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

ggplotly(c3) %>% 
  layout(legend = list(orientation = 'h')) %>% 
  layout(legend=list(title=list(text='')))


##C4
c4<-amb_dta_plot %>%
  filter(str_detect(metric, 'c4_')) %>%
  mutate(met_lab=ifelse(metric=="c4_mean", "Mean (min:sec)", "90th centile (min:sec)")) %>% 
  ggplot(.,aes(x=date2, y=resp_time2, group=met_lab, colour=met_lab))+
  geom_line(linetype='solid')+
  # geom_point(size=0.25)+
  geom_hline(yintercept = as_hms("00:07:00"), colour = '#524c48', linetype='dashed' )+
  geom_hline(yintercept = as_hms("00:15:00"), colour = '#524c48', linetype='dashed')+
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
        legend.box.margin=margin(-10,-10,-10,-10),
        strip.text.x = element_text(size = 6))

ggplotly(c4) %>% 
  layout(legend = list(orientation = 'h')) %>% 
  layout(legend=list(title=list(text=''))) 
  



# scale_y_continuous(breaks = as_hms(seq(0, 1500, by = 300)))


# Seperate for Regions ----------------------------------------------------


#Data load

buck<-'thf-dap-tier0-projects-iht-067208b7-projectbucket-1mrmynh0q7ljp/ambulance/clean'


amb_dta<-s3read_using(read.csv # Which function are we using to read
                      , object = 'amb_RT_regions.csv' # File to open
                      , bucket = buck) # Bucket name defined above

#reformat for plots
# vars<-c(paste0("c1_",c("mean", "90thcent")),c(paste0("c1T_",c("mean", "90thcent"))),
# c(paste0("c2_",c("mean", "90thcent"))),c(paste0("c3_",c("mean", "90thcent"))),
# c(paste0("c4_",c("mean", "90thcent"))))

amb_dta_plot<-amb_dta %>% 
  pivot_longer(c(c1_mean:c4_90thcent), names_to = 'metric', values_to = 'resp_time')

amb_dta_plot<-amb_dta_plot %>% 
  mutate(resp_time2=as.POSIXct(as.numeric(resp_time),origin = "1970-01-01", tz="GMT")) %>% 
  mutate(resp_time2=format(resp_time2, format="%H:%M:%S")) %>% 
  mutate(resp_time2=as_hms(resp_time2)) %>% 
  mutate(date2=yearmonth(date)) %>% 
  mutate(org_lab=factor(org_name, levels=c("England","North East and Yorkshire","North West",
                                           "Midlands","East of England","London","South East","South West")))
#First type of trends graph 
amb_dta_plot %>%
  filter(org_lab=="North East and Yorkshire") %>%
  mutate(met_group=substr(metric,0,3)) %>% 
  mutate(met_lab=ifelse(str_detect(metric, 'mean'), "_Mean (hours:min:sec)"
                        ,"_90th centile (min:sec)")) %>% 
  ggplot(.,aes(x=date2, y=resp_time2, group=met_lab, colour=met_lab))+
  geom_line(linetype='solid')+
  # geom_point(size=0.25)+
  # geom_hline(yintercept = as_hms("00:07:00"), colour = '#524c48', linetype='dashed' )+
  # geom_hline(yintercept = as_hms("00:15:00"), colour = '#524c48', linetype='dashed')+
  scale_x_yearmonth( breaks = '6 months',date_labels = "%b %g")+
  scale_y_time(limits = as_hms(c(0,72000)), breaks = as_hms(seq(0, 72000, by = 3600)))+
  theme_THF()+
  facet_grid(cols=vars(met_group))+
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


#second type 
amb_dta_plot %>%
  filter(org_lab=="North East and Yorkshire") %>%
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
  # scale_y_time(limits = as_hms(c(0,72000)), breaks = as_hms(seq(0, 72000, by = 7200)))+
  scale_y_time(breaks = as_hms(seq(0, 72000, by = 1800)))+
  theme_THF()+
  facet_grid(cols=vars(met_cat))+
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





trends_graph()






# Category x Trusts ------------------------------------------------------------------

#Data load

buck<-'thf-dap-tier0-projects-iht-067208b7-projectbucket-1mrmynh0q7ljp/ambulance/clean'


amb_dta_trusts<-s3read_using(read.csv # Which function are we using to read
                      , object = 'amb_RT_trusts.csv' # File to open
                      , bucket = buck) # Bucket name defined above


amb_dta_plot<-amb_dta_trusts %>% 
  pivot_longer(c(c1_mean:c4_90thcent), names_to = 'metric', values_to = 'resp_time') %>% 
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
  mutate(resp_time2=as.POSIXct(as.numeric(resp_time),origin = "1970-01-01", tz="GMT")) %>% 
  mutate(resp_time2=format(resp_time2, format="%H:%M:%S")) %>% 
  mutate(resp_time2=as_hms(resp_time2)) %>% 
  mutate(date2=yearmonth(date)) %>%
  mutate(org_lab=factor(org_name, levels=long_names, labels=label_names))


amb_dta_plot %>%
  filter(str_detect(metric, 'c1_')) %>%
  mutate(met_lab=ifelse(metric=="c1_mean", "Mean (min:sec)", "90th centile (min:sec)")) %>% 
  ggplot(.,aes(x=date2, y=resp_time2, group=met_lab, colour=met_lab))+
  geom_line(linetype='solid')+
  # geom_point(size=0.25)+
  geom_hline(yintercept = as_hms("00:07:00"), colour = '#524c48', linetype='dashed' )+
  geom_hline(yintercept = as_hms("00:15:00"), colour = '#524c48', linetype='dashed')+
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

ggplotly(c4) %>% 
  layout(legend = list(orientation = 'h')) %>% 
  layout(legend=list(title=list(text=''))) 



# For each trust ----------------------------------------------------------

trends_graph(var.x=label_names[4])




# Data for flourish -------------------------------------------------------

amb_dta_mean<-amb_dta %>% 
  select(c(org_code:org_name, "date", contains("mean"))) %>% 
  mutate(type="Mean")

names(amb_dta_mean)[4:8]<-c("c1", "c1t", "c2", "c3", "c4")


amb_dta_90th<-amb_dta %>% 
  select(c(org_code:org_name, "date", contains("90thcent"))) %>% 
  mutate(type="90th percentile")
  
names(amb_dta_90th)[4:8]<-c("c1", "c1t", "c2", "c3", "c4")


amb_dta_flourish<-rbind(amb_dta_mean, amb_dta_90th)

amb_dta_flourish<-amb_dta_flourish %>% 
  mutate(date2=yearmonth(date)) %>% 
  mutate(org_lab=factor(org_name, levels=c("England","North East and Yorkshire","North West",
                                           "Midlands","East of England","London","South East","South West"))) %>% 
  mutate(c1=as.POSIXct(as.numeric(c1),origin = "1970-01-01", tz="GMT")) %>% 
  mutate(c1=format(c1, format="%H:%M:%S")) %>% 
  mutate(c1=as_hms(c1)) %>% 
  mutate(c2=as.POSIXct(as.numeric(c2),origin = "1970-01-01", tz="GMT")) %>% 
  mutate(c2=format(c2, format="%H:%M:%S")) %>% 
  mutate(c2=as_hms(c2)) %>% 
  mutate(c3=as.POSIXct(as.numeric(c3),origin = "1970-01-01", tz="GMT")) %>% 
  mutate(c3=format(c3, format="%H:%M:%S")) %>% 
  mutate(c3=as_hms(c3)) %>% 
  mutate(c4=as.POSIXct(as.numeric(c4),origin = "1970-01-01", tz="GMT")) %>% 
  mutate(c4=format(c4, format="%H:%M:%S")) %>% 
  mutate(c4=as_hms(c4)) 


amb_dta_try<-amb_dta_flourish %>% 
  mutate(c1=as.character(c1),
         c2=as.character(c2),
         c3=as.character(c3), 
         c4=as.character(c4))


write.csv(amb_dta_flourish, "amb_dta_flourish.csv")

#### save R objects from the environment directly to your s3 bucket
buck <- 'thf-dap-tier0-projects-iht-067208b7-projectbucket-1mrmynh0q7ljp/ambulance/outputs' ## my bucket name

s3write_using(amb_dta_flourish # What R object we are saving
              , FUN = write.csv # Which R function we are using to save
              , object = 'amb_resptimes.csv' # Name of the file to save to (include file type)
              , bucket = buck) # Bucket name defined above


amb_dta_plot_eng<-amb_dta_plot %>% 
  filter(org_lab=="England")


s3write_using(amb_dta_plot_eng # What R object we are saving
              , FUN = write.csv # Which R function we are using to save
              , object = 'amb_resptimes_eng.csv' # Name of the file to save to (include file type)
              , bucket = buck) # Bucket name defined above


amb_dta_plot_regions<-amb_dta_plot %>% 
  filter(org_lab!="England")


s3write_using(amb_dta_plot_regions # What R object we are saving
              , FUN = write.csv # Which R function we are using to save
              , object = 'amb_resptimes_regions.csv' # Name of the file to save to (include file type)
              , bucket = buck) # Bucket name defined above



#calculations

pre_dates<-format(as.Date(seq(ymd('2018-03-01'),ymd('2019-06-01'),by='1 month')),"%Y-%m-%d")
p_dates<-format(as.Date(seq(ymd('2020-03-01'),ymd('2021-06-01'),by='1 month')),"%Y-%m-%d")
post_dates<-format(as.Date(seq(ymd('2021-07-01'),ymd('2022-06-01'),by='1 month')),"%Y-%m-%d")


pre<-amb_dta %>% 
  filter(date %in% pre_dates & region=="Eng") %>% 
  select(org_name:date) %>% 
  pivot_longer(c1_mean:c4_90thcent, names_to="metric", values_to="resp_time") %>% 
  mutate(resp_time=as.numeric(resp_time)) %>% 
  group_by(metric) %>% 
  summarise(mean_resp_time=mean(resp_time)) %>% 
  mutate(resp_time2=as.POSIXct(as.numeric(mean_resp_time),origin = "1970-01-01", tz="GMT")) %>% 
  mutate(resp_time2=format(resp_time2, format="%H:%M:%S")) %>% 
  mutate(resp_time2=as_hms(resp_time2))

  
pandemic<-amb_dta %>% 
  filter(date %in% p_dates & region=="Eng") %>% 
  select(org_name:date) %>% 
  pivot_longer(c1_mean:c4_90thcent, names_to="metric", values_to="resp_time") %>% 
  mutate(resp_time=as.numeric(resp_time)) %>% 
  group_by(metric) %>% 
  summarise(mean_resp_time=mean(resp_time)) %>% 
  mutate(resp_time2=as.POSIXct(as.numeric(mean_resp_time),origin = "1970-01-01", tz="GMT")) %>% 
  mutate(resp_time2=format(resp_time2, format="%H:%M:%S")) %>% 
  mutate(resp_time2=as_hms(resp_time2))

post<-amb_dta %>% 
  filter(date %in% post_dates & region=="Eng") %>% 
  select(org_name:date) %>% 
  pivot_longer(c1_mean:c4_90thcent, names_to="metric", values_to="resp_time") %>% 
  mutate(resp_time=as.numeric(resp_time)) %>% 
  group_by(metric) %>% 
  summarise(mean_resp_time=mean(resp_time)) %>% 
  mutate(resp_time2=as.POSIXct(as.numeric(mean_resp_time),origin = "1970-01-01", tz="GMT")) %>% 
  mutate(resp_time2=format(resp_time2, format="%H:%M:%S")) %>% 
  mutate(resp_time2=as_hms(resp_time2))
