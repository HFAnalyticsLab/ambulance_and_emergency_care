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


# Make variables numeric --------------------------------------------------

amb_dta[7:16] = lapply(amb_dta[7:16], FUN = function(y){as.numeric(y)})


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

amb_dta_charts<-amb_dta %>% 
  mutate(monthyear=format(as.Date(date), "%b %y"))

amb_dta_charts[7:16] = lapply(amb_dta_charts[7:16], FUN = function(y){(y)/60})

cc<-amb_dta_charts %>% 
  select(date, monthyear, org_name, contains("c1_")) %>% 
  pivot_longer(contains("c1_"), names_to="Metric", values_to="C1") %>% 
  mutate(Metric=substr(Metric, 4,str_length(Metric))) %>% 
  left_join(amb_dta_charts %>% 
              select(date, monthyear, org_name, contains("c2_")) %>% 
              pivot_longer(contains("c2_"), names_to="Metric", values_to="C2") %>% 
              mutate(Metric=substr(Metric, 4,str_length(Metric)))) %>% 
  left_join(amb_dta_charts %>% 
  select(date, monthyear, org_name, contains("c3_")) %>% 
  pivot_longer(contains("c3_"), names_to="Metric", values_to="C3") %>% 
  mutate(Metric=substr(Metric, 4,str_length(Metric)))) %>% 
  left_join(amb_dta_charts %>% 
  select(date, monthyear, org_name, contains("c4_")) %>% 
  pivot_longer(contains("c4_"), names_to="Metric", values_to="C4") %>% 
  mutate(Metric=substr(Metric, 4,str_length(Metric)))) %>% 
  mutate(Metric= ifelse(Metric=="mean", "Mean", "90th percentile"))

write.csv(amb_dta_plot,'amb_dta_resp_charts.csv')


#calculations

pre_dates<-format(as.Date(seq(ymd('2018-08-01'),ymd('2019-07-01'),by='1 month')),"%Y-%m-%d")
post_dates<-format(as.Date(seq(ymd('2021-08-01'),ymd('2022-07-01'),by='1 month')),"%Y-%m-%d")
list_dates<-c(pre_dates, post_dates)

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



write.csv(pre, 'pre.csv')
write.csv(post, 'post.csv' )

#Alternative figure 2 
amb_dta_try<-amb_dta %>% 
  select(org_code:org_name, date, contains("mean")) %>% 
  pivot_longer(cols = contains("mean"), names_to = "Metric", values_to="Mean") %>% 
  mutate(Metric=ifelse(str_detect(Metric,"c1T"), substr(Metric, 0,3), substr(Metric, 0,2))) %>% 
  left_join(amb_dta %>% 
  select(org_code:org_name, date, contains("90thcent")) %>% 
  pivot_longer(cols=contains("90thcent"), names_to="Metric", values_to="90thcent") %>% 
  mutate(Metric=ifelse(str_detect(Metric,"c1T"), substr(Metric, 0,3), substr(Metric, 0,2)))) %>% 
  mutate(date2=yearmonth(date)) %>% 
  filter(Metric!="c1T") %>% 
  mutate(monthyear=format(as.Date(date), "%b %y")) 
  

write_csv(amb_dta_try, 'amb_dta_resp_time_v2.csv')


#Calcs average
amb_dta<-read_csv(here::here('data', "ambsys.csv"))

#select relevant columns 
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
  
  
  
