library(data.table)
library(aws.s3)
library(readr)
library(tidyverse)
library(rio)
library(lubridate)
library(ggplot2)
library(tsibble)
library(feasts)
library(hms)
library(tidyverse)
library(ggtext)
library(THFstyle)



#Data load

buck<-'thf-dap-tier0-projects-iht-067208b7-projectbucket-1mrmynh0q7ljp/ambulance/clean'


amb_dta<-s3read_using(read.csv # Which function are we using to read
                   , object = 'amb_RT_clean.csv' # File to open
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
        legend.box.margin=margin(-10,-10,-10,-10))

ggplotly(c4) %>% 
  layout(legend = list(orientation = 'h')) %>% 
  layout(legend=list(title=list(text='')))



# scale_y_continuous(breaks = as_hms(seq(0, 1500, by = 300)))

