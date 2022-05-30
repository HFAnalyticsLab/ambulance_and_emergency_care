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
vars<-c(paste0("c1_",c("mean", "90thcent")),c(paste0("c1T_",c("mean", "90thcent"))),
c(paste0("c2_",c("mean", "90thcent"))),c(paste0("c3_",c("mean", "90thcent"))),
c(paste0("c4_",c("mean", "90thcent"))))



amb_dta_plot<-amb_dta %>% 
 pivot_longer(c(c1_mean:c4_90thcent), names_to = 'metric', values_to = 'resp_time')

try<-amb_dta_plot %>% 
  mutate(resp_time2=as.POSIXct(as.numeric(resp_time),origin = "1970-01-01", tz="GMT")) %>% 
  mutate(resp_time2=format(resp_time2, format="%H:%M:%S")) %>% 
  mutate(resp_time2=as_hms(resp_time2)) %>% 
  mutate(date2=yearmonth(date))


targ<-


amb_ts_plot<-amb_dta_plot %>%
  # filter(metric=="c1_mean") %>%
  mutate(date=yearmonth(date))
# 
# amb_ts_plot= as_tsibble(amb_ts_plot,
#                         index= date,
#                         key= metric, 
#                         validate=FALSE)



try %>%
  filter(str_detect(metric, 'c1_')) %>%
  ggplot(.,aes(x=date2, y=resp_time2, group=region, colour=region))+
  geom_line(linetype='dashed')+
  geom_point(size=1)+
  annotate("segment", x = min(try$date2), xend = max(try$date2), 
           y =00:07:00, yend = 00:07:00, alpha = .1,fill = "grey20")+
  theme_THF()+
  facet_grid(rows=vars(metric))+
  theme(legend.text=element_text(size=11),
        axis.text.x=element_text(size=14),
        axis.text.y=element_text(size=11),
        plot.caption = element_markdown(hjust=0, size=9),
        plot.margin = unit(c(1,1.5,0.5,0.5), "cm"),
        legend.margin=margin(0,0,0,0),
        legend.box.margin=margin(-10,-10,-10,-10))

