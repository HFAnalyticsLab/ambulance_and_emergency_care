#Ambulance handover times 


# Housekeeping ------------------------------------------------------------

rm(list=ls())

#Library
library(aws.s3)
library(tidyverse)
library(ggplot2)
library(ISOweek)


# Data load ---------------------------------------------------------------
buck<-'thf-dap-tier0-projects-iht-067208b7-projectbucket-1mrmynh0q7ljp/ambulance/clean'

#amball <- readRDS("amball201722.rds")

amball<-s3read_using(read.csv # Which function are we using to read
                      , object = 'amball201722.csv' # File to open
                      , bucket = buck) # Bucket name defined above



# Format data -------------------------------------------------------------

#Set dates for the different years
date_1<-format(as.Date(seq(ymd('2017-11-20'),ymd('2018-03-04'),by='1 day')),"%Y-%m-%d")
date_2<-format(as.Date(seq(ymd('2018-12-03'),ymd('2019-03-03'), by='1 day')),"%Y-%m-%d")
date_3<-format(as.Date(seq(ymd('2019-12-02'),ymd('2020-03-01'), by='1 day')),"%Y-%m-%d")
date_4<-format(as.Date(seq(ymd('2020-11-30'),ymd('2021-04-04'), by='1 day')),"%Y-%m-%d")
date_5<-format(as.Date(seq(ymd('2021-11-29'),ymd('2022-04-03'), by='1 day')),"%Y-%m-%d")
  

amball <- amball %>%
  mutate(monthyear=format(as.Date(date), "%Y-%m")) %>% 
  mutate(start_week=ISOweek2date(str_c(date2,"-1"))) %>% 
  mutate(year=case_when(date %in% date_1~ "Winter 2017/18",
                        date %in% date_2~ "Winter 2018/19",
                        date %in% date_3~ "Winter 2019/20",
                        date %in% date_4~ "Winter 2020/21",
                        date %in% date_5~ "Winter 2021/22"))



# Data for flourish  -------------------------------------------------------------
summamb <- amball %>%
  drop_na(pctdelay60plus) %>%
  drop_na(pctdelay3060) %>%
  group_by(date2) %>%
  mutate(mean60plus=mean(pctdelay60plus), mean3060=mean(pctdelay3060), n=n()) %>% 
  mutate(mean30plus=mean60plus+mean60plus) %>% 
  select(c(start_week,date2, year,mean60plus,mean3060,mean30plus, n)) %>% 
  distinct() %>% 
  mutate(week=str_sub(date2,6,8)) %>% 
  mutate(start_week=format(start_week, "%d %b %y")) %>% 
  ungroup()
  
#write.csv(summamb,'flourish_amb_handovers.csv')

# Calculations- summary of handover delays by years -----------------------

calcs<-amball %>% 
  drop_na(pctdelay60plus) %>%
  drop_na(pctdelay3060) %>%
  group_by(year) %>%
  summarise_if(is.numeric, sum, na.rm = TRUE) %>% 
  mutate(numdelay30plus=numdelays60plus+numdelays3060, pctdelay30plus=(numdelay30plus/denom)*100,
         pctdelay60plus=(numdelays60plus/denom)*100,
         pctdelay3060=(numdelays3060/denom)*100)

write.csv(calcs, 'calcs.csv')



# Visualise data ----------------------------------------------------------

plot<-summamb %>% 
  select(start_week, year, mean60plus, mean3060) %>% 
  pivot_longer(mean60plus:mean3060,names_to="metric", values_to="prop") %>% 
  mutate(met_lab=factor(metric, levels=c("mean3060", "mean60plus"), 
                        labels=c("Handovers within 30-60 mins(%)", "Handovers taking more than 60 mins(%)"))) %>% 
  ggplot(.,aes(x=start_week, y=as.numeric(prop), fill=met_lab, colour=met_lab))+
  # scale_x_yearmonth( breaks = '6 months',date_labels = "%b %g")+
  theme_THF()+
  facet_grid(cols=vars(year))+
  scale_fill_THF()+
  # scale_y_continuous()+
  labs(x = "", y="Handovers over 30 mins", caption = "")+
  theme(legend.text=element_text(size=11),
        legend.title = element_blank(),
        axis.text.x=element_text(size=8, angle=60), 
        axis.text.y=element_text(size=11),
        plot.caption = element_markdown(hjust=0, size=9),
        plot.margin = unit(c(1,1.5,0.5,0.5), "cm"),
        legend.margin=margin(0,0,0,0),
        legend.box.margin=margin(-10,-10,-10,-10))


  
