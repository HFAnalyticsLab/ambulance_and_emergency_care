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
library(ggtext)
library(stringr)
library(tsibble)
library(janitor)

#Functions

trends_graph <-  function(data=amb_dta_plot,var.x="North East and Yorkshire"){
  aaa <- ggplot2::enquo(var.x)
  # bbb <- ggplot2::enquo(var.y)
  plot <-  data %>%
    filter(org_lab==!!aaa) %>%
    mutate(met_group=substr(metric,0,2)) %>% 
    mutate(met_group=factor(met_group, levels=c("c1", "c2", "c3", "c4"),
                            labels=c("Category 1","Category 2", "Category 3", "Category 4"))) %>% 
    mutate(met_cat=ifelse(str_detect(metric,'mean'),'Mean','90th centile')) %>% 
    mutate(met_cat=factor(met_cat, levels=c('Mean', '90th centile'))) %>% 
    mutate(met_lab=ifelse(str_detect(metric, 'mean'), paste(met_group,"_Mean (hours:min:sec)")
                          ,paste(met_group,"_90th centile (hours:min:sec)"))) %>% 
    ggplot(.,aes(x=date, y=resp_time2, group=met_group, colour=met_group))+
    geom_line(aes(linetype=met_cat))+
    annotate("rect", xmin=as.Date("2020-03-01"), xmax=as.Date("2021-05-01"), 
             ymin=0, ymax=max(data$resp_time),fill="grey20", alpha=.1)+
    annotate("richtext",x=as.Date("2020-03-01"), y=(max(data$resp_time)-4000), 
             label= "First two waves <br> of COVID-19", size=3, colour="black",hjust=0, fill=NA, label.color=NA)+
    # geom_point(size=0.25)+
    # geom_hline(yintercept = as_hms("00:07:00"), colour = '#524c48', linetype='dashed' )+
    # geom_hline(yintercept = as_hms("00:15:00"), colour = '#524c48', linetype='dashed')+
    tsibble::scale_x_yearmonth( breaks = '6 months',date_labels = "%b %y")+
    theme_THF()+
    facet_grid(cols=vars(met_cat))+
    scale_colour_THF()+
    labs(x = "", y="Response time (hrs:mins:secs)", caption = "NHS England, Ambulance Quality Indicators")+
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

#Does not contain to be used for filter 
`%notin%` <- Negate(`%in%`)


# Ambulance response times ------------------------------------------------


# Data load ---------------------------------------------------------------

buck<-'thf-dap-tier0-projects-iht-067208b7-projectbucket-1mrmynh0q7ljp/ambulance/clean'

amb_dta<-s3read_using(read.csv # Which function are we using to read
                      , object = 'amb_RT_regions.csv' # File to open
                      , bucket = buck) # Bucket name defined above


# Format data --------------------------------------------------

#Make columns numeric
amb_dta[7:16]=lapply(amb_dta[7:16], FUN = function(y){gsub(",","",y)})
amb_dta[7:16] = lapply(amb_dta[7:16], FUN = function(y){as.numeric(y)})

amb_dta_plot<-amb_dta %>%
  pivot_longer(c(c1_mean:c4_90thcent), names_to = 'metric', values_to = 'resp_time')

amb_dta_plot<-amb_dta_plot %>% 
  mutate(resp_time2=as.POSIXct(as.numeric(resp_time),origin = "1970-01-01", tz="GMT")) %>% 
  mutate(resp_time2=format(resp_time2, format="%H:%M:%S")) %>% 
  mutate(resp_time2=as_hms(resp_time2)) %>% 
  mutate(org_lab=factor(org_name, levels=c("England","North East and Yorkshire","North West",
                                           "Midlands","East of England","London","South East","South West"))) %>% 
  filter(!str_detect(metric, "c1T")) %>% 
  mutate(date=as.Date(date, format="%Y-%m-%d")) %>% 
  mutate(monthyear=format(date, "%b %y"))

filter_dates<-as.Date(seq(ymd('2017-08-01'),ymd('2018-03-01'),by='1 month'), format="%Y-%m-%d")

amb_dta_plot<-amb_dta_plot %>% 
  filter(date %notin% filter_dates)



# Visualise response times -------------------------------------


plot <-  amb_dta_plot %>%
filter(org_lab=="England") %>%
  mutate(met_group=substr(metric,0,2)) %>% 
  mutate(met_group=factor(met_group, levels=c("c1", "c2", "c3", "c4"),
                          labels=c("Category 1","Category 2", "Category 3", "Category 4"))) %>% 
  mutate(met_cat=ifelse(str_detect(metric,'mean'),'Mean','90th centile')) %>% 
  mutate(met_cat=factor(met_cat, levels=c('Mean', '90th centile'))) %>% 
  mutate(met_lab=ifelse(str_detect(metric, 'mean'), paste(met_group,"_Mean (hours:min:sec)")
                        ,paste(met_group,"_90th centile (hours:min:sec)"))) %>% 
  ggplot(.,aes(x=date, y=resp_time2, group=met_cat, colour=met_cat))+
  geom_line(aes(linetype=met_cat))+
  #annotate("rect", xmin=as.Date("2020-03-01"), xmax=as.Date("2021-05-01"), 
          # ymin=0, ymax=max(data$resp_time),fill="grey20", alpha=.1)+
 # annotate("richtext",x=as.Date("2020-03-01"), y=(max(data$resp_time)-4000), 
          # label= "First two waves <br> of COVID-19", size=3, colour="black",hjust=0, fill=NA, label.color=NA)+
  # geom_point(size=0.25)+
  # geom_hline(yintercept = as_hms("00:07:00"), colour = '#524c48', linetype='dashed' )+
  # geom_hline(yintercept = as_hms("00:15:00"), colour = '#524c48', linetype='dashed')+
  #tsibble::scale_x_yearmonth( breaks = '6 months',date_labels = "%b %y")+
  theme_THF()+
  facet_grid(cols=vars(met_group))+
  scale_colour_THF()+
  labs(x = "", y="Response time (hrs:mins:secs)", caption = "NHS England, Ambulance Quality Indicators")+
  theme(legend.text=element_text(size=11),
        legend.title = element_blank(),
        axis.text.x=element_text(size=8, angle=60), 
        axis.text.y=element_text(size=11),
        plot.caption = element_markdown(hjust=0, size=9),
        plot.margin = unit(c(1,1.5,0.5,0.5), "cm"),
        legend.margin=margin(0,0,0,0),
        legend.box.margin=margin(-10,-10,-10,-10))



plot

filter_dates<-as.Date(seq(ymd('2022-11-01'),ymd('2023-03-01'),by='1 month'), format="%Y-%m-%d")

amb_dta_plot %>%
  #filter(date %in% filter_dates) %>% 
  filter(org_lab=="England") %>%
  mutate(met_group=substr(metric,0,2)) %>% 
  mutate(met_group=factor(met_group, levels=c("c1", "c2", "c3", "c4"),
                          labels=c("Category 1","Category 2", "Category 3", "Category 4"))) %>% 
  filter(met_group=="Category 1") %>% 
  mutate(met_cat=ifelse(str_detect(metric,'mean'),'Mean','90th centile')) %>% 
  mutate(met_cat=factor(met_cat, levels=c('Mean', '90th centile'))) %>% 
  mutate(met_lab=ifelse(str_detect(metric, 'mean'), paste(met_group,"_Mean (hours:min:sec)")
                        ,paste(met_group,"_90th centile (hours:min:sec)"))) %>% 
  ggplot(.,aes(x=month, y=resp_time2, group=met_cat, colour=met_cat))+
  geom_line(aes(linetype=met_cat))+
  #annotate("rect", xmin=as.Date("2020-03-01"), xmax=as.Date("2021-05-01"), 
  # ymin=0, ymax=max(data$resp_time),fill="grey20", alpha=.1)+
  # annotate("richtext",x=as.Date("2020-03-01"), y=(max(data$resp_time)-4000), 
  # label= "First two waves <br> of COVID-19", size=3, colour="black",hjust=0, fill=NA, label.color=NA)+
  # geom_point(size=0.25)+
  # geom_hline(yintercept = as_hms("00:07:00"), colour = '#524c48', linetype='dashed' )+
  # geom_hline(yintercept = as_hms("00:15:00"), colour = '#524c48', linetype='dashed')+
 # tsibble::scale_x_yearmonth( breaks = 'months',date_labels = "%b %y")+
  theme_THF()+
  facet_grid(cols=vars(year))+
  scale_colour_THF()+
  labs(x = "", y="Response time (hrs:mins:secs)", caption = "NHS England, Ambulance Quality Indicators")+
  theme(legend.text=element_text(size=11),
        legend.title = element_blank(),
        axis.text.x=element_text(size=8, angle=60), 
        axis.text.y=element_text(size=11),
        plot.caption = element_markdown(hjust=0, size=9),
        plot.margin = unit(c(1,1.5,0.5,0.5), "cm"),
        legend.margin=margin(0,0,0,0),
        legend.box.margin=margin(-10,-10,-10,-10))






#Response times seems to be back to pre pandemic levels and the highest drop seems to be in Category 3 and 4
#Might be worth checking why that is- has there been a change in reporting? 

# Calculations for average response times ---------------------------------

#Date ranges
dates_18_19<-format(as.Date(seq(ymd('2018-04-01'),ymd('2019-03-01'),by='1 month')),"%Y-%m-%d")
dates_19_20<-format(as.Date(seq(ymd('2019-04-01'),ymd('2020-03-01'),by='1 month')),"%Y-%m-%d")
dates_20_21<-format(as.Date(seq(ymd('2020-04-01'),ymd('2021-03-01'),by='1 month')),"%Y-%m-%d")
dates_21_22<-format(as.Date(seq(ymd('2021-04-01'),ymd('2022-03-01'),by='1 month')),"%Y-%m-%d")
dates_22_23<-format(as.Date(seq(ymd('2022-04-01'),ymd('2023-03-01'),by='1 month')),"%Y-%m-%d")
dates_23_24<-format(as.Date(seq(ymd('2023-04-01'),ymd('2024-03-01'),by='1 month')),"%Y-%m-%d")
list_dates<-c(dates_18_19, dates_19_20, dates_20_21, dates_21_22, dates_22_23, dates_23_24)


#Load data 
amb_dta<-read_csv(here::here('data', "ambsys.csv"))


amb_dta_clean_eng<-amb_dta %>% 
  clean_names() %>% 
  select(year:org_name, paste0("a",c(8,10:12, 24,30,33,36, 26,32,35,38)))

#Region codes
list_org_codes_region<-c("Y63", "Y62","Y60", "Y61", "Y56", "Y59", "Y58")

#Rename columns 
names(amb_dta_clean_eng)[c(6,10)]<-c(paste0("c1_",c("incidents", "RT")))
names(amb_dta_clean_eng)[c(7,11)]<-c(paste0("c2_",c("incidents", "RT")))
names(amb_dta_clean_eng)[c(8,12)]<-c(paste0("c3_",c("incidents", "RT")))
names(amb_dta_clean_eng)[c(9,13)]<-c(paste0("c4_",c("incidents", "RT")))
names(amb_dta_clean_eng)[c(14:17)]<-c(paste0(c("c1","c2", "c3", "c4"),"_90RT"))


amb_dta_clean_eng<-amb_dta_clean_eng %>% 
  filter(org_code %in% c(list_org_codes_region)) %>% 
  mutate(date=as.Date(paste0(year,"/",ifelse (month<10, paste0(0,month),month),"/",01))) 


#Make columns numeric
amb_dta_clean_eng[6:17]=lapply(amb_dta_clean_eng[6:17], FUN = function(y){gsub(",","",y)})
amb_dta_clean_eng[6:17] = lapply(amb_dta_clean_eng[6:17], FUN = function(y){as.numeric(y)})

london<-amb_dta_clean_eng %>% 
  filter(region=="London") %>% 
  filter(date %in% c(as.Date("2022-10-01"),as.Date("2022-11-01")))


eng<-amb_dta_clean_eng %>% 
  mutate(c1_incidents=ifelse(region=="London"& date %in% c(as.Date("2022-10-01"),as.Date("2022-11-01")), NA,c1_incidents),
         c2_incidents=ifelse(region=="London"& date %in% c(as.Date("2022-10-01"),as.Date("2022-11-01")), NA,c2_incidents),
         c3_incidents=ifelse(region=="London"& date %in% c(as.Date("2022-10-01"),as.Date("2022-11-01")), NA,c3_incidents),
         c4_incidents=ifelse(region=="London"& date %in% c(as.Date("2022-10-01"),as.Date("2022-11-01")), NA,c4_incidents)) %>% 
  group_by(date) %>%
  summarise(across(where(is.numeric), sum, na.rm=TRUE)) %>% 
  mutate(org_code="Eng") %>% 
  mutate(region="England") %>% 
  mutate(org_name="England") %>% 
  mutate(year=format(date, "%Y")) %>% 
  mutate(month=format(date,"%m"))


eng_calcs<-eng %>% 
  filter(as.character(date) %in% list_dates) %>% 
  mutate(time=case_when(as.character(date) %in% dates_18_19 ~ "2018/19",
                        as.character(date) %in% dates_19_20 ~ "2019/20",
                        as.character(date) %in% dates_20_21 ~ "2020/21",
                        as.character(date) %in% dates_21_22 ~ "2021/22",
                        as.character(date) %in% dates_22_23 ~ "2022/23",
                        as.character(date) %in% dates_23_24 ~ "2023/24",
                        TRUE ~ "NA")) %>% 
  group_by(time) %>% 
  summarise(across(where(is.numeric), ~ sum(.x, na.rm = TRUE))) %>% 
  mutate(c1_mean=round(c1_RT/c1_incidents), 
         c2_mean=round(c2_RT/c2_incidents), 
         c3_mean=round(c3_RT/c3_incidents),
         c4_mean=round(c4_RT/c4_incidents)) %>% 
  select(-contains("RT")) %>% 
  mutate(c1_mean=as.POSIXct(as.numeric(c1_mean),origin = "1970-01-01", tz="GMT")) %>% 
  mutate(c1_mean=format(c1_mean, format="%H:%M:%S")) %>% 
  mutate(c1_mean=as_hms(c1_mean)) %>% 
  mutate(c2_mean=as.POSIXct(as.numeric(c2_mean),origin = "1970-01-01", tz="GMT")) %>% 
  mutate(c2_mean=format(c2_mean, format="%H:%M:%S")) %>% 
  mutate(c2_mean=as_hms(c2_mean)) %>% 
  mutate(c3_mean=as.POSIXct(as.numeric(c3_mean),origin = "1970-01-01", tz="GMT")) %>% 
  mutate(c3_mean=format(c3_mean, format="%H:%M:%S")) %>% 
  mutate(c3_mean=as_hms(c3_mean)) %>% 
  mutate(c4_mean=as.POSIXct(as.numeric(c4_mean),origin = "1970-01-01", tz="GMT")) %>% 
  mutate(c4_mean=format(c4_mean, format="%H:%M:%S")) %>% 
  mutate(c4_mean=as_hms(c4_mean))
  
eng_calcs 


#Date ranges
#Date ranges
dates_18_19<-format(as.Date(seq(ymd('2018-04-01'),ymd('2019-03-01'),by='1 month')),"%Y-%m-%d")
dates_19_20<-format(as.Date(seq(ymd('2019-04-01'),ymd('2020-03-01'),by='1 month')),"%Y-%m-%d")
dates_20_21<-format(as.Date(seq(ymd('2020-04-01'),ymd('2021-03-01'),by='1 month')),"%Y-%m-%d")
dates_21_22<-format(as.Date(seq(ymd('2021-04-01'),ymd('2022-03-01'),by='1 month')),"%Y-%m-%d")
dates_22_23<-format(as.Date(seq(ymd('2022-04-01'),ymd('2023-03-01'),by='1 month')),"%Y-%m-%d")
dates_23_24<-format(as.Date(seq(ymd('2023-04-01'),ymd('2024-03-01'),by='1 month')),"%Y-%m-%d")
list_dates<-c(dates_18_19, dates_19_20, dates_20_21, dates_21_22, dates_22_23, dates_23_24)



#Select and format relevant columns 
amb_dta_clean<-amb_dta %>% 
  clean_names() %>% 
  select(year:org_name, paste0("a",c(8,10:12,24,30,33,36))) %>% 
  filter(region=="Eng") %>% 
  mutate(date=as.Date(paste0(year,"/",ifelse (month<10, paste0(0,month),month),"/",01))) %>% 
  filter(as.character(date) %in% list_dates) %>% 
  mutate(time=case_when(as.character(date) %in% dates_18_19 ~ "2018/19",
                        as.character(date) %in% dates_19_20 ~ "2019/20",
                        as.character(date) %in% dates_20_21 ~ "2020/21",
                        as.character(date) %in% dates_21_22 ~ "2021/22",
                        as.character(date) %in% dates_22_23 ~ "2022/23",
                        as.character(date) %in% dates_23_24 ~ "2023/24",
                        TRUE ~ "NA")) 


amb_dta_clean[6:13]=lapply(amb_dta_clean[6:13], FUN = function(y){gsub(",","",y)})
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



amb_dta_clean<-amb_dta %>% 
  clean_names() %>% 
  select(year:org_name, paste0("a",c(8,10:12,24,30,33,36,7))) %>% 
  filter(region=="Eng") %>% 
  mutate(date=as.Date(paste0(year,"/",ifelse (month<10, paste0(0,month),month),"/",01))) %>% 
  filter(as.character(date) %in% list_dates) %>% 
  mutate(time=case_when(as.character(date) %in% dates_18_19 ~ "2018/19",
                        as.character(date) %in% dates_19_20 ~ "2019/20",
                        as.character(date) %in% dates_20_21 ~ "2020/21",
                        as.character(date) %in% dates_21_22 ~ "2021/22",
                        as.character(date) %in% dates_22_23 ~ "2022/23",
                        as.character(date) %in% dates_23_24 ~ "2023/24",
                        TRUE ~ "NA")) 


amb_dta_clean[6:14]=lapply(amb_dta_clean[6:14], FUN = function(y){gsub(",","",y)})
amb_dta_clean[6:14] = lapply(amb_dta_clean[6:14], FUN = function(y){as.numeric(y)})

calcs<-amb_dta_clean %>% 
  group_by(time) %>% 
  summarise(across(where(is.numeric), ~ sum(.x, na.rm = TRUE))) %>% 
  #mutate(total_incidents=a8+a10+a11+a12) %>% 
  mutate(total_hours=a24+a30+a33+a36) %>% 
  select(c(time, total_incidents=a7, total_hours)) %>% 
  mutate(mean_resptime=total_hours/total_incidents) %>% 
  mutate(resp_time2=as.POSIXct(as.numeric(mean_resptime),origin = "1970-01-01", tz="GMT")) %>% 
  mutate(resp_time2=format(resp_time2, format="%H:%M:%S")) %>% 
  mutate(resp_time2=as_hms(resp_time2)) 

calcs 


update_plot <-  amb_dta_plot %>%
  filter(org_lab=="England") %>%
  mutate(met_group=substr(metric,0,2)) %>% 
  mutate(met_group=factor(met_group, levels=c("c1", "c2", "c3", "c4"),
                          labels=c("Category 1","Category 2", "Category 3", "Category 4"))) %>% 
  mutate(met_cat=ifelse(str_detect(metric,'mean'),'Mean','90th centile')) %>% 
  mutate(met_cat=factor(met_cat, levels=c('Mean', '90th centile'))) %>% 
  pivot_wider(id_cols=c(date,monthyear, met_group), names_from=met_cat, values_from=resp_time2)


write.csv(update_plot, "response_times.csv")




# Handover times ----------------------------------------------------------



x2023handovers<-read_excel("data/raw2023handovers.xlsx", sheet='All handovers', range="F14:BNR19")


ambdelay<-x2023handovers %>% 
  t() %>% 
  as.data.frame() %>% 
  select(group=V1, metric=V2, count=V5) %>% 
  fill(group, .direction="down")


rownames(ambdelay) <-NULL



t<-ambdelay %>% 
  filter(group=="Proportion of handovers") %>% 
  arrange(metric) %>% 
 mutate(date=rep(format(as.Date(seq(ymd('2023-11-20'),ymd('2024-03-31'), by='1 day')),"%Y-%m-%d"),4)) %>% 
  mutate(count=as.numeric(count)) %>% 
  drop_na(count) %>% 
  mutate(date2=date2ISOweek(date)) %>% 
  mutate(date2=str_sub(date2,0,8)) %>% 
  group_by(date2,metric) %>% 
  mutate(mean_count=mean(count)) 



t %>% 
  ggplot(.,aes(x=date2, y=mean_count*100, colour=metric, group=metric, order=metric))+
  geom_line()+
  #scale_y_continuous(labels = scales::percent_format(scale = 10)) +
  #scale_x_yearmonth(breaks = '1 month',date_labels = "%b %g")+
  # scale_x_date(date_breaks = '1 month', date_labels ="%b %g")
  # scale_x_discrete(labels=unique(plot_data$monthyear))+
  theme_THF()+
  facet_grid(cols=vars(metric), scales="free")+
  #scale_fill_manual(values=c('#53a9cd','#dd0031'))+
  labs(x = "", y="", caption = "NHS England, Urgent and Emergency Care Daily Situation Reports")+
  theme(legend.text=element_text(size=11),
        legend.title = element_blank(),
        # axis.text.x = element_blank(),
        axis.text.x=element_text(size=11, angle=60),
        axis.text.y=element_text(size=11),
        plot.caption = element_markdown(hjust=0, size=9),
        plot.margin = unit(c(1,1.5,0.5,0.5), "cm"),
        legend.margin=margin(0,0,0,0),
        legend.box.margin=margin(-10,-10,-10,-10))



# Data load ---------------------------------------------------------------
buck<-'thf-dap-tier0-projects-iht-067208b7-projectbucket-1mrmynh0q7ljp/ambulance/clean'

#amball <- readRDS("amball201722.rds")

amball<-s3read_using(read.csv # Which function are we using to read
                     , object = 'amball201724.csv' # File to open
                     , bucket = buck) # Bucket name defined above



trial <- amball %>%
  drop_na(pctdelay60plus) %>%
  drop_na(pctdelay3060) %>%
  group_by(date2) %>%
  # mutate(mean60plus=mean(pctdelay60plus), mean3060=mean(pctdelay3060), n=n()) %>% 
  # mutate(mean30plus=mean60plus+mean60plus) %>% 
  # select(c(start_week,date2, year,mean60plus,mean3060,mean30plus, n)) %>% 
  # distinct() %>% 
  # mutate(week=str_sub(date2,6,8)) %>% 
  # ungroup() %>% 
  mutate(start_week_lab=format(as.Date(start_week), "%d %b %y")) %>% 
  mutate(monthyear=format(start_week, "%b %y")) 




trial %>% 
  mutate(num30plus=numdelays3060+numdelays60plus) %>% 
  mutate(pct30plus=num30plus/denom) %>% 
  ggplot(aes(x=date, y=pct30plus))+
  geom_line(group=1)+
  facet_grid(cols=vars(year), scales="free")+
  theme_minimal()


# Define the interval for displaying labels (e.g., every 10th label)
n <- 50

# Create a named vector for labels, setting some to NULL
label_vector <- unique(trial$date)
label_vector <- ifelse(seq_along(label_vector) %% n == 0, label_vector, "")




trial %>% 
  mutate(num30plus=numdelays3060+numdelays60plus) %>% 
  mutate(pct30plus=num30plus/denom) %>% 
  ggplot(.,aes(x=date, y=pct30plus, fill=year,  group=1, order=year))+
  geom_line()+
  scale_x_discrete(labels = label_vector) +
 # scale_x_yearmonth(breaks = '1 month',date_labels = "%b %g")+
 #scale_x_date(date_breaks = '1 month', date_labels ="%b %g")
  #scale_x_discrete(labels=unique(plot_data$monthyear))+
  # scale_x_date(
  #   date_breaks = '1 month', 
  #   date_labels = "%b %y",
  #   expand = c(0, 0)  # Ensure no extra space before or after the data range
  # ) +
  theme_THF()+
 # facet_grid(cols=vars(year), scales="free")+
  #scale_fill_manual(values=c('#53a9cd','#dd0031'))+
  labs(x = "", y="Handovers over 30 mins (%)", caption = "NHS England, Urgent and Emergency Care Daily Situation Reports")+
  theme(legend.text=element_text(size=11),
        legend.title = element_blank(),
        # axis.text.x = element_blank(),
        axis.text.x=element_text(size=11, angle=60),
        axis.text.y=element_text(size=11),
        plot.caption = element_markdown(hjust=0, size=9),
        plot.margin = unit(c(1,1.5,0.5,0.5), "cm"),
        legend.margin=margin(0,0,0,0),
        legend.box.margin=margin(-10,-10,-10,-10))

