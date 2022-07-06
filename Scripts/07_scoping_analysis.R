## Scoping Analysis
rm(list=ls())

#Library
library(data.table)
library(aws.s3)
library(readr)
library(tidyverse)
library(ggplot2)
library(THFstyle)
library(ggtext)
library(lubridate)
library(ggpubr)
library(tsibble)
library(Hmisc)
library(fpp2)
library(zoo)
library(scales)



#Methodology for correlations: https://www.r-bloggers.com/2020/03/testing-the-correlation-between-time-series-variables/


#functions

flattenCorrMatrix <- function(cormat, pmat) {
  ut <- upper.tri(cormat)
  data.frame(
    row = rownames(cormat)[row(cormat)[ut]],
    column = rownames(cormat)[col(cormat)[ut]],
    cor  =(cormat)[ut],
    p = pmat[ut]
  )
}


correlations_plot <-  function(data=df,var.x="c1"){
  aaa <- ggplot2::enquo(var.x)
  # bbb <- ggplot2::enquo(var.y)
  plot <-  data %>%
    filter(type==!!aaa) %>% 
    select(type,resp_time2,incidents) %>% 
    ggscatter(., x = "incidents", y = "resp_time2",
              add="reg.line",
              add.params = list(colour="red", fill="lightgray"),
              conf.int = TRUE,
              xlab = "Incidents", ylab = "Response Time")+
    stat_cor(method="spearman", label.x=3, label.y=3)+
    ggtitle(var.x)
  
  plot
}


# Data load ---------------------------------------------------------------
#Specify bucket 
buck<-'thf-dap-tier0-projects-iht-067208b7-projectbucket-1mrmynh0q7ljp/ambulance/clean'



#ambulance indicators 
amb_response_raw<-s3read_using(read.csv # Which function are we using to read
                      , object = 'amb_RT_regions.csv' # File to open
                      , bucket = buck) # Bucket name defined above

amb_incidents<-s3read_using(read.csv # Which function are we using to read
                            , object = 'amb_incidents.csv' # File to open
                            , bucket = buck) # Bucket name defined above

#A&E indicators 

aeattend<-s3read_using(readRDS # Which function are we using to read
                       , object = 'aeattend.rds' # File to open
                       , bucket = buck) # Bucket name defined above

#Bed occupancy

overnight_beds<-s3read_using(readRDS # Which function are we using to read
                             , object = 'England_overnightbeds.Rds' # File to open
                             , bucket = buck) # Bucket name defined above


#workforce indicators 
wf_eng<-s3read_using(read.csv # Which function are we using to read
                     , object = 'workforce_eng.csv' # File to open
                     , bucket = buck) # Bucket name defined above


turnover<-s3read_using(read.csv # Which function are we using to read
                       , object = 'turnover_clean.csv' # File to open
                       , bucket = buck) # Bucket name defined above

sick_ab<-s3read_using(read.csv # Which function are we using to read
                      , object = 'sick_ab_clean.csv' # File to open
                      , bucket = buck) # Bucket name defined above


# Response Times  ----------------------------------------------------------

amb_response<-amb_response_raw %>% 
  filter(org_code=="Eng") %>% 
  pivot_longer(c(c1_mean:c4_90thcent), names_to = 'metric', values_to = 'resp_time') %>% 
  mutate(resp_time=as.numeric(resp_time)) %>% 
  mutate(date=as.Date(date, format="%Y-%m-%d")) %>% 
  mutate(date2=yearmonth(date)) %>% 
  # mutate(resp_time2=as.POSIXct(as.numeric(resp_time),origin = "1970-01-01", tz="GMT")) %>% 
  # mutate(resp_time2=format(resp_time2, format="%H:%M:%S")) %>% 
  # mutate(resp_time2=as_hms(resp_time2)) %>% 
  pivot_wider(c("org_name", "date", "date2"),names_from=  metric, values_from = resp_time)


# correlation between response times  --------------------------------------

mean_corr <- rcorr(as.matrix(amb_dta), type="spearman")
mean_corr

t<-flattenCorrMatrix(mean_corr$r, mean_corr$P)


buck <- 'thf-dap-tier0-projects-iht-067208b7-resultsbucket-zzn273xwd1pg/ambulance' ## my bucket name

s3write_using(t # What R object we are saving
              , FUN = write.csv # Which R function we are using to save
              , object = 'response_times_corr.csv' # Name of the file to save to (include file type)
              , bucket = buck) # Bucket name defined above


# Rolling averages --------------------------------------------------------

amb_response_average<-amb_response %>% 
  mutate(c1_mean_rollmean=rollmean(c1_mean, k=12,fill=NA),
         c2_mean_rollmean=rollmean(c2_mean, k=12,fill=NA),
         c3_mean_rollmean=rollmean(c3_mean, k=12,fill=NA),
         c4_mean_rollmean=rollmean(c4_mean, k=12,fill=NA),
         c1_90thcent_rollmean=rollmean(c1_90thcent, k=12,fill=NA),
         c2_90thcent_rollmean=rollmean(c2_90thcent, k=12,fill=NA),
         c3_90thcent_rollmean=rollmean(c3_90thcent, k=12,fill=NA),
         c4_90thcent_rollmean=rollmean(c4_90thcent, k=12,fill=NA)) %>% 
  drop_na()

start_dates<-format(as.Date(seq(ymd('2017-08-01'),ymd('2021-05-01'),by='1 month')),"%Y-%m-%d")
end_dates<-format(as.Date(seq(ymd('2018-07-01'),ymd('2022-04-01'),by='1 month')),"%Y-%m-%d")

list_dates<-paste0(yearmonth(start_dates),"-",yearmonth(end_dates))
order<-c(1:46)

amb_response_average_plots<-cbind(amb_response_average,list_dates,order)

amb_response_average_plots<-amb_response_average_plots %>% 
  select(c(date, date2, list_dates, order, contains("rollmean"))) %>% 
  pivot_longer(c(c1_mean_rollmean:c4_90thcent_rollmean), names_to = 'metric', values_to = 'resp_time') %>% 
  mutate(resp_time=as.numeric(resp_time)) %>% 
  mutate(date=as.Date(date, format="%Y-%m-%d")) %>% 
  mutate(date2=yearmonth(date)) %>% 
  mutate(resp_time2=as.POSIXct(as.numeric(resp_time),origin = "1970-01-01", tz="GMT")) %>%
  mutate(resp_time2=format(resp_time2, format="%H:%M:%S")) %>%
  mutate(resp_time2=as_hms(resp_time2)) %>% 

amb_response_average_plots %>%
  mutate(met_group=substr(metric,0,2)) %>% 
  mutate(met_cat=ifelse(str_detect(metric,'cent'),'90th centile','Mean')) %>% 
  mutate(met_cat=factor(met_cat, levels=c('Mean', '90th centile'))) %>% 
  mutate(met_lab=ifelse(str_detect(metric, 'mean'), paste(met_group,"_Mean (hours:min:sec)")
                        ,paste(met_group,"_90th centile (hours:min:sec)"))) %>%
  mutate(name=fct_reorder(list_dates,order)) %>% 
  ggplot(.,aes(x=name, y=resp_time2, group=met_group, colour=met_group))+
  geom_line(aes(linetype=met_cat))+
  # geom_point(size=0.25)+
  # geom_hline(yintercept = as_hms("00:07:00"), colour = '#524c48', linetype='dashed' )+
  # geom_hline(yintercept = as_hms("00:15:00"), colour = '#524c48', linetype='dashed')+
  # scale_x_yearmonth( breaks = '6 months',date_labels = "%b %g")+
  # scale_y_time(limits = as_hms(c(0,72000)), breaks = as_hms(seq(0, 72000, by = 7200)))+
  scale_y_time(breaks = as_hms(seq(0, 72000, by = 1800)))+
  theme_THF()+
  facet_grid(cols=vars(met_cat))+
  scale_colour_THF()+
  labs(x = "", y="Response time (minutes)", caption = "NHS England, Ambulance Quality Indicators")+
  theme(legend.text=element_text(size=11),
        legend.title = element_blank(),
        axis.text.x=element_text(size=8, angle=90), 
        axis.text.y=element_text(size=11),
        plot.caption = element_markdown(hjust=0, size=9),
        plot.margin = unit(c(1,1.5,0.5,0.5), "cm"),
        legend.margin=margin(0,0,0,0),
        legend.box.margin=margin(-10,-10,-10,-10))



# Incidents ---------------------------------------------------------------

amb_incidents<-amb_incidents %>% 
  filter(org_code=="Eng") %>% 
  select(c(year:c4,date)) %>% 
  mutate(date=as.Date(date, format="%Y-%m-%d")) %>% 
  mutate(date2=yearmonth(date)) %>% 
  select(org_name:date2)

#Join together
amb_dta<-amb_response %>% 
  left_join(amb_incidents, by= c("org_name", "date2", "date"))


amb_dta<-amb_dta %>% 
  select(-c("org_name", "date2", "date"))

#correlations 
corr <- rcorr(as.matrix(amb_dta), type="spearman")
corr

t<-flattenCorrMatrix(corr$r, corr$P)

t<-t %>% 
  filter(!str_detect(column,"mean")) %>% 
  filter(!str_detect(column,"90"))


buck <- 'thf-dap-tier0-projects-iht-067208b7-resultsbucket-zzn273xwd1pg/ambulance' ## my bucket name

s3write_using(t # What R object we are saving
              , FUN = write.csv # Which R function we are using to save
              , object = 'incidents_corr.csv' # Name of the file to save to (include file type)
              , bucket = buck) # Bucket name defined above




# Rolling averages --------------------------------------------------------

roll_mean_incidents<-amb_incidents %>% 
  mutate(all_incidents_rollmean=rollmean(as.numeric(all_incidents), k=12,fill=NA),
         c1_rollmean=rollmean(as.numeric(c1), k=12,fill=NA),
         c2_rollmean=rollmean(as.numeric(c2), k=12,fill=NA),
         c3_rollmean=rollmean(as.numeric(c3), k=12,fill=NA),
         c4_rollmean=rollmean(as.numeric(c4), k=12,fill=NA)) %>% 
  drop_na()


start_dates<-format(as.Date(seq(ymd('2017-08-01'),ymd('2021-05-01'),by='1 month')),"%Y-%m-%d")
end_dates<-format(as.Date(seq(ymd('2018-07-01'),ymd('2022-04-01'),by='1 month')),"%Y-%m-%d")

list_dates<-paste0(yearmonth(start_dates),"-",yearmonth(end_dates))
order<-c(1:46)

amb_incidents_average_plots<-cbind(roll_mean_incidents,list_dates,order)

amb_incidents_average_plots<-amb_incidents_average_plots %>% 
  select(c(date, date2, list_dates, order, contains("rollmean"))) %>% 
  pivot_longer(c(c1_rollmean:c4_rollmean), names_to = 'metric', values_to = 'counts') %>% 
  mutate(met_group=substr(metric,0,2)) %>% 
  mutate(met_cat=ifelse(str_detect(metric,'cent'),'90th centile','Mean')) %>% 
  mutate(met_cat=factor(met_cat, levels=c('Mean', '90th centile'))) %>% 
  mutate(met_lab=ifelse(str_detect(metric, 'mean'), paste(met_group,"_Mean (hours:min:sec)")
                        ,paste(met_group,"_90th centile (hours:min:sec)"))) %>%
  mutate(name=fct_reorder(list_dates,order)) 


amb_incidents_average_plots %>% 
  ggplot(.,aes(x=name, y=counts, group=met_group, colour=met_group))+
  geom_line(linetype='solid')+
  scale_y_continuous(labels=comma)+
  theme_THF()+
  facet_grid(cols=vars(met_cat))+
  scale_colour_THF()+
  labs(x = "", y="Response time (minutes)", caption = "NHS England, Ambulance Quality Indicators")+
  theme(legend.text=element_text(size=11),
        legend.title = element_blank(),
        axis.text.x=element_text(size=8, angle=90), 
        axis.text.y=element_text(size=11),
        plot.caption = element_markdown(hjust=0, size=9),
        plot.margin = unit(c(1,1.5,0.5,0.5), "cm"),
        legend.margin=margin(0,0,0,0),
        legend.box.margin=margin(-10,-10,-10,-10))

# A&E attendances ----------------------------------------------------------

summary(aeattend)
colnames(aeattend)

aeattend <- aeattend %>%
  mutate(time=paste0(year, formatC(month, width=2, flag="0"))) %>%
  mutate(pct4to12hrsadmit=ifelse(totaladmit==0, 0, pct4to12hrsadmit )) %>%
  mutate(pct12plushrsadmit=ifelse(totaladmit==0, 0, pct12plushrsadmit )) 

summattend <- aeattend %>%
  drop_na(totalattend) %>%
  drop_na(`Number of A&E attendances Type 1`) %>%
  group_by(time) %>%
  summarise(meanattend=mean(totalattend), meantype1=mean(`Number of A&E attendances Type 1`),
            meantype2=mean(`Number of A&E attendances Type 2`), meantypeoth=mean(`Number of A&E attendances Other A&E Department`), n=n())

summattend<-summattend %>% 
  mutate(date=paste0(substr(time,0,4),"-",substr(time,5,6),"-01")) %>% 
  mutate(date=as.Date(date, format="%Y-%m-%d")) %>% 
  mutate(date2=yearmonth(date)) %>% 
  mutate(org_name="England")


amb_ae_dta<-amb_response %>% 
  left_join(summattend, by= c("org_name", "date2", "date")) %>% 
  drop_na()

amb_ae_dta<-amb_ae_dta %>% 
  select(-c("time", "org_name", "date2", "date",))

#correlations 
corr <- rcorr(as.matrix(amb_ae_dta), type="spearman")
corr

t<-flattenCorrMatrix(corr$r, corr$P)

t<-t %>% 
  filter(!str_detect(column,"c"))


buck <- 'thf-dap-tier0-projects-iht-067208b7-resultsbucket-zzn273xwd1pg/ambulance' ## my bucket name

s3write_using(t # What R object we are saving
              , FUN = write.csv # Which R function we are using to save
              , object = 'ae_attends_corr.csv' # Name of the file to save to (include file type)
              , bucket = buck) # Bucket name defined above


# A&E waiting times -------------------------------------------------------

summattend <- aeattend %>%
  drop_na(totaladmit) %>%
  drop_na(pct4to12hrsadmit) %>%
  drop_na(pct12plushrsadmit) %>%
  group_by(time) %>%
  summarise(meanadmit=mean(totaladmit), mean412admit=mean(pct4to12hrsadmit), mean12plusadmit=mean(pct12plushrsadmit), n=n())

summattend<-summattend %>% 
  mutate(date=paste0(substr(time,0,4),"-",substr(time,5,6),"-01")) %>% 
  mutate(date=as.Date(date, format="%Y-%m-%d")) %>% 
  mutate(date2=yearmonth(date)) %>% 
  mutate(org_name="England")

amb_ae_dta<-amb_response %>% 
  left_join(summattend, by= c("org_name", "date2", "date")) %>% 
  drop_na()


amb_ae_dta<-amb_response %>% 
  left_join(summattend, by= c("org_name", "date2", "date")) %>% 
  drop_na()

amb_ae_dta<-amb_ae_dta %>% 
  select(-c("time", "org_name", "date2", "date", "n"))

#correlations 
corr <- rcorr(as.matrix(amb_ae_dta), type="spearman")
corr

t<-flattenCorrMatrix(corr$r, corr$P)

t<-t %>% 
  filter(!str_detect(column,"c"))


buck <- 'thf-dap-tier0-projects-iht-067208b7-resultsbucket-zzn273xwd1pg/ambulance' ## my bucket name

s3write_using(t # What R object we are saving
              , FUN = write.csv # Which R function we are using to save
              , object = 'ae_wait_times_corr.csv' # Name of the file to save to (include file type)
              , bucket = buck) # Bucket name defined above

# waiting times by type ---------------------------------------------------

summattend <- aeattend %>%
  drop_na(pct4plushrswaittype1) %>%
  drop_na(pct4plushrswaittype2) %>%
  group_by(time) %>%
  summarise(mean4pluswaittype1=mean(pct4plushrswaittype1),
            mean4pluswaittype2=mean(pct4plushrswaittype2), n=n())


summattend<-summattend %>% 
  mutate(date=paste0(substr(time,0,4),"-",substr(time,5,6),"-01")) %>% 
  mutate(date=as.Date(date, format="%Y-%m-%d")) %>% 
  mutate(date2=yearmonth(date)) %>% 
  mutate(org_name="England")


amb_ae_dta<-amb_response %>% 
  left_join(summattend, by= c("org_name", "date2", "date")) %>% 
  drop_na()


amb_ae_dta<-amb_ae_dta %>% 
  select(-c("time", "org_name", "date2", "date", "n"))

#correlations 
corr <- rcorr(as.matrix(amb_ae_dta), type="spearman")
corr

t<-flattenCorrMatrix(corr$r, corr$P)

t<-t %>% 
  filter(!str_detect(column,"c"))


buck <- 'thf-dap-tier0-projects-iht-067208b7-resultsbucket-zzn273xwd1pg/ambulance' ## my bucket name

s3write_using(t # What R object we are saving
              , FUN = write.csv # Which R function we are using to save
              , object = 'ae_wait_times_type_corr.csv' # Name of the file to save to (include file type)
              , bucket = buck) # Bucket name defined above


# Overnight bed occupancy -------------------------------------------------

eng_overnightbeds <- overnight_beds %>%
  mutate(time=paste0(Year,Period)) %>%
  mutate(pctoccuptot=as.numeric(Total...14)) %>%
  mutate(pctoccupgenacute=as.numeric(`General & Acute...15`)) %>% 
  select(Year:Period,time:pctoccupgenacute) %>% 
  mutate(year2=ifelse(Period %in% c("Q3", "Q4"),paste0(20,substr(Year,6,7)),substr(Year,0,4))) %>% 
  mutate(time=paste0(Period,"-",year2))
  
  
amb_response<-amb_response %>% 
  mutate(period=as.numeric(substr(date,6,7))) %>% 
  mutate(quart= case_when(period>0 &period<4~ "Q4", 
                          period>3 & period <7 ~ "Q1",
                          period>6& period<10~ "Q2", 
                          period>9& period<13~ "Q3")) %>% 
  mutate(time=paste0(quart,"-",substr(date,0,4))) %>% 
  group_by(time) %>% 
  summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE))) 


amb_bed_dta<-amb_response %>% 
  left_join(eng_overnightbeds, by= c("time")) %>% 
  drop_na()

amb_bed_dta<-amb_bed_dta %>% 
  select(-c("time","year2", "period", "Period", "Year"))


#correlations 
corr <- rcorr(as.matrix(amb_bed_dta), type="spearman")
corr

t<-flattenCorrMatrix(corr$r, corr$P)

t<-t %>% 
  filter(!str_detect(column,"c"))


buck <- 'thf-dap-tier0-projects-iht-067208b7-resultsbucket-zzn273xwd1pg/ambulance' ## my bucket name

s3write_using(t # What R object we are saving
              , FUN = write.csv # Which R function we are using to save
              , object = 'bed_occupancy_corr.csv' # Name of the file to save to (include file type)
              , bucket = buck) # Bucket name defined above




# fte count ---------------------------------------------------------------

list_dates<-format(as.Date(seq(ymd('2017-08-31'),ymd('2022-02-08'),by='4 weeks')),"%Y-%m")

wf_eng_amb<-wf_eng %>% 
    mutate(date=as.Date(date, format="%Y-%m-%d")) %>%
    mutate(date2=yearmonth(date)) %>%
  filter(str_detect(group,"Ambulance")& met=="fte_count") %>% 
  pivot_wider(c(date, date2), names_from = group, values_from=val) %>% 
  clean_names() %>% 
  mutate(total=ambulance_staff+support_to_ambulance_staff)

amb_wf_dta<-amb_response %>% 
  select(-c(date, org_name)) %>% 
  left_join(wf_eng_amb, by= c("date2")) %>% 
  drop_na()

#correlations 
corr <- rcorr(as.matrix(amb_wf_dta[,c(2:11,15)]), type="spearman")
corr

t<-flattenCorrMatrix(corr$r, corr$P)

t<-t %>% 
  filter(column=="total")

buck <- 'thf-dap-tier0-projects-iht-067208b7-resultsbucket-zzn273xwd1pg/ambulance' ## my bucket name

s3write_using(t # What R object we are saving
              , FUN = write.csv # Which R function we are using to save
              , object = 'fte_total_count_corr.csv' # Name of the file to save to (include file type)
              , bucket = buck) # Bucket name defined above

#To include the types

wf_eng_amb<-wf_eng_amb %>% 
  filter(date2> yearmonth(as.Date("2019-04-01", format="%Y-%m-%d")))

amb_wf_dta<-amb_response %>% 
  select(-c(date, org_name)) %>% 
  left_join(wf_eng_amb, by= c("date2")) %>% 
  drop_na()

corr <- rcorr(as.matrix(amb_wf_dta[,c(2:11,13:14)]), type="spearman")
corr

t<-flattenCorrMatrix(corr$r, corr$P)

t<-t %>% 
  filter(str_detect(column, "ambulance"))

buck <- 'thf-dap-tier0-projects-iht-067208b7-resultsbucket-zzn273xwd1pg/ambulance' ## my bucket name

s3write_using(t # What R object we are saving
              , FUN = write.csv # Which R function we are using to save
              , object = 'fte_by_count_corr.csv' # Name of the file to save to (include file type)
              , bucket = buck) # Bucket name defined above


# Turnover rate -----------------------------------------------------------


#data is every 3 months but for a year [not sure if this is feasible ]
turnover_clean<-turnover %>% 
  select(staff_group, stability_index, period, year_end) %>% 
  mutate(date=as.Date(paste0(year_end,"-",period,"-01"), format="%Y-%m-%d")) %>%
  mutate(date2=yearmonth(date)) %>% 
  pivot_wider(c(date, date2), names_from=staff_group, values_from=stability_index) %>% 
  clean_names() %>% 
  mutate(amb_total=ambulance_staff+support_to_ambulance_staff)



amb_response_turnover<-amb_response %>% 
  



amb_turnover_dta<-amb_response %>% 
  select(-c(org_name, date)) %>% 
  left_join(turnover_clean, by= c("date2")) %>% 
  drop_na() %>% 
  select(-date2)


corr <- rcorr(as.matrix(), type="spearman")
corr

t<-flattenCorrMatrix(corr$r, corr$P)

t<-t %>% 
  filter(str_detect(column, "ambulance"))





# Staff sickness and absence rate -----------------------------------------

sick_ab_clean<-sick_ab %>% 
  select(-X) %>% 
  mutate(date=as.Date(date2, format="%Y-%m-%d")) %>%
  mutate(date2=yearmonth(date)) %>% 
  filter(org_type=="Ambulance")
  

amb_sickab_dta<-amb_response %>% 
  select(-c(org_name, date)) %>% 
  left_join(sick_ab_clean, by= c("date2")) %>% 
  drop_na() %>% 
  select(-c(date, date2, org_type))


corr <- rcorr(as.matrix(amb_sickab_dta), type="spearman")
corr

t<-flattenCorrMatrix(corr$r, corr$P)

t<-t %>% 
  filter(str_detect(column, "sa_rate"))

buck <- 'thf-dap-tier0-projects-iht-067208b7-resultsbucket-zzn273xwd1pg/ambulance' ## my bucket name

s3write_using(t # What R object we are saving
              , FUN = write.csv # Which R function we are using to save
              , object = 'sa_rate_corr.csv' # Name of the file to save to (include file type)
              , bucket = buck) # Bucket name defined above

