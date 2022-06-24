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


#Methodology: https://www.r-bloggers.com/2020/03/testing-the-correlation-between-time-series-variables/


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
amb_response<-s3read_using(read.csv # Which function are we using to read
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

amb_response<-amb_response %>% 
  filter(org_code=="Eng") %>% 
  pivot_longer(c(c1_mean:c4_90thcent), names_to = 'metric', values_to = 'resp_time') %>% 
  mutate(resp_time=as.numeric(resp_time)) %>% 
  mutate(date=as.Date(date, format="%Y-%m-%d")) %>% 
  mutate(date2=yearmonth(date)) %>% 
  pivot_wider(c("org_name", "date", "date2"),names_from=  metric, values_from = resp_time)


# correlation between response times (need to update this- 23/6/22) --------------------------------------

amb_dta<-amb_response %>%
  select(contains ("mean"))

mean_corr <- rcorr(as.matrix(amb_dta), type="spearman")
mean_corr



t<-flattenCorrMatrix(mean_corr$r, mean_corr$P)


buck <- 'thf-dap-tier0-projects-iht-067208b7-resultsbucket-zzn273xwd1pg/ambulance' ## my bucket name

s3write_using(t # What R object we are saving
              , FUN = write.csv # Which R function we are using to save
              , object = 'response_times_corr.csv' # Name of the file to save to (include file type)
              , bucket = buck) # Bucket name defined above

#90th percentile
amb_dta<-amb_response %>%
  filter(str_detect(metric, "90")) %>% 
  pivot_wider(c("org_name", "date"),names_from=  metric, values_from = resp_time) %>% 
  select(-c("org_name", "date"))

percent_corr <- rcorr(as.matrix(amb_dta), type="spearman")
percent_corr

t<-flattenCorrMatrix(percent_corr$r, percent_corr$P)


buck <- 'thf-dap-tier0-projects-iht-067208b7-resultsbucket-zzn273xwd1pg/ambulance' ## my bucket name

s3write_using(t # What R object we are saving
              , FUN = write.csv # Which R function we are using to save
              , object = 'response_times_90thpercent_corr.csv' # Name of the file to save to (include file type)
              , bucket = buck) # Bucket name defined above


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


# A&E attendances ----------------------------------------------------------

summary(aeattend)
colnames(aeattend)

aeattend <- aeattend %>%
  mutate(time=paste0(year, formatC(month, width=2, flag="0"))) %>%
  mutate(pct4to12hrsadmit=ifelse(totaladmit==0, 0, pct4to12hrsadmit )) %>%
  mutate(pct12plushrsadmit=ifelse(totaladmit==0, 0, pct12plushrsadmit )) 

nax<-aeattend %>% 
    filter_all(any_vars(is.na(.)))



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


#Correlation between mean attendances

df<-amb_ae_dta %>% 
  filter(str_detect(metric,"mean")) %>% 
  group_by(type) %>% 
  mutate(
    rank_resp=rank(resp_time2),
    rank_meanattend=rank(meanattend),
    d=rank_resp-rank_meanattend, 
    d_squared=d^2,
    d_square_sum=sum(d_squared),
    n=n(),
    rho_s=round((1-(6*(d_square_sum))/(n*(n^2-1))),2))

df %>% 
  distinct(rho_s)

#type 1 attendance
df<-amb_ae_dta %>% 
  filter(str_detect(metric,"mean")) %>% 
  group_by(type) %>% 
  mutate(
    rank_resp=rank(resp_time2),
    rank_meantype1=rank(meantype1),
    d=rank_resp-rank_meantype1, 
    d_squared=d^2,
    d_square_sum=sum(d_squared),
    n=n(),
    rho_s=round((1-(6*(d_square_sum))/(n*(n^2-1))),2))

df %>% 
  distinct(rho_s)

#type 2 attendance


df<-amb_ae_dta %>% 
  filter(str_detect(metric,"mean")) %>% 
  group_by(type) %>% 
  mutate(
    rank_resp=rank(resp_time2),
    rank_meantype2=rank(meantype2),
    d=rank_resp-rank_meantype2, 
    d_squared=d^2,
    d_square_sum=sum(d_squared),
    n=n(),
    rho_s=round((1-(6*(d_square_sum))/(n*(n^2-1))),2))

df %>% 
  distinct(rho_s)

#correlation between 90th percentile

#Mean attendance 
df<-amb_ae_dta %>% 
  filter(str_detect(metric,"90")) %>% 
  group_by(type) %>% 
  mutate(
    rank_resp=rank(resp_time2),
    rank_meanattend=rank(meanattend),
    d=rank_resp-rank_meanattend, 
    d_squared=d^2,
    d_square_sum=sum(d_squared),
    n=n(),
    rho_s=round((1-(6*(d_square_sum))/(n*(n^2-1))),2))

df %>% 
  distinct(rho_s)

#type 1 attendance
df<-amb_ae_dta %>% 
  filter(str_detect(metric,"90")) %>% 
  group_by(type) %>% 
  mutate(
    rank_resp=rank(resp_time2),
    rank_meantype1=rank(meantype1),
    d=rank_resp-rank_meantype1, 
    d_squared=d^2,
    d_square_sum=sum(d_squared),
    n=n(),
    rho_s=round((1-(6*(d_square_sum))/(n*(n^2-1))),2))

df %>% 
  distinct(rho_s)


#type 2 attendance


df<-amb_ae_dta %>% 
  filter(str_detect(metric,"90")) %>% 
  group_by(type) %>% 
  mutate(
    rank_resp=rank(resp_time2),
    rank_meantype2=rank(meantype2),
    d=rank_resp-rank_meantype2, 
    d_squared=d^2,
    d_square_sum=sum(d_squared),
    n=n(),
    rho_s=round((1-(6*(d_square_sum))/(n*(n^2-1))),2))

df %>% 
  distinct(rho_s)


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

#Correlation between mean attendances

#total admit

df<-amb_ae_dta %>% 
  filter(str_detect(metric,"mean")) %>% 
  group_by(type) %>% 
  mutate(
    rank_resp=rank(resp_time2),
    rank_x=rank(meanadmit),
    d=rank_resp-rank_x, 
    d_squared=d^2,
    d_square_sum=sum(d_squared),
    n=n(),
    rho_s=round((1-(6*(d_square_sum))/(n*(n^2-1))),2))

df %>% 
  distinct(rho_s)




#4-12 hrs 
df<-amb_ae_dta %>% 
  filter(str_detect(metric,"mean")) %>% 
  group_by(type) %>% 
  mutate(
    rank_resp=rank(resp_time2),
    rank_x=rank(mean412admit),
    d=rank_resp-rank_x, 
    d_squared=d^2,
    d_square_sum=sum(d_squared),
    n=n(),
    rho_s=round((1-(6*(d_square_sum))/(n*(n^2-1))),2))

df %>% 
  distinct(rho_s)

#+12 hrs 

df<-amb_ae_dta %>% 
  filter(str_detect(metric,"mean")) %>% 
  group_by(type) %>% 
  mutate(
    rank_resp=rank(resp_time2),
    rank_x=rank(mean12plusadmit),
    d=rank_resp-rank_x, 
    d_squared=d^2,
    d_square_sum=sum(d_squared),
    n=n(),
    rho_s=round((1-(6*(d_square_sum))/(n*(n^2-1))),2))

df %>% 
  distinct(rho_s)



#90th percentile

#total admit

df<-amb_ae_dta %>% 
  filter(str_detect(metric,"90")) %>% 
  group_by(type) %>% 
  mutate(
    rank_resp=rank(resp_time2),
    rank_x=rank(meanadmit),
    d=rank_resp-rank_x, 
    d_squared=d^2,
    d_square_sum=sum(d_squared),
    n=n(),
    rho_s=round((1-(6*(d_square_sum))/(n*(n^2-1))),2))

df %>% 
  distinct(rho_s)


#4-12 hrs
df<-amb_ae_dta %>% 
  filter(str_detect(metric,"90")) %>% 
  group_by(type) %>% 
  mutate(
    rank_resp=rank(resp_time2),
    rank_x=rank(mean412admit),
    d=rank_resp-rank_x, 
    d_squared=d^2,
    d_square_sum=sum(d_squared),
    n=n(),
    rho_s=round((1-(6*(d_square_sum))/(n*(n^2-1))),2))

df %>% 
  distinct(rho_s)

#12+hrs

df<-amb_ae_dta %>% 
  filter(str_detect(metric,"90")) %>% 
  group_by(type) %>% 
  mutate(
    rank_resp=rank(resp_time2),
    rank_x=rank(mean12plusadmit),
    d=rank_resp-rank_x, 
    d_squared=d^2,
    d_square_sum=sum(d_squared),
    n=n(),
    rho_s=round((1-(6*(d_square_sum))/(n*(n^2-1))),2))

df %>% 
  distinct(rho_s)


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


#Correlation between mean attendances
#4+ type 1 
df<-amb_ae_dta %>% 
  filter(str_detect(metric,"mean")) %>% 
  group_by(type) %>% 
  mutate(
    rank_resp=rank(resp_time2),
    rank_x=rank(mean4pluswaittype1),
    d=rank_resp-rank_x, 
    d_squared=d^2,
    d_square_sum=sum(d_squared),
    n=n(),
    rho_s=round((1-(6*(d_square_sum))/(n*(n^2-1))),2))

df %>% 
  distinct(rho_s)

#4+type2
df<-amb_ae_dta %>% 
  filter(str_detect(metric,"mean")) %>% 
  group_by(type) %>% 
  mutate(
    rank_resp=rank(resp_time2),
    rank_x=rank(mean4pluswaittype2),
    d=rank_resp-rank_x, 
    d_squared=d^2,
    d_square_sum=sum(d_squared),
    n=n(),
    rho_s=round((1-(6*(d_square_sum))/(n*(n^2-1))),2))

df %>% 
  distinct(rho_s)

#90th percentile

#4+ type 1 
df<-amb_ae_dta %>% 
  filter(str_detect(metric,"90")) %>% 
  group_by(type) %>% 
  mutate(
    rank_resp=rank(resp_time2),
    rank_x=rank(mean4pluswaittype1),
    d=rank_resp-rank_x, 
    d_squared=d^2,
    d_square_sum=sum(d_squared),
    n=n(),
    rho_s=round((1-(6*(d_square_sum))/(n*(n^2-1))),2))

df %>% 
  distinct(rho_s)

#4+type2
df<-amb_ae_dta %>% 
  filter(str_detect(metric,"90")) %>% 
  group_by(type) %>% 
  mutate(
    rank_resp=rank(resp_time2),
    rank_x=rank(mean4pluswaittype2),
    d=rank_resp-rank_x, 
    d_squared=d^2,
    d_square_sum=sum(d_squared),
    n=n(),
    rho_s=round((1-(6*(d_square_sum))/(n*(n^2-1))),2))

df %>% 
  distinct(rho_s)


# Overnight bed occupancy -------------------------------------------------

eng_overnightbeds <- overnight_beds %>%
  mutate(time=paste0(Year,Period)) %>%
  mutate(pctoccuptot=as.numeric(Total...14)) %>%
  mutate(pctoccupgenacute=as.numeric(`General & Acute...15`)) %>% 
  select(Year,time:pctoccupgenacute) %>% 
  mutate(year_start=str_sub(eng_overnightbeds$Year,0,4)) %>% 
  mutate(year_end=paste0("20",str_sub(eng_overnightbeds$Year,6,7)))

# %>% 
#   mutate(year_start2=yearmonth(paste0(year_start,"-",period))) %>% 
#   mutate(year_end2=yearmonth(paste0(year_end,"-",period)))

# mutate(date2=paste0(year_end,"-",ifelse(period==12,period,paste0(0,period)),"-",ifelse(period %in% c(9,6),30,31))) %>% 
#   mutate(date=as.Date(date2, origin = "1899-12-30")) %>% 
#   mutate(date=lubridate::date(date))   


