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


#Specify bucket 
buck<-'thf-dap-tier0-projects-iht-067208b7-projectbucket-1mrmynh0q7ljp/ambulance/clean'


#data load
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

bedoccup<-s3read_using(readRDS # Which function are we using to read
                       , object = 'bedoccup.rds' # File to open
                       , bucket = buck) # Bucket name defined above

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
  pivot_longer(c(c1_mean:c4_90thcent), names_to = 'metric', values_to = 'resp_time')

amb_response<-amb_response %>% 
  mutate(resp_time2=as.POSIXct(as.numeric(resp_time),origin = "1970-01-01", tz="GMT")) %>% 
  mutate(resp_time2=format(resp_time2, format="%H:%M:%S")) %>% 
  mutate(resp_time2=as_hms(resp_time2)) %>% 
  mutate(date=as.Date(date, format="%Y-%m-%d")) %>% 
  mutate(date2=yearmonth(date)) %>% 
  select(c(org_name:date2)) %>% 
  mutate(type=ifelse(str_detect(substr(metric,0,3),"T"),substr(metric,0,3),substr(metric,0,2))) 



# correlation between response times --------------------------------------

#Mean response times 
amb_dta<-amb_response %>%
  filter(str_detect(metric, "90th")) %>% 
  pivot_wider(c("org_name", "date"),names_from=  metric, values_from = resp_time2)

df_c1_c1t<-amb_dta %>% 
  mutate(
    rank_c1_resp=rank(c1_mean),
    rank_c1T_resp=rank(c1T_mean),
    d=rank_c1_resp-rank_c1T_resp, 
    d_squared=d^2,
    d_square_sum=sum(d_squared),
    n=n(),
    rho_s=round((1-(6*(d_square_sum))/(n*(n^2-1))),2))

df_c1_c1t %>% 
  distinct(rho_s)

df_c1_c2<-amb_dta %>% 
  mutate(
    rank_c1_resp=rank(c1_mean),
    rank_c2_resp=rank(c2_mean),
    d=rank_c1_resp-rank_c2_resp, 
    d_squared=d^2,
    d_square_sum=sum(d_squared),
    n=n(),
    rho_s=round((1-(6*(d_square_sum))/(n*(n^2-1))),2))

df_c1_c2 %>% 
  distinct(rho_s)

df_c1_c3<-amb_dta %>% 
  mutate(
    rank_c1_resp=rank(c1_mean),
    rank_c3_resp=rank(c3_mean),
    d=rank_c1_resp-rank_c3_resp, 
    d_squared=d^2,
    d_square_sum=sum(d_squared),
    n=n(),
    rho_s=round((1-(6*(d_square_sum))/(n*(n^2-1))),2))


df_c1_c3 %>% 
  distinct(rho_s)

df_c1_c4<-amb_dta %>% 
  mutate(
    rank_c1_resp=rank(c1_mean),
    rank_c4_resp=rank(c4_mean),
    d=rank_c1_resp-rank_c4_resp, 
    d_squared=d^2,
    d_square_sum=sum(d_squared),
    n=n(),
    rho_s=round((1-(6*(d_square_sum))/(n*(n^2-1))),2))


df_c1_c4 %>% 
  distinct(rho_s)



x<-amb_dta %>% 
  mutate(
    rank_c1t_resp=rank(c1T_mean),
    rank_c4_resp=rank(c4_mean),
    d=rank_c1t_resp-rank_c4_resp, 
    d_squared=d^2,
    d_square_sum=sum(d_squared),
    n=n(),
    rho_s=round((1-(6*(d_square_sum))/(n*(n^2-1))),2))

x %>% 
  distinct(rho_s)

# Incidents ---------------------------------------------------------------


amb_incidents<-amb_incidents %>% 
  filter(org_code=="Eng") %>% 
  select(c(year:c4,date)) %>% 
  pivot_longer(c(all_incidents:c4), names_to = 'type', values_to = 'incidents') %>% 
  mutate(date=as.Date(date, format="%Y-%m-%d")) %>% 
  mutate(date2=yearmonth(date)) %>% 
  select(org_name:date2)

#Join together
amb_dta<-amb_response %>% 
  left_join(amb_incidents, by= c("org_name", "date2", "date", "type"))





# Mean response times and Incidents ---------------------------------------

df<- amb_dta %>% 
  filter(!str_detect(type,"T")) %>% 
  filter(!str_detect(metric,"90th")) %>% 
  group_by(type) %>% 
  mutate(
  incidents=as.numeric(incidents),
  rank_resp=rank(resp_time2),
  rank_incidents=rank(incidents),
  d=rank_resp-rank_incidents, 
  d_squared=d^2,
  d_square_sum=sum(d_squared),
  n=n(),
  rho_s=round((1-(6*(d_square_sum))/(n*(n^2-1))),2))


df %>% 
  distinct(rho_s)

#all correlations are >0.34 suggest they are significant and suggests that there is a relationship between number of incidents and mean response times 
#There is a positive correlation between the number of incidents and mean response times for c1 and c2 types of incidents
#Whereas there is a negative correlation between number of incidents and mean response times for c3 and c4 type of incidents 


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
 
correlations_plot(var.x="c1")
correlations_plot(var.x="c2")
correlations_plot(var.x="c3")
correlations_plot(var.x="c4")


amb_dta_c1<-amb_dta %>% 
  filter(type=="c1") %>% 
  select(type,resp_time2,incidents) %>% 
  mutate(incidents=as.numeric(incidents))

acf(amb_dta_c1$resp_time2, amb_dta_c1$incidents)


acf(amb_dta_c1$resp_time2)



# 90th centile response times and incidents  ------------------------------


df<- amb_dta %>% 
  filter(!str_detect(type,"T")) %>% 
  filter(str_detect(metric,"90th")) %>% 
  group_by(type) %>% 
  mutate(
    incidents=as.numeric(incidents),
    rank_resp=rank(resp_time2),
    rank_incidents=rank(incidents),
    d=rank_resp-rank_incidents, 
    d_squared=d^2,
    d_square_sum=sum(d_squared),
    n=n(),
    rho_s=round((1-(6*(d_square_sum))/(n*(n^2-1))),2))


df %>% 
  distinct(rho_s)

#same pattern as above, positive correlation for number of incidents and 90th percentile response times for c1 and c2
#whereas negative correlation for number of incidents and 90th centile response times for c3 and c4 

correlations_plot(var.x="c1")
correlations_plot(var.x="c2")
correlations_plot(var.x="c3")
correlations_plot(var.x="c4")



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


#Correlation between mean attendances

df<-amb_ae_dta %>% 
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
