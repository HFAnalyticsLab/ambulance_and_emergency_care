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

#Format data
#Response times 
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
  

#Number of incidents 
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

#all correlations are >0.34 suggest they are significant and suggests that there is a relationship between number of incidents and response times 
#There is a positive correlation between the number of incidents and response times for c1 and c2 types of incidents
#Whereas there is a negative correlation between number of incidents and response times for c3 and c4 type of incidents 


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








