#Workforce data 

rm(list=ls())

#Library
library(tidyverse)
library(here)
library(aws.s3)
library(janitor)
library(readxl)
library(dplyr)
library(tsibble)

#data load 
workforce_eng<-read_excel(here::here('data', "eng_workforce.xlsx"), sheet=2, skip=6)

workforce_eng_clean<-workforce_eng %>% 
  clean_names() %>% 
  fill(england, .direction='down') %>% 
  mutate(x2=ifelse(is.na(england), "date", x2)) %>% 
  filter(x2 %in% c("Ambulance staff", "date", "Total", "Support to ambulance staff")) %>% 
  mutate(england=ifelse(is.na(england),"date",england)) %>% 
  unite(met,england:x2, remove=FALSE) %>%
  t() %>%
  row_to_names(row_number=1) %>% 
  as.data.frame() %>% 
  clean_names() %>% 
  pivot_longer(c(headcount_total:full_time_equivalent_fte_support_to_ambulance_staff), names_to='metric', values_to='val') %>% 
  mutate(group=ifelse(str_detect(metric, 'total'),"England", 
                      ifelse(str_detect(metric, "support"),"Support to Ambulance staff", "Ambulance staff"))) %>% 
  mutate(met=ifelse(str_detect(metric,'headcount'),"headcount", "fte_count")) %>% 
  mutate(date=as.Date(as.numeric(date_date), origin = "1899-12-30")) %>% 
  mutate(date=lubridate::date(date)) %>% 
  mutate(val=as.numeric(val)) %>% 
  filter(!is.na(date)) %>% 
  clean_names() %>% 
  select(c(date,group,met,val))

#save workforce data 
#### save R objects from the environment directly to your s3 bucket
buck <- 'thf-dap-tier0-projects-iht-067208b7-projectbucket-1mrmynh0q7ljp/ambulance/clean' ## my bucket name

s3write_using(workforce_eng_clean # What R object we are saving
              , FUN = write.csv # Which R function we are using to save
              , object = 'workforce_eng.csv' # Name of the file to save to (include file type)
              , bucket = buck) # Bucket name defined above


#Staff sickness and absence rate 
sick_ab<-read_csv(here::here('data', "eng_sickness.csv"))

sick_ab_all<-sick_ab %>% 
  clean_names() %>% 
  mutate_if(is.numeric, ~replace(., is.na(.), 0)) %>% 
  mutate(org_type_new=case_when(org_type=="England"~ "England",
                                org_type=="Ambulance"~ "Ambulance", 
                                org_type=="Acute"~"Acute",
                                org_type=="Mental Health"~ "Mental Health",
                                TRUE ~ "Other")) %>% 
  group_by(date, org_type_new) %>% 
  summarise(across(where(is.numeric), sum)) %>% 
  select(-c(sort_date, sa_rate_percent)) %>% 
  mutate(sa_rate=round((fte_days_sick/fte_days_available)*100,2)) %>% 
  mutate(date2=as.Date(paste0(date,"-01"), format="%Y-%b-%d"))


eng_sick_ab<-sick_ab %>%
  clean_names() %>% 
  mutate_if(is.numeric, ~replace(., is.na(.), 0)) %>% 
  group_by(date) %>%
  summarise(across(where(is.numeric), sum)) %>% 
  select(-c(sort_date, sa_rate_percent)) %>% 
  mutate(sa_rate=round((fte_days_sick/fte_days_available)*100,2)) %>% 
  mutate(date2=as.Date(paste0(date,"-01"), format="%Y-%b-%d")) %>% 
  arrange(date2) %>% 
  mutate(org_type_new= "England")

sick_ab_clean<-rbind(eng_sick_ab, sick_ab_all)

sick_ab_clean<-sick_ab_clean %>% 
  arrange(date2)


buck <- 'thf-dap-tier0-projects-iht-067208b7-projectbucket-1mrmynh0q7ljp/ambulance/clean' ## my bucket name

s3write_using(sick_ab_clean # What R object we are saving
              , FUN = write.csv # Which R function we are using to save
              , object = 'sick_ab_clean.csv' # Name of the file to save to (include file type)
              , bucket = buck) # Bucket name defined above








