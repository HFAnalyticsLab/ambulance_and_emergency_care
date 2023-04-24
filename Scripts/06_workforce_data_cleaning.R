#Workforce data 


# Housekeeping ------------------------------------------------------------
rm(list=ls())

#Library
library(readxl)
library(tidyverse)
library(here)
library(aws.s3)
library(janitor)
library(dplyr)


# Load data ---------------------------------------------------------------
sick_ab<-read_csv(here::here('data', "eng_sickness.csv"))
sick_ab_jun<-read_csv(here::here('data', "eng_sickness_jun.csv"))
sick_ab_jul<-read_csv(here::here('data', "eng_sickness_jul.csv"))
sick_ab_aug<-read_csv(here::here('data', "eng_sickness_aug.csv"))
sick_ab_sep<-read_csv(here::here('data', "eng_sickness_sep.csv"))
sick_ab_oct<-read_csv(here::here('data', "eng_sickness_oct.csv"))
sick_ab_nov<-read_csv(here::here('data', "eng_sickness_nov.csv"))

sick_ab_new<-rbind(sick_ab_jun, sick_ab_jul, sick_ab_aug[,c(1:3,6:11)], sick_ab_sep[,c(1:3,6:11)], sick_ab_oct[,c(1:3,6:11)], sick_ab_nov[,c(1:3, 6:11)])

sick_ab_new<-sick_ab_new %>% 
  clean_names() %>% 
  rename(fte_days_sick=fte_days_lost,sa_rate_percent=sickness_absence_rate_percent) %>% 
  mutate(date=format(as.Date(sick_ab_new$DATE, format= "%d/%m/%Y"),"%Y-%b"))


sick_ab<-sick_ab %>% 
  clean_names() %>% 
  mutate(date=str_to_title(str_squish(sick_ab$Date))) %>% 
  select(-sort_date) 

sick_ab_all<-sick_ab %>% 
  bind_rows(sick_ab_new) %>% 
  mutate_if(is.numeric, ~replace(., is.na(.), 0)) %>% 
  mutate(org_type_new=case_when(org_type=="Ambulance"~ "Ambulance", 
                                org_type=="Acute"~"Acute",
                                org_type=="Mental Health"~ "Mental Health",
                                TRUE ~ "Other")) %>%
  group_by(date, org_type_new) %>% 
  summarise(across(where(is.numeric), sum)) %>% 
  select(-sa_rate_percent) %>% 
  mutate(sa_rate=round((fte_days_sick/fte_days_available)*100,2)) %>% 
  mutate(date2=as.Date(paste0(date,"-01"), format="%Y-%b-%d")) %>% 
  arrange(date2)


eng_sick_ab<-sick_ab %>%
  clean_names() %>% 
  mutate_if(is.numeric, ~replace(., is.na(.), 0)) %>% 
  group_by(date) %>%
  summarise(across(where(is.numeric), sum)) %>% 
  select(-sa_rate_percent) %>% 
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








