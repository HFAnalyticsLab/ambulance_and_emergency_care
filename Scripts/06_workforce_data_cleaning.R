#Workforce data 


# Housekeeping ------------------------------------------------------------
rm(list=ls())

#Library
library(tidyverse)
library(here)
library(aws.s3)
library(janitor)
library(readxl)
library(dplyr)


# Load data ---------------------------------------------------------------
sick_ab<-read_csv(here::here('data', "eng_sickness.csv"))

sick_ab_all<-sick_ab %>% 
  clean_names() %>% 
  mutate_if(is.numeric, ~replace(., is.na(.), 0)) %>% 
  mutate(org_type_new=case_when(org_type=="Ambulance"~ "Ambulance", 
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








