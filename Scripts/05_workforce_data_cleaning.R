#Workforce data 

rm(list=ls())

#Library
library(tidyverse)
library(here)
library(aws.s3)
library(janitor)
library(readxl)
library(dplyr)

#data load 
workforce_eng<-read_excel(here::here('data', "eng_workforce.xlsx"), sheet=2, skip=6)

workforce_eng<-workforce_eng %>% 
  clean_names() %>% 
  fill(england, .direction='down') %>% 
  mutate(x2=ifelse(is.na(england), "date", x2)) %>% 
  filter(x2 %in% c("Ambulance staff", "date")) %>% 
  mutate(england=ifelse(is.na(england),"date",england)) %>% 
  select(-x2) %>% 
  t() %>% 
  row_to_names(row_number=1) %>% 
  as.data.frame() 

workforce_eng_clean<-workforce_eng %>% 
  mutate(date=as.Date(as.numeric(date), origin = "1899-12-30")) %>% 
  mutate(date=lubridate::date(date)) %>% 
  filter(!is.na(date)) %>% 
  clean_names() 

#save workforce data 
#### save R objects from the environment directly to your s3 bucket
buck <- 'thf-dap-tier0-projects-iht-067208b7-projectbucket-1mrmynh0q7ljp/ambulance/clean' ## my bucket name

s3write_using(workforce_eng_clean # What R object we are saving
              , FUN = write.csv # Which R function we are using to save
              , object = 'workforce_eng.csv' # Name of the file to save to (include file type)
              , bucket = buck) # Bucket name defined above




