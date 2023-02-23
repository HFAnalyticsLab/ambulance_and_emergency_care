#Library
library(aws.s3)
library(readxl)
library(tidyverse)
library(lubridate)
library(stringr)
library(ISOweek)


rm(list=ls())

wt<-read_excel("data/waiting_times.xlsx")


wt2<-wt %>% 
  clean_names() %>% 
  slice(which(title=="Year"):n()) %>% 
  row_to_names(., 2) %>% 
  clean_names() %>% 
  rename(year=na, month=na_2) %>% 
  fill(year) %>% 
  select(year,month,no_within_18_weeks_with_estimates_for_missing_data,no_18_weeks_with_estimates_for_missing_data,
         no_52_weeks_with_estimates_for_missing_data) 

wt2[3:5] = lapply(wt2[3:5], FUN = function(y){as.numeric(y)})

wt2<-wt2 %>% 
  drop_na() %>% 
  mutate(tot_wt=no_within_18_weeks_with_estimates_for_missing_data+no_18_weeks_with_estimates_for_missing_data) %>% 
  mutate(date=as.Date(as.numeric(month), origin = "1899-12-30")) %>% 
  mutate(monthyear=format(as.Date(date), "%b %y"))



  


