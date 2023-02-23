#Bed occupancy

rm(list=ls())
# Housekeeping ------------------------------------------------------------
#Library
library(readxl)
library(tidyverse)
library(janitor)

#Functions 



# Data load ---------------------------------------------------------------

bedoccup<-readxl::read_excel("data/overnightbedoccup.xls", sheet='Open Overnight')
bedoccup_monthly<-readxl::read_excel("data/bedoccup_montly.xlsx", sheet='Timeseries type 1 acute trusts')



# Formatting --------------------------------------------------------------

#Quarterly 
bedoccup<-bedoccup %>% 
  clean_names() %>% 
  slice(which(title=="Year"):n()) %>% 
  row_to_names(., 1) %>% 
  clean_names()

bedoccup_clean<-bedoccup %>% 
  clean_names() %>% 
  select(c(year, period,avail=general_acute, occup=general_acute_2)) %>% 
  mutate(avail=as.numeric(avail), 
         occup=as.numeric(occup),
         date=paste0(year," ", period),
         unoccup=avail-occup, 
         metric="Quarterly") %>% 
  filter(year %in% c("2017/18", "2018/19", "2019/20", "2020/21", "2021/22", "2022/23")) %>% 
  na.omit()
  
#write.csv(bedoccup_clean, 'bed_occup_quarters.csv')



#Monthly 

bedoccup_monthly<-bedoccup_monthly %>% 
  clean_names() %>% 
  slice(which(title=="Month"):n()) %>% 
  row_to_names(., 1) %>% 
  clean_names()

bedoccup_monthly_clean<-bedoccup_monthly %>% 
  clean_names() %>% 
  select(avail=g_a_beds_available,occup=g_a_beds_occupied, date=month) %>% 
  mutate(avail=as.numeric(avail), 
         occup=as.numeric(occup),
         unoccup=avail-occup, 
         metric="Monthly") %>% 
  mutate(date2=as.Date(paste0("01 ",date), format="%d %B %Y")) %>% 
  mutate(monthyear=format(as.Date(date2), "%b %y")) %>% 
  na.omit()

#write.csv(bedoccup_monthly_clean, 'bedoccup_monthly.csv')


# Save data ---------------------------------------------------------------
#Only saving monthly data 
#### save R objects from the environment directly to your s3 bucket
buck <- 'thf-dap-tier0-projects-iht-067208b7-projectbucket-1mrmynh0q7ljp/ambulance/clean' ## my bucket name

s3write_using(bedoccup_monthly_clean # What R object we are saving
              , FUN = write.csv # Which R function we are using to save
              , object = 'bedoccup_monthly_clean.csv' # Name of the file to save to (include file type)
              , bucket = buck) # Bucket name defined above

