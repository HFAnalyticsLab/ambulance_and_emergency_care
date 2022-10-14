#Bed occupancy

# Housekeeping ------------------------------------------------------------
#Library
library(readxl)
library(tidyverse)
library(janitor)

#Functions 



# Data load ---------------------------------------------------------------
#N.B. Need to update the range to include latest data 
bedoccup<-readxl::read_excel("data/overnightbedoccup.xls", sheet='Open Overnight', range="B14:O63")
bedoccup_monthly<-readxl::read_excel("data/bedoccup_montly.xlsx", sheet='Timeseries type 1 acute trusts', range="B13:D44")



# Formatting --------------------------------------------------------------

bedoccup_clean<-bedoccup %>% 
  clean_names() %>% 
  select(c(year, period,avail=general_acute_5, occup=general_acute_11)) %>% 
  mutate(date=paste0(year," ", period),
         unoccup=avail-occup, 
         metric="Quarterly") %>% 
  filter(year %in% c("2017/18", "2018/19", "2019/20", "2020/21", "2021/22", "2022/23"))
  
#write.csv(bedoccup_clean, 'bed_occup_quarters.csv')

bedoccup_monthly_clean<-bedoccup_monthly %>% 
  clean_names() %>% 
  rename(avail=g_a_beds_available,occup=g_a_beds_occupied, date=month) %>% 
  mutate(unoccup=avail-occup, 
         metric="Monthly") %>% 
  mutate(date2=as.Date(paste0("01 ",date), format="%d %B %Y")) %>% 
  mutate(monthyear=format(as.Date(date2), "%b %y")) 

#write.csv(bedoccup_monthly_clean, 'bedoccup_monthly.csv')


#Combining monthly with quarterly data 

bed_occupancy<-bedoccup_clean %>% 
  full_join(bedoccup_monthly_clean)

#write.csv(bed_occupancy, 'bed_occupancy_full.csv')

# Save data ---------------------------------------------------------------
#Only saving monthly data 
#### save R objects from the environment directly to your s3 bucket
buck <- 'thf-dap-tier0-projects-iht-067208b7-projectbucket-1mrmynh0q7ljp/ambulance/clean' ## my bucket name

s3write_using(bedoccup_monthly_clean # What R object we are saving
              , FUN = write.csv # Which R function we are using to save
              , object = 'bedoccup_monthly_clean.csv' # Name of the file to save to (include file type)
              , bucket = buck) # Bucket name defined above

