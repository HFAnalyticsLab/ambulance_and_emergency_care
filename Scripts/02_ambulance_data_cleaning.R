#Data cleaning 

#Libraries
library(tidyverse)
library(here)
library(aws.s3)
library(janitor)



#load data 
amb_dta<-read_csv(here::here('data', "ambsys.csv"))

#select relevant columns 
amb_dta_clean<-amb_dta %>% 
  clean_names() %>% 
  select(year:org_name, paste0("a",c(25:26, 28:29, 31:32,34:35, 37:38)))

#by trust 
list_org_codes_trust<-c("RX9", "RYC", "R1F", "RRU", "RX6", "RX7", "RYE", "RYD", "RYF", "RYA", "RX8") 
#some data is no complete

#by region
list_org_codes_region<-c("Y63", "Y62","Y60", "Y61", "Y56", "Y59", "Y58")

amb_dta_clean<-amb_dta_clean %>% 
  filter(org_code %in% list_org_codes_region) %>% 
  mutate(date=as.Date(paste0(year,"/",ifelse (month<10, paste0(0,month),month),"/",01))) 

        
names(amb_dta_clean)[6:7]<-c(paste0("c1_",c("mean", "90thcent")))
names(amb_dta_clean)[8:9]<-c(paste0("c1T_",c("mean", "90thcent")))
names(amb_dta_clean)[10:11]<-c(paste0("c2_",c("mean", "90thcent")))
names(amb_dta_clean)[12:13]<-c(paste0("c3_",c("mean", "90thcent")))
names(amb_dta_clean)[14:15]<-c(paste0("c4_",c("mean", "90thcent")))



#save data
#### save R objects from the environment directly to your s3 bucket
buck <- 'thf-dap-tier0-projects-iht-067208b7-projectbucket-1mrmynh0q7ljp/ambulance/clean' ## my bucket name

s3write_using(amb_dta_clean # What R object we are saving
              , FUN = write.csv # Which R function we are using to save
              , object = 'amb_RT_clean.csv' # Name of the file to save to (include file type)
              , bucket = buck) # Bucket name defined above
