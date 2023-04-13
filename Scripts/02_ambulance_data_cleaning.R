#Ambulance data cleaning 


# Housekeeping ------------------------------------------------------------

rm(list=ls())

#Library
library(tidyverse)
library(here)
library(aws.s3)
library(janitor)

#Functions
`%notin%` <- Negate(`%in%`)

# Load data ---------------------------------------------------------------
amb_dta<-read_csv(here::here('data', "ambsys.csv"))


# Clean Data --------------------------------------------------------------


# Ambulance Response times ------------------------------------------------

#Select relevant columns 
amb_dta_clean<-amb_dta %>% 
  clean_names() %>% 
  select(year:org_name, paste0("a",c(25:26, 28:29, 31:32,34:35, 37:38)))

#Region codes
list_org_codes_region<-c("Y63", "Y62","Y60", "Y61", "Y56", "Y59", "Y58")
#N.B. some data for some months are not complete 

#Rename columns 
names(amb_dta_clean)[6:7]<-c(paste0("c1_",c("mean", "90thcent")))
names(amb_dta_clean)[8:9]<-c(paste0("c1T_",c("mean", "90thcent")))
names(amb_dta_clean)[10:11]<-c(paste0("c2_",c("mean", "90thcent")))
names(amb_dta_clean)[12:13]<-c(paste0("c3_",c("mean", "90thcent")))
names(amb_dta_clean)[14:15]<-c(paste0("c4_",c("mean", "90thcent")))

#Add date variable
amb_dta_regions<-amb_dta_clean %>% 
  filter(org_code %in% c(list_org_codes_region)) %>% 
  mutate(date=as.Date(paste0(year,"/",ifelse (month<10, paste0(0,month),month),"/",01))) %>% 
  mutate(month=format(date,"%m")) %>% 
  mutate(year=format(date, "%Y"))

#Make columns numeric
amb_dta_regions[6:15]=lapply(amb_dta_regions[6:15], FUN = function(y){gsub(",","",y)})
amb_dta_regions[6:15] = lapply(amb_dta_regions[6:15], FUN = function(y){as.numeric(y)})




# calculate response times ------------------------------------------------


#Select relevant columns 
amb_dta_clean_eng<-amb_dta %>% 
  clean_names() %>% 
  select(year:org_name, paste0("a",c(8,10:12, 24,30,33,36, 26,32,35,38)))

#Region codes
list_org_codes_region<-c("Y63", "Y62","Y60", "Y61", "Y56", "Y59", "Y58")

#Rename columns 
names(amb_dta_clean_eng)[c(6,10)]<-c(paste0("c1_",c("incidents", "RT")))
names(amb_dta_clean_eng)[c(7,11)]<-c(paste0("c2_",c("incidents", "RT")))
names(amb_dta_clean_eng)[c(8,12)]<-c(paste0("c3_",c("incidents", "RT")))
names(amb_dta_clean_eng)[c(9,13)]<-c(paste0("c4_",c("incidents", "RT")))
names(amb_dta_clean_eng)[c(14:17)]<-c(paste0(c("c1","c2", "c3", "c4"),"_90RT"))


amb_dta_clean_eng<-amb_dta_clean_eng %>% 
  filter(org_code %in% c(list_org_codes_region)) %>% 
  mutate(date=as.Date(paste0(year,"/",ifelse (month<10, paste0(0,month),month),"/",01))) 


#Make columns numeric
amb_dta_clean_eng[6:17]=lapply(amb_dta_clean_eng[6:17], FUN = function(y){gsub(",","",y)})
amb_dta_clean_eng[6:17] = lapply(amb_dta_clean_eng[6:17], FUN = function(y){as.numeric(y)})

london<-amb_dta_clean_eng %>% 
  filter(region=="London") %>% 
  filter(date %in% c(as.Date("2022-10-01"),as.Date("2022-11-01")))


eng<-amb_dta_clean_eng %>% 
  mutate(c1_incidents=ifelse(region=="London"& date %in% c(as.Date("2022-10-01"),as.Date("2022-11-01")), NA,c1_incidents),
         c2_incidents=ifelse(region=="London"& date %in% c(as.Date("2022-10-01"),as.Date("2022-11-01")), NA,c2_incidents),
        c3_incidents=ifelse(region=="London"& date %in% c(as.Date("2022-10-01"),as.Date("2022-11-01")), NA,c3_incidents),
        c4_incidents=ifelse(region=="London"& date %in% c(as.Date("2022-10-01"),as.Date("2022-11-01")), NA,c4_incidents)) %>% 
  group_by(date) %>%
  mutate(c1_prod=c1_incidents*c1_90RT, 
         c2_prod=c2_incidents*c2_90RT, 
         c3_prod=c3_incidents*c3_90RT, 
         c4_prod=c4_incidents*c4_90RT) %>% 
  summarise(across(where(is.numeric), sum, na.rm=TRUE)) %>% 
  mutate(c1_mean=round(c1_RT/c1_incidents), 
         c2_mean=round(c2_RT/c2_incidents), 
         c3_mean=round(c3_RT/c3_incidents),
         c4_mean=round(c4_RT/c4_incidents)) %>% 
  mutate(c1_90thcent=round(c1_prod/c1_incidents),
         c2_90thcent=round(c2_prod/c2_incidents),
         c3_90thcent=round(c3_prod/c3_incidents),
         c4_90thcent=round(c4_prod/c4_incidents)) %>% 
  mutate(org_code="Eng") %>% 
  mutate(region="England") %>% 
  mutate(org_name="England") %>% 
  mutate(year=format(date, "%Y")) %>% 
  mutate(month=format(date,"%m")) %>% 
  select(-contains(c("RT", "incidents", "prod")))
  
amb_dta_regions<-amb_dta_regions %>% 
  full_join(eng)
  

#Save Data
#### save R objects from the environment directly to your s3 bucket
buck <- 'thf-dap-tier0-projects-iht-067208b7-projectbucket-1mrmynh0q7ljp/ambulance/clean' ## my bucket name

s3write_using(amb_dta_regions # What R object we are saving
              , FUN = write.csv # Which R function we are using to save
              , object = 'amb_RT_regions.csv' # Name of the file to save to (include file type)
              , bucket = buck) # Bucket name defined above




# Number of incidents ---------------------------------------------------------------

#Select relevant columns 
amb_dta_clean<-amb_dta %>% 
  clean_names() %>% 
  select(year:org_name, paste0("a",c(7:12, 17, 53:55)))

#Rename columns
names(amb_dta_clean)[6:11]<-c("all_incidents", "c1", "c1t", "c2", "c3", "c4")
names(amb_dta_clean)[12:15]<-c("hear_treat", "convey_ED", "convey_elsewhere", "see_treat")

#Add date variable
amb_incidents<-amb_dta_clean %>% 
  mutate(date=as.Date(paste0(year,"/",ifelse (month<10, paste0(0,month),month),"/",01))) %>% 
  filter(date %notin% c(as.Date("2017/08/01"),as.Date("2017/09/01")))

#Save incidents data 
#### save R objects from the environment directly to your s3 bucket
buck <- 'thf-dap-tier0-projects-iht-067208b7-projectbucket-1mrmynh0q7ljp/ambulance/clean' ## my bucket name

s3write_using(amb_incidents # What R object we are saving
              , FUN = write.csv # Which R function we are using to save
              , object = 'amb_incidents.csv' # Name of the file to save to (include file type)
              , bucket = buck) # Bucket name defined above

# Number of Calls -------------------------------------------------------------------

#Select relevant columns
amb_dta_clean<-amb_dta %>% 
  clean_names() %>% 
  select(year:org_name, paste0("a",c(0:6, 114)))

#Rename columns
names(amb_dta_clean)[6:13]<-c("contact_count", "calls_answered", "answered_times_total", "answered_times_mean", 
                              "answered_times_median", "answered_times_95","answered_times_99", "answered_times_90")

#Add date variable
amb_calls<-amb_dta_clean %>% 
  mutate(date=as.Date(paste0(year,"/",ifelse (month<10, paste0(0,month),month),"/",01))) 

#### save R objects from the environment directly to your s3 bucket
buck <- 'thf-dap-tier0-projects-iht-067208b7-projectbucket-1mrmynh0q7ljp/ambulance/clean' ## my bucket name

s3write_using(amb_calls # What R object we are saving
              , FUN = write.csv # Which R function we are using to save
              , object = 'amb_calls.csv' # Name of the file to save to (include file type)
              , bucket = buck) # Bucket name defined above


