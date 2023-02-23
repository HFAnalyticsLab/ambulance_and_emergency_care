##Data download

#Library 
library(curl)
library(here)

#Data Download

#Need to create a data folder in your working directory before this can work


# Ambulance Quality Indicators ---------------------------------------------

link <- 'https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2023/01/AmbSYS-for-December-2022.csv'

destfile <- here::here('data', "ambsys.csv")
curl_download(link, destfile = destfile)



# NHS staff sickness and absence rate -------------------------------------

link<-'https://files.digital.nhs.uk/0B/9C9B7F/ESR_ABSENCE_CSV_NHSE.csv'

destfile <- here::here('data', "eng_sickness.csv")
curl_download(link, destfile = destfile)


#June

link<-'https://files.digital.nhs.uk/39/163FFC/NHS%20Sickness%20Absence%20rates%20CSV%2C%20June%202022.csv'
destfile <- here::here('data', "eng_sickness_jun.csv")
curl_download(link, destfile = destfile)

#July

link<-'https://files.digital.nhs.uk/93/CE5B3B/NHS%20Sickness%20Absence%20rates%20CSV%2C%20July%202022.csv'
destfile <- here::here('data', "eng_sickness_jul.csv")
curl_download(link, destfile = destfile)


#August 

link<-'https://files.digital.nhs.uk/39/B64543/NHS%20Sickness%20Absence%20rates%20CSV%2C%20August%202022.csv'
destfile <- here::here('data', "eng_sickness_aug.csv")
curl_download(link, destfile = destfile)


# Ambulance handover delays -----------------------------------------------

#2017
link<-'https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2018/03/Winter-data-Timeseries-20180304.xlsx'

destfile <- here::here('data', "raw2017handovers.xlsx")
curl_download(link, destfile = destfile)

#2018
link<-'https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2019/03/Winter-data-timeseries-20190307.xlsx'

destfile <- here::here('data', "raw2018handovers.xlsx")
curl_download(link, destfile = destfile)

#2019
link<-'https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2020/03/Winter-SitRep-Acute-Time-series-2-December-2019-1-March-2020.xlsx'

destfile <- here::here('data', "raw2019handovers.xlsx")
curl_download(link, destfile = destfile)

#2020
link<-'https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2021/04/UEC-Daily-SitRep-Acute-Web-File-Timeseries-1.xlsx'

destfile <- here::here('data', "raw2020handovers.xlsx")
curl_download(link, destfile = destfile)

#2021
link<-'https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2022/04/UEC-Daily-SitRep-Web-File-Timeseries.xlsx'

destfile <- here::here('data', "raw2021handovers.xlsx")
curl_download(link, destfile = destfile)

#2022
link<-'https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2023/01/Ambulance-Collection-Web-File-Timeseries-1.xlsx'

destfile <- here::here('data', "raw2022handovers.xlsx")
curl_download(link, destfile = destfile)


# A&E admissions and waiting times ----------------------------------------


#Adjusted monthly time series 
link<-'https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2023/01/Adjusted-Monthly-AE-Time-Series-December-2022.xls'

destfile <- here::here('data', "aevol.xls")
curl_download(link, destfile = destfile)


# Bed Occupancy -----------------------------------------------------------

#Bed occupancy (Yearly)
link<-'https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2022/08/Beds-Timeseries-2010-11-onwards-Q1-2022-23-ADJ-for-missings-TRFGH.xls'

destfile <- here::here('data', "overnightbedoccup.xls")
curl_download(link, destfile = destfile)


#Overnight bed occupancy in months

link<-'https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2023/01/Beds-publication-Timeseries-March-2020-December-2022.xlsx'

destfile <- here::here('data', "bedoccup_montly.xlsx")
curl_download(link, destfile = destfile)


# Waiting times -----------------------------------------------------------


link<-'https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2023/01/RTT-Overview-Timeseries-Including-Estimates-for-Missing-Trusts-Nov22-XLS-98K-63230.xlsx'

destfile <- here::here('data', "waiting_times.xlsx")
curl_download(link, destfile = destfile)


