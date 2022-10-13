##Data download

#Library 
library(curl)
library(here)
library(aws.s3)


#Data Download

#Need to create a data folder in your working directory before this can work


# Ambulance Quality Indicators ---------------------------------------------

link <- 'https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2022/10/AmbSYS-to-Sep22.csv'

destfile <- here::here('data', "ambsys.csv")
curl_download(link, destfile = destfile)



# NHS staff sickness and absence rate -------------------------------------

link<-'https://files.digital.nhs.uk/0B/9C9B7F/ESR_ABSENCE_CSV_NHSE.csv'

destfile <- here::here('data', "eng_sickness.csv")
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


# A&E admissions and waiting times ----------------------------------------


#Adjusted monthly time series 
link<-'https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2022/10/Adjusted-Monthly-AE-Time-Series-September-2022.xls'

destfile <- here::here('data', "aevol.xls")
curl_download(link, destfile = destfile)


<<<<<<< HEAD
# Bed Occupancy -----------------------------------------------------------

#Bed occupancy (Yearly)
link<-'https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2022/08/Beds-Timeseries-2010-11-onwards-Q1-2022-23-ADJ-for-missings-TRFGH.xls'

destfile <- here::here('data', "overnightbedoccup.xls")
curl_download(link, destfile = destfile)


=======
#Overnight bed occupancy in months

link<-'https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2022/10/Beds-publication-Timeseries-March-2020-September-2022.xlsx'

destfile <- here::here('data', "bedoccup_montly.xlsx")
curl_download(link, destfile = destfile)

>>>>>>> d921ea1b35e97e32838757215935f929ff027309
