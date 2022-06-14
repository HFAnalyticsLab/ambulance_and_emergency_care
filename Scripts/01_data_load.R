##Data download

#Libraries 
library(curl)
library(tidyverse)
library(data.table)
library(here)
library(aws.s3)
library(janitor)

#data download


#Ambulance Quality Indicators 

link <- 'https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2022/05/AmbSYS-for-2022-Apr.csv'

destfile <- here::here('data', "ambsys.csv")
curl_download(link, destfile = destfile)



#NHS workforce statistics 

link<-'https://files.digital.nhs.uk/95/096385/NHS%20Workforce%20Statistics%2C%20February%202022%20England%20and%20Organisation.xlsx'

destfile <- here::here('data', "eng_workforce.xlsx")
curl_download(link, destfile = destfile)




 
