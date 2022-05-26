##Data download

#Libraries 
library(curl)
library(tidyverse)
library(data.table)
library(here)
library(aws.s3)
library(janitor)

#data download

link <- 'https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2022/05/AmbSYS-for-2022-Apr.csv'

destfile <- here::here('data', "ambsys.csv")
curl_download(link, destfile = destfile)








 
