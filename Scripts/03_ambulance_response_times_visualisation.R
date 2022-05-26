library(data.table)
library(aws.s3)
library(readr)
library(tidyverse)
library(rio)
library(lubridate)
library(ggplot2)

#Data load

buck<-'thf-dap-tier0-projects-iht-067208b7-projectbucket-1mrmynh0q7ljp/ambulance/clean'


amb_dta<-s3read_using(read.csv # Which function are we using to read
                   , object = 'amb_RT_clean.csv' # File to open
                   , bucket = buck) # Bucket name defined above

#Convert vars into time variables
vars<-c(paste0("c1_",c("mean", "90thcent")),c(paste0("c1T_",c("mean", "90thcent"))),
c(paste0("c2_",c("mean", "90thcent"))),c(paste0("c3_",c("mean", "90thcent"))),
c(paste0("c4_",c("mean", "90thcent"))))

amb_dta_plot<-amb_dta %>% 
  mutate_at(vars,as.numeric)




try$c1_90thcent_lab <- as.POSIXct(as.numeric(amb_dta$c1_90thcent), origin = "1970-01-01", tz = "GMT") %>%
  format("%H:%M:%S")
try$c1_90thcent_lab2 <- as.duration(amb_dta_plot$c1_90thcent)


try<-amb_dta[,c(1:8,17,18)]



Y.minT <- as.POSIXct("06:00:00" , format = "%H:%M:%S")
Y.maxT <- as.POSIXct("12:00:00" , format = "%H:%M:%S")
Y.min <- as.numeric(Y.minT)
Y.max <- as.numeric(Y.maxT)

nums <- seq(from = Y.min, to = Y.max, length.out = 1+abs(2 * difftime(Y.maxT, Y.minT)[[1]]))

#Create a vector of date labels
# labels <- as.POSIXct(nums, origin = "1970-01-01", tz = "GMT") %>% format("%H:%M")


try %>% 
  ggplot(.,aes(x=date, y=as.numeric(c1_90thcent_lab), group=region, colour=region))+
  geom_line()+
  geom_point()+
  
  
  
  # scale_y_continuous(breaks = nums, labels = labels, limits = c(Y.min, Y.max))+ 
  # scale_x_date(breaks = seq( min(try$date), max(try$date),by="8 weeks"),
  #            date_labels = '%d %b %g')

