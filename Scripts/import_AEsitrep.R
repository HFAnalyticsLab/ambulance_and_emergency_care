library(aws.s3)
library("readxl")
library(tidyverse)

headers <- as.character(read_excel("ambulance/UEC-Daily-SitRep-Acute-Web-File-Timeseries-1.xlsx", sheet=9, n_max=1, skip=14, col_names=FALSE))
ambdelay <- read_excel("ambulance/UEC-Daily-SitRep-Acute-Web-File-Timeseries-1.xlsx", sheet=9, skip=18, col_names=headers)
ambdelay <- subset(ambdelay, `NHS England Region`=="East of England" | `NHS England Region`=="London" | `NHS England Region`== "Midlands" | `NHS England Region`== "North East and Yorkshire"
                   | `NHS England Region`== "North West" | `NHS England Region`== "South East" | `NHS England Region`== "South West")
table(ambdelay$`NHS England Region`)

ambdelay60 <- ambdelay[ , grep("Delay >60", names( ambdelay))]  # keep columns for delays >60 mins
ambdelay3060 <- ambdelay[ , grep("Delay 30-60", names( ambdelay))]  # keep columns for delays 30-60 mins
ambdenom <- ambdelay[ , grep("Arriving by", names( ambdelay))]
geog <- subset(ambdelay, select=c(`NHS England Region`, Code, Name))

test1 <- bind_cols(geog, ambdenom)
test2 <- bind_cols(geog, ambdelay60)
test3 <- bind_cols(geog, ambdelay3060)

# now need to convert wide to long 
longtest1 <- test1 %>%
  pivot_longer(cols=starts_with("Arriving by ambulance..."), names_to="day", names_prefix="Arriving by ambulance...", values_to="denom", values_drop_na=FALSE)
longtest2 <- test2 %>%
  pivot_longer(cols=starts_with("Delay >60 mins..."), names_to="day", names_prefix="Delay >60 mins...", values_to="numdelays60plus", values_drop_na=FALSE)
longtest3 <- test3 %>%
  pivot_longer(cols=starts_with("Delay 30-60 mins..."), names_to="day", names_prefix="Delay 30-60 mins...", values_to="numdelays3060", values_drop_na=FALSE)

amball <- merge(longtest1,longtest2, by=c("Code", "Name", "day","NHS England Region"))
amball <- merge(amball,longtest3, by=c("Code", "Name", "day","NHS England Region"))
amball <- amball %>%
  mutate(day=as.numeric(day)) %>%
  mutate(denom=as.numeric(denom)) %>%
  mutate(numdelays60plus=as.numeric(numdelays60plus)) %>%
  mutate(numdelays3060=as.numeric(numdelays3060)) %>%
  mutate(pctdelay60plus=numdelays60plus*100/denom) %>%
  mutate(pctdelay60plus=na_if(pctdelay60plus, Inf)) %>%
  mutate(pctdelay3060=numdelays3060*100/denom) %>%
  mutate(pctdelay3060=na_if(pctdelay3060, Inf))
  

saveRDS(amball, file="ambulance/amball.rds")
