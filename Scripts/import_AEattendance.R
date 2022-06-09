library(aws.s3)
library("readxl")
library(tidyverse)
library(data.table)  


aeattend <- list.files(path = "Data",  
                       pattern = "*.csv", full.names = TRUE) %>% 
  lapply(read_csv) %>%            
  bind_rows                       


aeattend <- aeattend %>%
  mutate(`NHS England Region`=`Parent Org`)

aeattend$`NHS England Region`[aeattend$`Parent Org` == "NHS ENGLAND EAST OF ENGLAND"] <- "East of England"
aeattend$`NHS England Region`[aeattend$`Parent Org` == "NHS ENGLAND EAST OF ENGLAND (EAST)"] <- "East of England" 
aeattend$`NHS England Region`[aeattend$`Parent Org` == "NHS ENGLAND MIDLANDS AND EAST (EAST)"] <- "East of England" 
aeattend$`NHS England Region`[aeattend$`Parent Org` == "NHS ENGLAND LONDON"] <- "London" 
aeattend$`NHS England Region`[aeattend$`Parent Org` == "NHS ENGLAND MIDLANDS"] <- "Midlands" 
aeattend$`NHS England Region`[aeattend$`Parent Org` == "NHS ENGLAND MIDLANDS (CENTRAL MIDLANDS)"] <- "Midlands" 
aeattend$`NHS England Region`[aeattend$`Parent Org` == "NHS ENGLAND MIDLANDS (NORTH MIDLANDS)"] <- "Midlands"
aeattend$`NHS England Region`[aeattend$`Parent Org` == "NHS ENGLAND MIDLANDS (WEST MIDLANDS)"] <- "Midlands"
aeattend$`NHS England Region`[aeattend$`Parent Org` == "NHS ENGLAND MIDLANDS AND EAST (CENTRAL MIDLANDS)"] <- "Midlands"
aeattend$`NHS England Region`[aeattend$`Parent Org` == "NHS ENGLAND MIDLANDS AND EAST (NORTH MIDLANDS)"] <- "Midlands" 
aeattend$`NHS England Region`[aeattend$`Parent Org` == "NHS ENGLAND MIDLANDS AND EAST (WEST MIDLANDS)"] <- "Midlands" 
aeattend$`NHS England Region`[aeattend$`Parent Org` == "NHS ENGLAND NORTH WEST"] <- "North West" 
aeattend$`NHS England Region`[aeattend$`Parent Org` == "NHS ENGLAND NORTH WEST (GREATER MANCHESTER)"] <- "North West"
aeattend$`NHS England Region`[aeattend$`Parent Org` == "NHS ENGLAND NORTH (GREATER MANCHESTER)"] <- "North West"
aeattend$`NHS England Region`[aeattend$`Parent Org` == "NHS ENGLAND NORTH WEST (LANCASHIRE AND SOUTH CUMBRIA)"] <- "North West"
aeattend$`NHS England Region`[aeattend$`Parent Org` == "NHS ENGLAND NORTH (LANCASHIRE AND SOUTH CUMBRIA)"] <- "North West"
aeattend$`NHS England Region`[aeattend$`Parent Org` == "NHS ENGLAND NORTH (CHESHIRE AND MERSEYSIDE)"] <- "North West"
aeattend$`NHS England Region`[aeattend$`Parent Org` == "NHS ENGLAND NORTH WEST (CHESHIRE AND MERSEYSIDE)"] <- "North West"
aeattend$`NHS England Region`[aeattend$`Parent Org` == "NHS ENGLAND NORTH (CUMBRIA AND NORTH EAST)"] <- "North East and Yorkshire"
aeattend$`NHS England Region`[aeattend$`Parent Org` == "NHS ENGLAND NORTH (YORKSHIRE AND HUMBER)"] <- "North East and Yorkshire"
aeattend$`NHS England Region`[aeattend$`Parent Org` == "NHS ENGLAND NORTH EAST AND YORKSHIRE (YORKSHIRE AND HUMBER)"] <- "North East and Yorkshire"
aeattend$`NHS England Region`[aeattend$`Parent Org` == "NHS ENGLAND NORTH EAST AND YORKSHIRE"] <- "North East and Yorkshire"
aeattend$`NHS England Region`[aeattend$`Parent Org` == "NHS ENGLAND NORTH EAST AND YORKSHIRE (CUMBRIA AND NORTH EAST)"] <- "North East and Yorkshire"
aeattend$`NHS England Region`[aeattend$`Parent Org` == "NHS ENGLAND SOUTH EAST (KENT, SURREY AND SUSSEX)"] <- "South East"
aeattend$`NHS England Region`[aeattend$`Parent Org` == "NHS ENGLAND SOUTH EAST"] <- "South East"
aeattend$`NHS England Region`[aeattend$`Parent Org` == "NHS ENGLAND SOUTH EAST (HAMPSHIRE, ISLE OF WIGHT AND THAMES VALLEY)"] <- "South East" 
aeattend$`NHS England Region`[aeattend$`Parent Org` == "NHS ENGLAND SOUTH WEST"] <- "South West"
aeattend$`NHS England Region`[aeattend$`Parent Org` == "NHS ENGLAND SOUTH WEST (SOUTH WEST SOUTH)"] <- "South West"
aeattend$`NHS England Region`[aeattend$`Parent Org` == "NHS ENGLAND SOUTH WEST (SOUTH WEST NORTH)"] <- "South West" 

aeattend <- subset(aeattend, `NHS England Region`=="East of England" | `NHS England Region`=="London" | `NHS England Region`== "Midlands" | `NHS England Region`== "North East and Yorkshire"
                   | `NHS England Region`== "North West" | `NHS England Region`== "South East" | `NHS England Region`== "South West")

table(aeattend$`NHS England Region`)

aeattend %>% 
  group_by(`NHS England Region`, Period) %>%
  tally() %>%
  spread(`NHS England Region`, n)

## Create date from Period
aeattend <- aeattend %>%
  mutate(year=str_sub(Period, start=-4)) %>%
  mutate(year=as.numeric(year)) %>% 
  mutate(monthyr=str_sub(Period, start=8)) %>%
  mutate(monthtxt=str_sub(monthyr, start=1, -6)) %>%
  mutate(month=ifelse(monthtxt=="APRIL" | monthtxt=="April", 4, 
                          ifelse(monthtxt=="MAY" | monthtxt=="May", 5, 
                             ifelse(monthtxt=="JUNE"| monthtxt=="June", 6, 
                                    ifelse(monthtxt=="JULY" | monthtxt=="July", 7,
                                           ifelse(monthtxt=="AUGUST"| monthtxt=="August", 8,
                                                  ifelse(monthtxt=="SEPTEMBER"| monthtxt=="September", 9,
                                                         ifelse(monthtxt=="OCTOBER"| monthtxt=="October", 10,
                                                                ifelse(monthtxt=="NOVEMBER"| monthtxt=="November", 11,
                                                                       ifelse(monthtxt=="DECEMBER"| monthtxt=="December", 12,
                                                                              ifelse(monthtxt=="JANUARY"| monthtxt=="January", 1,
                                                                                     ifelse(monthtxt=="FEBRUARY"| monthtxt=="February", 2, 
                                                                                            ifelse(monthtxt=="MARCH"| monthtxt=="March", 3, 0))))))))))))) %>%
  mutate(`Number of A&E attendances Type 1`=ifelse(is.na(`Number of A&E attendances Type 1`), `A&E attendances Type 1`, `Number of A&E attendances Type 1`)) %>%
  mutate(`Number of A&E attendances Type 2`=ifelse(is.na(`Number of A&E attendances Type 2`), `A&E attendances Type 2`, `Number of A&E attendances Type 2`)) %>%
  mutate(`Number of A&E attendances Other A&E Department`=ifelse(is.na(`Number of A&E attendances Other A&E Department`), `A&E attendances Other A&E Department`, `Number of A&E attendances Other A&E Department`)) %>%
  mutate(`Number of attendances over 4hrs Type 1`=ifelse(is.na(`Number of attendances over 4hrs Type 1`), `Attendances over 4hrs Type 1`, `Number of attendances over 4hrs Type 1`)) %>%
  mutate(`Number of attendances over 4hrs Type 2`=ifelse(is.na(`Number of attendances over 4hrs Type 2`), `Attendances over 4hrs Type 2`, `Number of attendances over 4hrs Type 2`)) %>%
  mutate(`Number of attendances over 4hrs Other A&E Department`=ifelse(is.na(`Number of attendances over 4hrs Other A&E Department`), `Attendances over 4hrs Other Department`, `Number of attendances over 4hrs Other A&E Department`)) %>%
  mutate(totalattend=`Number of A&E attendances Type 1`+`Number of A&E attendances Type 2`+`Number of A&E attendances Other A&E Department`) %>%
  mutate(totaladmit=`Emergency admissions via A&E - Type 1` + `Emergency admissions via A&E - Type 2` + `Emergency admissions via A&E - Other A&E department`, `Other emergency admissions`) %>%
  mutate(pct4to12hrsadmit=`Patients who have waited 4-12 hs from DTA to admission`*100/totaladmit) %>%
  mutate(pct12plushrsadmit=`Patients who have waited 12+ hrs from DTA to admission`*100/totaladmit) %>%
  mutate(pct4plushrswaittype1=`Number of attendances over 4hrs Type 1`*100/`Number of A&E attendances Type 1`) %>%
  mutate(pct4plushrswaittype2=`Number of attendances over 4hrs Type 2`*100/`Number of A&E attendances Type 2`)
  
aeattend <- subset(aeattend, select=-c(`A&E attendances Type 1`, `A&E attendances Type 2`,`A&E attendances Other A&E Department`, `Attendances over 4hrs Type 1`,  `Attendances over 4hrs Type 2`, `Attendances over 4hrs Other Department`))
viewdata <- aeattend[with(aeattend, order(`NHS England Region`, year, month)), ]

saveRDS(aeattend, file="aeattend.rds")
