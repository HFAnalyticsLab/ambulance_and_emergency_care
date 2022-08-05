
library(aws.s3)
library("readxl")
library(tidyverse)
library(lubridate)

aevolume <- read_excel("Data/AEvolume.xlsx")
aewait <- read_excel("Data/AEwait4plus.xlsx")

aevolwait <- merge(aevolume, aewait, by="Period")

aevolwait <- aevolwait %>%
  mutate(Period=as.Date(Period), origin="1899-12-30") %>%
  mutate(monthyear=format(as.Date(Period), "%Y-%m")) %>%
  mutate(aewait4plus=(1-`Percentage in 4 hours or less (type 1)`)) %>%
  mutate(pct4to12admitted=`Number of patients spending >4 hours from decision to admit to admission`/`Total Emergency Admissions`) %>%
  mutate(pct12admitted=`Number of patients spending >12 hours from decision to admit to admission`/`Total Emergency Admissions`) %>%
  mutate(pct4plusadmitted=pct4to12admitted+pct12admitted)

## Chart AE attendances
coeff=max(aevolwait$`Type 1 Departments - Major A&E`)
ggp1 <- ggplot(data=aevolwait) +
  geom_bar(aes(x=monthyear, y=`Type 1 Departments - Major A&E`), colour="blue", stat="identity") +
  xlab("Year and quarter") +
  theme(axis.text.x=element_text(angle=90, hjust=1))
ggp2 <- ggp1 + 
  geom_line(aes(monthyear, aewait4plus*coeff, group=1), color="red", lwd=2)
ggp3 <- ggp2 +
  scale_y_continuous(name="Number of A&E attendances", sec.axis=sec_axis(~./coeff, name="Proportion in A&E 4+ hours")) 
ggp3

## Chart emergency admissions
coeff=max(aevolwait$`Total Emergency Admissions`)
ggp1 <- ggplot(aevolwait) +
  geom_bar(aes(x=monthyear, y=`Total Emergency Admissions`), color="blue", stat="identity") +
  xlab("Year and quarter") +
  theme(axis.text.x=element_text(angle=90, hjust=1))
ggp2 <- ggp1 +
  geom_line(aes(monthyear, pct4plusadmitted*coeff, group=1), color="red", lwd=2 ) 
ggp3 <- ggp2 +
  scale_y_continuous(labels=scales::comma, name="Number of emergency admissions", sec.axis=sec_axis(~./coeff, name="Proportion waiting 4+ hours to be admitted")) 
ggp3

