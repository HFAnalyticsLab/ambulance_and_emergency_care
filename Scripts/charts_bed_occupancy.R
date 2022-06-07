library(aws.s3)
library(tidyverse)
library(ggplot2)


bedoccup <- readRDS("ambulance/bedoccup.rds")

bedoccup <- bedoccup %>%
  mutate(timeyr=substr(Year,1,4)) %>%
  mutate(timeyr=as.numeric(timeyr)) %>%
  mutate(timeqtr=ifelse(`Period End`=="June", 1,
                        ifelse(`Period End`=="December",3,
                               ifelse(`Period End`=="March",4,2)))) %>%
  mutate(time=paste0(timeyr,timeqtr)) %>%
  mutate(timenum=as.numeric(time)) %>%
  mutate(pctoccuptot=as.numeric(Total...18))

bedoccup2019_2021 <- filter(bedoccup, timenum>20184)
bedoccup2019_2021$`Region Code` <- factor(bedoccup2019_2021$`Region Code`, labels=c("London", "Southwest", "Southeast", "Midlands", "East", "Northwest", "Northeast"))
mytable <- table(bedoccup2019_2021$time)
mytable

aggregate(bedoccup2019_2021$pctoccuptot, list(bedoccup2019_2021$`Region Code`), FUN=mean, na.rm=TRUE)

summocc <- bedoccup2019_2021 %>%
  drop_na(pctoccuptot) %>%
  group_by(`Region Code`, time) %>%
  summarise(meanoccup=mean(pctoccuptot), n=n())

ggplot(data=summocc, aes(x=time, y=meanoccup, group=`Region Code`)) +
  geom_line(aes(linetype=`Region Code`, colour=`Region Code`)) +
  ylim(0.5,1.0)


bedoccup2015_2017 <- filter(bedoccup, timenum<20181 & timenum>20144)
bedoccup2015_2017$`Region Code` <- factor(bedoccup2015_2017$`Region Code`, labels=c("North", "Mids & East", "London", "South"))

aggregate(bedoccup2015_2017$pctoccuptot, list(bedoccup2015_2017$`Region Code`), FUN=mean, na.rm=TRUE)

summocc <- bedoccup2015_2017 %>%
  drop_na(pctoccuptot) %>%
  group_by(`Region Code`, time) %>%
  summarise(meanoccup=mean(pctoccuptot), n=n())
ggplot(data=summocc, aes(x=time, y=meanoccup, group=`Region Code`)) +
  geom_line(aes(linetype=`Region Code`, colour=`Region Code`)) +
  ylim(0.5,1.0)


