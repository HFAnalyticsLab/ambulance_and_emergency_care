library(aws.s3)
library(tidyverse)
library(ggplot2)

amball <- readRDS("ambulance/amball.rds")

amball <- amball %>%
  mutate(month=cut(day, breaks=c(-Inf, 34, 65, 93, Inf ), 
                   labels=c("Dec2020", "Jan2021", "Feb2021", "March2021")))  %>% ## March2021 includes first 4 days of April too
  mutate(month=as.numeric(month))

summamb <- amball %>%
  drop_na(pctdelay60plus) %>%
  drop_na(pctdelay3060) %>%
  group_by(`NHS England Region`, month) %>%
  summarise(mean60plus=mean(pctdelay60plus), mean3060=mean(pctdelay3060), n=n())

ggplot(data=summamb, aes(x=month, y=mean60plus)) +
  geom_line(aes(linetype=`NHS England Region`, colour=`NHS England Region`)) +
  ylim(0, 20) +
  ylab("% handover >60mins") +
  ggtitle("Dec2020-March2021")
ggplot(data=summamb, aes(x=month, y=mean3060)) +
  geom_line(aes(linetype=`NHS England Region`, colour=`NHS England Region`)) +
  ylim(0, 20) +
  ylab("% handover 30-60mins") +
  ggtitle("Dec2020-March2021")

