library(aws.s3)
library(tidyverse)
library(ggplot2)

aeattend <- readRDS("aeattend.rds")

aeattend <- aeattend %>%
  mutate(time=paste0(year, month))

displaysummattend <- aeattend %>%
  group_by(`NHS England Region`, year, month) %>%
  summarise(meanattend=mean(totalattend), meanadmit=mean(totaladmit), n=n()) ## why is March 2019 data missing?

summattend <- aeattend %>%
  drop_na(totalattend) %>%
  drop_na(`Number of A&E attendances Type 1`) %>%
  drop_na(`Number of A&E attendances Type 2`) %>%
  drop_na(`Number of A&E attendances Other A&E Department`) %>%
  group_by(`NHS England Region`, time) %>%
  summarise(meanattend=mean(totalattend), meanadmit=mean(totaladmit), meantype1=mean(`Number of A&E attendances Type 1`),
            meantype2=mean(`Number of A&E attendances Type 2`), meantypeoth=mean(`Number of A&E attendances Other A&E Department`), n=n())

## figure of attendances and admissions
ggplot(data=summattend, aes(x=time, y=meanattend, group=`NHS England Region`)) +
  geom_line(aes(linetype=`NHS England Region`, colour=`NHS England Region`)) +
  ylab("Number of A&E attendances") +
  ggtitle("Apr2018-Mar21")
ggplot(data=summattend, aes(x=time, y=meantype1, group=`NHS England Region`)) +
  geom_line(aes(linetype=`NHS England Region`, colour=`NHS England Region`)) +
  ylab("Number of type 1 attendances") +
  ggtitle("Apr2018-Mar21")
ggplot(data=summattend, aes(x=time, y=meantype2, group=`NHS England Region`)) +
  geom_line(aes(linetype=`NHS England Region`, colour=`NHS England Region`)) +
  ylab("Number of type 2 attendances") +
  ggtitle("Apr2018-Mar21")
ggplot(data=summattend, aes(x=time, y=meantypeoth, group=`NHS England Region`)) +
  geom_line(aes(linetype=`NHS England Region`, colour=`NHS England Region`)) +
  ylab("Number of other attendances") +
  ggtitle("Apr2018-Mar21")

ggplot(data=summattend, aes(x=time, y=meanadmit, group=`NHS England Region`)) +
  geom_line(aes(linetype=`NHS England Region`, colour=`NHS England Region`)) +
  ylab("Number admitted from A&E") +
  ggtitle("Apr2018-Mar21")


summattend <- aeattend %>%
  drop_na(pct4to12hrsadmit) %>%
  drop_na(pct12plushrsadmit) %>%
  drop_na(pct4plushrswaittype1) %>%
  drop_na(pct4plushrswaittype2) %>%
  group_by(`NHS England Region`, time) %>%
  summarise(mean412admit=mean(pct4to12hrsadmit), mean12plusadmit=mean(pct12plushrsadmit), mean4pluswaittype1=mean(pct4plushrswaittype1),
            mean4pluswaittype2=mean(pct4plushrswaittype2), n=n())

ggplot(data=summattend, aes(x=time, y=mean4pluswaittype1, group=`NHS England Region`)) +
  geom_line(aes(linetype=`NHS England Region`, colour=`NHS England Region`)) +
  ylab("% waiting 4+ hrs in A&E type 1") +
  ggtitle("Apr2018-Mar21")

ggplot(data=summattend, aes(x=time, y=mean4pluswaittype2, group=`NHS England Region`)) +
  geom_line(aes(linetype=`NHS England Region`, colour=`NHS England Region`)) +
  ylab("% waiting 4+ hrs in A&E type 2") +
  ggtitle("Apr2018-Mar21")

ggplot(data=summattend, aes(x=time, y=mean412admit, group=`NHS England Region`)) +
  geom_line(aes(linetype=`NHS England Region`, colour=`NHS England Region`)) +
  ylab("% waiting 4-12 hrs to be admitted from A&E") +
  ggtitle("Apr2018-Mar21")

ggplot(data=summattend, aes(x=time, y=mean12plusadmit, group=`NHS England Region`)) +
  geom_line(aes(linetype=`NHS England Region`, colour=`NHS England Region`)) +
  ylab("% waiting 12+ hrs to be admitted from A&E") +
  ggtitle("Apr2018-Mar21")



## Two lines per graph
ggplot(data=summattend, aes(x=time, group=`NHS England Region`)) +
  geom_line(aes(y=meanattend, colour=`NHS England Region`)) +
  geom_line(aes(y=meanadmit, colour=`NHS England Region`)) 


ggplot(data=summattend, aes(x=time, y=meanattend, group=`NHS England Region`)) +
  geom_line(aes(linetype=`NHS England Region`, colour=`NHS England Region`)) +
  ylab("Number of A&E attendances") +
  ggtitle("Apr2018-Mar21")






summattend <- aeattend %>%
  drop_na(totalattend) %>%
  group_by(`NHS England Region`, time) %>%
  summarise(meanattend=mean(totalattend), n=n()) 
summattendt1 <- aeattend %>%
  drop_na(`Number of A&E attendances Type 1`) %>%
  group_by(`NHS England Region`, time) %>%
  summarise(meantype1=mean(`Number of A&E attendances Type 1`), n=n())

summadmit <- aeattend %>%
  drop_na(totaladmit) %>%
  group_by(`NHS England Region`, time) %>%
  summarise(meanadmit=mean(totaladmit), n=n()) 
summattend <- aeattend %>%
  drop_na(totalattend) %>%
  group_by(`NHS England Region`, time) %>%
  summarise(meanattend=mean(totalattend), meanadmit=mean(totaladmit), meantype1=mean(`Number of A&E attendances Type 1`),
            meantype2=mean(`Number of A&E attendances Type 2`), meantypeoth=mean(`Number of A&E attendances Other A&E Department`), n=n()) ## why is March 2019 data missing?



ggplot(data=summocc, aes(x=time, y=meanoccup, group=`Region Code`)) +
  geom_line(aes(linetype=`Region Code`, colour=`Region Code`)) +
  ylim(0.5,1.0)

ggplot(data=summamb, aes(x=month, y=mean3060)) +
  geom_line(aes(linetype=`NHS England Region`, colour=`NHS England Region`)) +
  ylim(0, 20) +
  ylab("% handover 30-60mins") +
  ggtitle("Dec2020-March2021")

