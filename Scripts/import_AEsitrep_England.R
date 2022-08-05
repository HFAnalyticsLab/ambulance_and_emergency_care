
library(aws.s3)
library(readxl)
library(tidyverse)
library(lubridate)
library(stringr)

ambdelay <- read_excel("data/2017handovers.xlsx")

ambdelay_60 <- ambdelay[ , grep("Delay >60", names( ambdelay))]  # keep columns for delays >60 mins
ambdelay_3060 <- ambdelay[ , grep("Delay 30-60", names( ambdelay))]  # keep columns for delays 30-60 mins
ambdenom <- ambdelay[ , grep("Arriving by", names( ambdelay))]

# now need to convert wide to long 
longtest1 <- ambdenom %>%
  pivot_longer(cols=starts_with("Arriving by ambulance..."), names_to="day", names_prefix="Arriving by ambulance...", values_to="denom", values_drop_na=FALSE)
longtest2 <- ambdelay_60 %>%
  pivot_longer(cols=starts_with("Delay >60 mins..."), names_to="day", names_prefix="Delay >60 mins...", values_to="numdelays60plus", values_drop_na=FALSE)
longtest3 <- ambdelay_3060 %>%
  pivot_longer(cols=starts_with("Delay 30-60 mins..."), names_to="day", names_prefix="Delay 30-60 mins...", values_to="numdelays3060", values_drop_na=FALSE)

longtest1 <- longtest1 %>%
  mutate(date=format(as.Date(seq(ymd('2017-11-20'),ymd('2018-03-04'), by='1 day')),"%Y-%m-%d"))
longtest2 <- longtest2 %>%
  mutate(date=format(as.Date(seq(ymd('2017-11-20'),ymd('2018-03-04'), by='1 day')),"%Y-%m-%d"))
longtest3 <- longtest3 %>%
  mutate(date=format(as.Date(seq(ymd('2017-11-20'),ymd('2018-03-04'), by='1 day')),"%Y-%m-%d"))

amball <- merge(longtest1,longtest2, by="date")
amball <- merge(amball,longtest3, by="date")
amball <- subset(amball, select=-c(day, day.x, day.y))
amball201718 <- amball %>%
  mutate(denom=as.numeric(denom)) %>%
  mutate(numdelays60plus=as.numeric(numdelays60plus)) %>%
  mutate(numdelays3060=as.numeric(numdelays3060)) %>%
  mutate(pctdelay60plus=numdelays60plus*100/denom) %>%
  mutate(pctdelay60plus=na_if(pctdelay60plus, Inf)) %>%
  mutate(pctdelay3060=numdelays3060*100/denom) %>%
  mutate(pctdelay3060=na_if(pctdelay3060, Inf)) %>% 
  mutate(date2=date2ISOweek(date)) %>% 
  mutate(date2=str_sub(date2,0,8))

saveRDS(amball201718, file="amball201718.rds")


ambdelay <- read_excel("data/2018handovers.xlsx")

ambdelay_60 <- ambdelay[ , grep("Delay >60", names( ambdelay))]  # keep columns for delays >60 mins
ambdelay_3060 <- ambdelay[ , grep("Delay 30-60", names( ambdelay))]  # keep columns for delays 30-60 mins
ambdenom <- ambdelay[ , grep("Arriving by", names( ambdelay))]

longtest1 <- ambdenom %>%
  pivot_longer(cols=starts_with("Arriving by ambulance..."), names_to="day", names_prefix="Arriving by ambulance...", values_to="denom", values_drop_na=FALSE)
longtest2 <- ambdelay_60 %>%
  pivot_longer(cols=starts_with("Delay >60 mins..."), names_to="day", names_prefix="Delay >60 mins...", values_to="numdelays60plus", values_drop_na=FALSE)
longtest3 <- ambdelay_3060 %>%
  pivot_longer(cols=starts_with("Delay 30-60 mins..."), names_to="day", names_prefix="Delay 30-60 mins...", values_to="numdelays3060", values_drop_na=FALSE)

longtest1 <- longtest1 %>%
  mutate(date=format(as.Date(seq(ymd('2018-12-03'),ymd('2019-03-03'), by='1 day')),"%Y-%m-%d"))
longtest2 <- longtest2 %>%
  mutate(date=format(as.Date(seq(ymd('2018-12-03'),ymd('2019-03-03'), by='1 day')),"%Y-%m-%d"))
longtest3 <- longtest3 %>%
  mutate(date=format(as.Date(seq(ymd('2018-12-03'),ymd('2019-03-03'), by='1 day')),"%Y-%m-%d"))

amball <- merge(longtest1,longtest2, by="date")
amball <- merge(amball,longtest3, by="date")
amball <- subset(amball, select=-c(day, day.x, day.y))
amball201819 <- amball %>%
  mutate(denom=as.numeric(denom)) %>%
  mutate(numdelays60plus=as.numeric(numdelays60plus)) %>%
  mutate(numdelays3060=as.numeric(numdelays3060)) %>%
  mutate(pctdelay60plus=numdelays60plus*100/denom) %>%
  mutate(pctdelay60plus=na_if(pctdelay60plus, Inf)) %>%
  mutate(pctdelay3060=numdelays3060*100/denom) %>%
  mutate(pctdelay3060=na_if(pctdelay3060, Inf)) %>% 
  mutate(date2=date2ISOweek(date)) %>% 
  mutate(date2=str_sub(date2,0,8))


saveRDS(amball201819, file="amball201819.rds")


ambdelay <- read_excel("data/2019handovers.xlsx")

ambdelay_60 <- ambdelay[ , grep("Delay >60", names( ambdelay))]  # keep columns for delays >60 mins
ambdelay_3060 <- ambdelay[ , grep("Delay 30-60", names( ambdelay))]  # keep columns for delays 30-60 mins
ambdenom <- ambdelay[ , grep("Arriving by", names( ambdelay))]

longtest1 <- ambdenom %>%
  pivot_longer(cols=starts_with("Arriving by ambulance..."), names_to="day", names_prefix="Arriving by ambulance...", values_to="denom", values_drop_na=FALSE)
longtest2 <- ambdelay_60 %>%
  pivot_longer(cols=starts_with("Delay >60 mins..."), names_to="day", names_prefix="Delay >60 mins...", values_to="numdelays60plus", values_drop_na=FALSE)
longtest3 <- ambdelay_3060 %>%
  pivot_longer(cols=starts_with("Delay 30-60 mins..."), names_to="day", names_prefix="Delay 30-60 mins...", values_to="numdelays3060", values_drop_na=FALSE)

longtest1 <- longtest1 %>%
  mutate(date=format(as.Date(seq(ymd('2019-12-02'),ymd('2020-03-01'), by='1 day')),"%Y-%m-%d"))
longtest2 <- longtest2 %>%
  mutate(date=format(as.Date(seq(ymd('2019-12-02'),ymd('2020-03-01'), by='1 day')),"%Y-%m-%d"))
longtest3 <- longtest3 %>%
  mutate(date=format(as.Date(seq(ymd('2019-12-02'),ymd('2020-03-01'), by='1 day')),"%Y-%m-%d"))

amball <- merge(longtest1,longtest2, by="date")
amball <- merge(amball,longtest3, by="date")
amball <- subset(amball, select=-c(day, day.x, day.y))
amball201920 <- amball %>%
  mutate(denom=as.numeric(denom)) %>%
  mutate(numdelays60plus=as.numeric(numdelays60plus)) %>%
  mutate(numdelays3060=as.numeric(numdelays3060)) %>%
  mutate(pctdelay60plus=numdelays60plus*100/denom) %>%
  mutate(pctdelay60plus=na_if(pctdelay60plus, Inf)) %>%
  mutate(pctdelay3060=numdelays3060*100/denom) %>%
  mutate(pctdelay3060=na_if(pctdelay3060, Inf)) %>% 
  mutate(date2=date2ISOweek(date)) %>% 
  mutate(date2=str_sub(date2,0,8))


saveRDS(amball201920, file="amball201920.rds")

ambdelay <- read_excel("data/2020handovers.xlsx")

ambdelay_60 <- ambdelay[ , grep("Delay >60", names( ambdelay))]  # keep columns for delays >60 mins
ambdelay_3060 <- ambdelay[ , grep("Delay 30-60", names( ambdelay))]  # keep columns for delays 30-60 mins
ambdenom <- ambdelay[ , grep("Arriving by", names( ambdelay))]

longtest1 <- ambdenom %>%
  pivot_longer(cols=starts_with("Arriving by ambulance..."), names_to="day", names_prefix="Arriving by ambulance...", values_to="denom", values_drop_na=FALSE)
longtest2 <- ambdelay_60 %>%
  pivot_longer(cols=starts_with("Delay >60 mins..."), names_to="day", names_prefix="Delay >60 mins...", values_to="numdelays60plus", values_drop_na=FALSE)
longtest3 <- ambdelay_3060 %>%
  pivot_longer(cols=starts_with("Delay 30-60 mins..."), names_to="day", names_prefix="Delay 30-60 mins...", values_to="numdelays3060", values_drop_na=FALSE)

longtest1 <- longtest1 %>%
  mutate(date=format(as.Date(seq(ymd('2020-11-30'),ymd('2021-04-04'), by='1 day')),"%Y-%m-%d"))
longtest2 <- longtest2 %>%
  mutate(date=format(as.Date(seq(ymd('2020-11-30'),ymd('2021-04-04'), by='1 day')),"%Y-%m-%d"))
longtest3 <- longtest3 %>%
  mutate(date=format(as.Date(seq(ymd('2020-11-30'),ymd('2021-04-04'), by='1 day')),"%Y-%m-%d"))

amball <- merge(longtest1,longtest2, by="date")
amball <- merge(amball,longtest3, by="date")
amball <- subset(amball, select=-c(day, day.x, day.y))
amball202021 <- amball %>%
  mutate(denom=as.numeric(denom)) %>%
  mutate(numdelays60plus=as.numeric(numdelays60plus)) %>%
  mutate(numdelays3060=as.numeric(numdelays3060)) %>%
  mutate(pctdelay60plus=numdelays60plus*100/denom) %>%
  mutate(pctdelay60plus=na_if(pctdelay60plus, Inf)) %>%
  mutate(pctdelay3060=numdelays3060*100/denom) %>%
  mutate(pctdelay3060=na_if(pctdelay3060, Inf)) %>% 
  mutate(date2=date2ISOweek(date)) %>% 
  mutate(date2=str_sub(date2,0,8))


saveRDS(amball202021, file="amball202021.rds")

ambdelay <- read_excel("data/2021handovers.xlsx")

ambdelay_60 <- ambdelay[ , grep("Delay >60", names( ambdelay))]  # keep columns for delays >60 mins
ambdelay_3060 <- ambdelay[ , grep("Delay 30-60", names( ambdelay))]  # keep columns for delays 30-60 mins
ambdenom <- ambdelay[ , grep("Arriving by", names( ambdelay))]

longtest1 <- ambdenom %>%
  pivot_longer(cols=starts_with("Arriving by ambulance..."), names_to="day", names_prefix="Arriving by ambulance...", values_to="denom", values_drop_na=FALSE)
longtest2 <- ambdelay_60 %>%
  pivot_longer(cols=starts_with("Delay >60 mins..."), names_to="day", names_prefix="Delay >60 mins...", values_to="numdelays60plus", values_drop_na=FALSE)
longtest3 <- ambdelay_3060 %>%
  pivot_longer(cols=starts_with("Delay 30-60 mins..."), names_to="day", names_prefix="Delay 30-60 mins...", values_to="numdelays3060", values_drop_na=FALSE)

longtest1 <- longtest1 %>%
  mutate(date=format(as.Date(seq(ymd('2021-11-29'),ymd('2022-04-03'), by='1 day')),"%Y-%m-%d"))
longtest2 <- longtest2 %>%
  mutate(date=format(as.Date(seq(ymd('2021-11-29'),ymd('2022-04-03'), by='1 day')),"%Y-%m-%d"))
longtest3 <- longtest3 %>%
  mutate(date=format(as.Date(seq(ymd('2021-11-29'),ymd('2022-04-03'), by='1 day')),"%Y-%m-%d"))

amball <- merge(longtest1,longtest2, by="date")
amball <- merge(amball,longtest3, by="date")
amball <- subset(amball, select=-c(day, day.x, day.y))
amball202122 <- amball %>%
  mutate(denom=as.numeric(denom)) %>%
  mutate(numdelays60plus=as.numeric(numdelays60plus)) %>%
  mutate(numdelays3060=as.numeric(numdelays3060)) %>%
  mutate(pctdelay60plus=numdelays60plus*100/denom) %>%
  mutate(pctdelay60plus=na_if(pctdelay60plus, Inf)) %>%
  mutate(pctdelay3060=numdelays3060*100/denom) %>%
  mutate(pctdelay3060=na_if(pctdelay3060, Inf)) %>% 
  mutate(date2=date2ISOweek(date)) %>% 
  mutate(date2=str_sub(date2,0,8))


saveRDS(amball202122, file="amball202122.rds")

amball201722 <- rbind(amball201718,amball201819)
amball201722 <- rbind(amball201722,amball201920)
amball201722 <- rbind(amball201722,amball202021)
amball201722 <- rbind(amball201722,amball202122)

saveRDS(amball201722, file="amball201722.rds")
