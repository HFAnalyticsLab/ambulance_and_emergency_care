#Ambulance handover delays- data cleaning 

# Housekeeping ------------------------------------------------------------

#Library
library(aws.s3)
library(readxl)
library(tidyverse)
library(lubridate)
library(stringr)
library(ISOweek)


rm(list=ls())
# 2017 --------------------------------------------------------------------

x2017handovers<-read_excel("data/raw2017handovers.xlsx", sheet='Ambulance Arrivals and Delays', range="F15:LH16")

ambdelay <- x2017handovers

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


# 2018 --------------------------------------------------------------------

x2018handovers<-read_excel("data/raw2018handovers.xlsx", sheet='Ambulance Arrivals and Delays', range="F15:JR16")

ambdelay<-x2018handovers


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


# 2019 --------------------------------------------------------------------


x2019handovers<-read_excel("data/raw2019handovers.xlsx", sheet='Ambulance Arrivals and Delays', range="F15:JR16")

ambdelay <- x2019handovers

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


# 2020 --------------------------------------------------------------------
 
x2020handovers<-read_excel("data/raw2020handovers.xlsx", sheet='Ambulance Arrivals and Delays', range="F15:NS17")

ambdelay<-x2020handovers[2,]

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


# 2021 --------------------------------------------------------------------

x2021handovers<-read_excel("data/raw2021handovers.xlsx", sheet='Ambulance Arrivals and Delays', range="F15:NS17")

ambdelay<-x2021handovers[2,]

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



# 2022 --------------------------------------------------------------------

x2022handovers<-read_excel("data/raw2022handovers.xlsx", sheet='Ambulance Arrivals and Delays', range="E15:UR16")

ambdelay<-x2022handovers

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
  mutate(date=format(as.Date(seq(ymd('2022-11-14'),ymd('2023-04-02'), by='1 day')),"%Y-%m-%d"))
longtest2 <- longtest2 %>%
  mutate(date=format(as.Date(seq(ymd('2022-11-14'),ymd('2023-04-02'), by='1 day')),"%Y-%m-%d"))
longtest3 <- longtest3 %>%
  mutate(date=format(as.Date(seq(ymd('2022-11-14'),ymd('2023-04-02'), by='1 day')),"%Y-%m-%d"))

amball <- merge(longtest1,longtest2, by="date")
amball <- merge(amball,longtest3, by="date")
amball <- subset(amball, select=-c(day, day.x, day.y))
amball202223 <- amball %>%
  mutate(denom=as.numeric(denom)) %>%
  mutate(numdelays60plus=as.numeric(numdelays60plus)) %>%
  mutate(numdelays3060=as.numeric(numdelays3060)) %>%
  mutate(pctdelay60plus=numdelays60plus*100/denom) %>%
  mutate(pctdelay60plus=na_if(pctdelay60plus, Inf)) %>%
  mutate(pctdelay3060=numdelays3060*100/denom) %>%
  mutate(pctdelay3060=na_if(pctdelay3060, Inf)) %>% 
  mutate(date2=date2ISOweek(date)) %>% 
  mutate(date2=str_sub(date2,0,8))


saveRDS(amball202223, file="amball202223.rds")





# 2023 --------------------------------------------------------------------


x2023handovers<-read_excel("data/raw2023handovers.xlsx", sheet='All handovers', range="F14:BNR19")


ambdelay<-x2023handovers %>% 
  t() %>% 
  as.data.frame() %>% 
  select(group=V1, metric=V2, count=V5) %>% 
  fill(group, .direction="down")


rownames(ambdelay) <-NULL


ambdelay<-ambdelay %>% 
  filter(group=="Count of all handovers (ED and non-ED inclusive)") %>% 
  mutate(metric2=case_when(str_detect(metric, "Handover time known")~ "denom", 
                          str_detect(metric, "Over 30")~ "numdelays30plus", 
                          str_detect(metric, "Over 60")~ "numdelays60plus", 
                          str_detect(metric, "Over 15")~ "numdelays15plus")) %>% 
  filter(!is.na(metric2)) %>% 
  select(-c(metric, group)) 
  


longtest1 <- ambdelay %>%
  filter(metric2=="denom") %>% 
  select(denom=count)
longtest2 <- ambdelay %>%
  filter(metric2=="numdelays60plus") %>% 
  select(numdelays60plus=count)
longtest3 <- ambdelay %>%
  filter(metric2=="numdelays30plus") %>% 
  select(numdelays30plus=count)

longtest1 <- longtest1 %>%
  mutate(date=format(as.Date(seq(ymd('2023-11-20'),ymd('2024-03-31'), by='1 day')),"%Y-%m-%d"))
longtest2 <- longtest2 %>%
  mutate(date=format(as.Date(seq(ymd('2023-11-20'),ymd('2024-03-31'), by='1 day')),"%Y-%m-%d"))
longtest3 <- longtest3 %>%
  mutate(date=format(as.Date(seq(ymd('2023-11-20'),ymd('2024-03-31'), by='1 day')),"%Y-%m-%d"))

amball <- merge(longtest1,longtest2, by="date")
amball <- merge(amball,longtest3, by="date")

amball202324 <- amball %>%
  mutate(denom=as.numeric(denom)) %>%
  mutate(numdelays60plus=as.numeric(numdelays60plus)) %>%
  mutate(numdelays30plus=as.numeric(numdelays30plus)) %>%
  mutate(numdelays3060=numdelays30plus-numdelays60plus) %>% 
  mutate(pctdelay60plus=numdelays60plus*100/denom) %>%
  mutate(pctdelay60plus=na_if(pctdelay60plus, Inf)) %>%
  mutate(pctdelay3060=numdelays3060*100/denom) %>%
  mutate(pctdelay3060=na_if(pctdelay3060, Inf)) %>% 
  mutate(date2=date2ISOweek(date)) %>% 
  mutate(date2=str_sub(date2,0,8)) %>% 
  select(-numdelays30plus)


# Combine data ------------------------------------------------------------


amball201723 <- rbind(amball201718,amball201819)
amball201723 <- rbind(amball201723,amball201920)
amball201723 <- rbind(amball201723,amball202021)
amball201723 <- rbind(amball201723,amball202122)
amball201723 <- rbind(amball201723,amball202223)
amball201724 <- rbind(amball201723,amball202324)

#saveRDS(amball201722, file="amball201722.rds")

#Save data
buck <- 'thf-dap-tier0-projects-iht-067208b7-projectbucket-1mrmynh0q7ljp/ambulance/clean' ## my bucket name

s3write_using(amball201724 # What R object we are saving
              , FUN = write.csv # Which R function we are using to save
              , object = 'amball201724.csv' # Name of the file to save to (include file type)
              , bucket = buck) # Bucket name defined above

