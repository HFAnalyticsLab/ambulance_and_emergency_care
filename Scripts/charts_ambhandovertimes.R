library(aws.s3)
library(tidyverse)
library(ggplot2)
library(ISOweek)


amball <- readRDS("amball201722.rds")


date_1<-format(as.Date(seq(ymd('2017-11-20'),ymd('2018-03-04'),by='1 day')),"%Y-%m-%d")
date_2<-format(as.Date(seq(ymd('2018-12-03'),ymd('2019-03-03'), by='1 day')),"%Y-%m-%d")
date_3<-format(as.Date(seq(ymd('2019-12-02'),ymd('2020-03-01'), by='1 day')),"%Y-%m-%d")
date_4<-format(as.Date(seq(ymd('2020-11-30'),ymd('2021-04-04'), by='1 day')),"%Y-%m-%d")
date_5<-format(as.Date(seq(ymd('2021-11-29'),ymd('2022-04-03'), by='1 day')),"%Y-%m-%d")
  
amball <- amball %>%
  mutate(monthyear=format(as.Date(date), "%Y-%m")) %>% 
  mutate(start_week=ISOweek2date(str_c(date2,"-1"))) %>% 
  mutate(year=case_when(date %in% date_1~ "2017/18",
                        date %in% date_2~ "2018/19",
                        date %in% date_3~ "2019/20",
                        date %in% date_4~ "2020/21",
                        date %in% date_5~ "2021/22"))

summamb <- amball %>%
  drop_na(pctdelay60plus) %>%
  drop_na(pctdelay3060) %>%
  group_by(date2) %>%
  mutate(mean60plus=mean(pctdelay60plus), mean3060=mean(pctdelay3060), n=n()) %>% 
  select(c(start_week,date2, year,mean60plus,mean3060,n)) %>% 
  distinct() %>% 
  mutate(week=str_sub(date2,6,8)) %>% 
  mutate(start_week=format(start_week, "%d %b %y")) %>% 
  ungroup()
  
order<-c(paste0("W", 47:53),paste0("W0",1:9), paste0("W",10:13))

flourish_amb_handover<-summamb %>% 
  filter(year=="2017/18") %>% 
  select(c(week, contains("mean"))) %>% 
  pivot_longer(contains("mean"), names_to="type", values_to="2017/18") %>% 
  full_join(summamb %>% 
              filter(year=="2018/19") %>% 
              select(c(week, contains("mean"))) %>% 
              pivot_longer(contains("mean"), names_to="type", values_to="2018/19"), by=c("week"="week",
                                                                                         "type"="type")) %>% 
  full_join(summamb %>% 
              filter(year=="2019/20") %>% 
              select(c(week, contains("mean"))) %>% 
              pivot_longer(contains("mean"), names_to="type", values_to="2019/20"), by=c("week"="week",
                                                                                         "type"="type")) %>% 
  full_join(summamb %>% 
              filter(year=="2020/21") %>% 
              select(c(week, contains("mean"))) %>% 
              pivot_longer(contains("mean"), names_to="type", values_to="2020/21"), by=c("week"="week",
                                                                                         "type"="type")) %>% 
  
  full_join(summamb %>% 
              filter(year=="2021/22") %>% 
              select(c(week, contains("mean"))) %>% 
              pivot_longer(contains("mean"), names_to="type", values_to="2021/22"), by=c("week"="week",
                                                                                         "type"="type")) %>% 
  mutate(week=factor(week, levels = order)) %>% 
  arrange(week)



write.csv(flourish_amb_handover,'flourish_amb_handovers.csv')


# 
# # drop months with fewer than 10 observations
# summamb <- filter(summamb, n>9)

p = ggplot(summamb) + 
  geom_line(aes(date2, mean60plus, group=1), color="red", lwd=2) +
  geom_line(aes(date2, mean3060, group=1), color="blue", lwd=2 ) +
  xlab("Month and year") +
  theme(axis.text.x=element_text(angle=90, hjust=1)) +
  ylab("% delayed")
p


write.csv(summamb,'summamb.csv')
