library(data.table)
library(aws.s3)
library(readr)
library(tidyverse)
library(rio)
library(lubridate)
library(ggplot2)
library(hms)
library(tidyverse)
library(THFstyle)
library(tsibble)
library(ggtext)

rm(list=ls())

# Data load -----------------------------------------------------------------


buck<-'thf-dap-tier0-projects-iht-067208b7-projectbucket-1mrmynh0q7ljp/ambulance/clean'


amb_dta<-s3read_using(read.csv # Which function are we using to read
                      , object = 'amb_calls.csv' # File to open
                      , bucket = buck) # Bucket name defined above

# Make variables numeric --------------------------------------------------

amb_dta[7:14] = lapply(amb_dta[7:14], FUN = function(y){as.numeric(y)})

amb_dta_plot<-amb_dta %>% 
  pivot_longer(c(answered_times_mean:answered_times_90), names_to = 'metric', values_to = 'answer_time')

amb_dta_plot<-amb_dta_plot %>% 
  mutate(answer_time2=as.POSIXct(as.numeric(answer_time),origin = "1970-01-01", tz="GMT")) %>% 
  mutate(answer_time2=format(answer_time2, format="%H:%M:%S")) %>% 
  mutate(answer_time2=as_hms(answer_time2)) %>% 
  mutate(date2=yearmonth(date)) %>% 
  mutate(org_lab=factor(org_name, levels=c("England","North East and Yorkshire","North West",
                                           "Midlands","East of England","London","South East","South West"))) %>% 
  mutate(contact_count=as.numeric(contact_count)) %>% 
  mutate(calls_answered=as.numeric(calls_answered)) %>% 
  select(c(contact_count:calls_answered,date:org_lab))


#just like with response times, answer times have also gone up, so suggesting it's an increase 
#in demand for the whole system and not just to do with transporting to ED 
amb_dta_plot %>%
  filter(org_lab=="England" & metric=="answered_times_mean") %>%
  ggplot(.,aes(x=date2, y=answer_time2, group=metric, colour=metric))+
  geom_line(linetype='solid')+
  # geom_point(size=0.25)+
  # geom_hline(yintercept = as_hms("00:07:00"), colour = '#524c48', linetype='dashed' )+
  # geom_hline(yintercept = as_hms("00:15:00"), colour = '#524c48', linetype='dashed')+
  scale_x_yearmonth( breaks = '6 months',date_labels = "%b %g")+
  theme_THF()+
  facet_grid(cols=vars(org_lab))+
  scale_colour_THF()+
  labs(x = "", y="Call answer times (seconds)", caption = "NHS England, Ambulance Quality Indicators")+
  theme(legend.text=element_text(size=11),
        legend.title = element_blank(),
        axis.text.x=element_text(size=8, angle=60), 
        axis.text.y=element_text(size=11),
        plot.caption = element_markdown(hjust=0, size=9),
        plot.margin = unit(c(1,1.5,0.5,0.5), "cm"),
        legend.margin=margin(0,0,0,0),
        legend.box.margin=margin(-10,-10,-10,-10))

amb_dta_plot %>%
  filter(org_lab=="England") %>%
  select(-c("metric", "answer_time", "answer_time2")) %>% 
  pivot_longer(c(contact_count, calls_answered), names_to="metric", values_to="count") %>% 
  ggplot(.,aes(x=date2, y=count, group=metric, colour=metric))+
  geom_line(linetype='solid')+
  # geom_line(aes(y=calls_answered),linetype='solid', colour="blue")+
  # geom_point(size=0.25)+
  # geom_hline(yintercept = as_hms("00:07:00"), colour = '#524c48', linetype='dashed' )+
  # geom_hline(yintercept = as_hms("00:15:00"), colour = '#524c48', linetype='dashed')+
  scale_x_yearmonth( breaks = '6 months',date_labels = "%b %g")+
  theme_THF()+
  facet_grid(cols=vars(org_lab))+
  scale_colour_THF()+
  labs(x = "", y="Counts", caption = "NHS England, Ambulance Quality Indicators")+
  theme(legend.text=element_text(size=11),
        legend.title = element_blank(),
        axis.text.x=element_text(size=8, angle=60), 
        axis.text.y=element_text(size=11),
        plot.caption = element_markdown(hjust=0, size=9),
        plot.margin = unit(c(1,1.5,0.5,0.5), "cm"),
        legend.margin=margin(0,0,0,0),
        legend.box.margin=margin(-10,-10,-10,-10))



# Rolling averages --------------------------------------------------------

#Call volume and answered

rolling_count_answered<-amb_dta_plot %>% 
  filter(org_lab=="England") %>% 
  dplyr::select(c(date2,date,contact_count, calls_answered)) %>% 
  distinct() %>% 
  mutate(contact_count_rollmean=rollmean(contact_count, k=12, fill=NA),
         calls_answered_rollmean=rollmean(calls_answered, k=12, fill=NA)) %>% 
  drop_na()

start_dates<-format(as.Date(seq(ymd('2017-08-01'),ymd('2021-05-01'),by='1 month')),"%Y-%m-%d")
end_dates<-format(as.Date(seq(ymd('2018-07-01'),ymd('2022-04-01'),by='1 month')),"%Y-%m-%d")

list_dates<-paste0(yearmonth(start_dates),"-",yearmonth(end_dates))
order<-c(1:46)

rolling_count_answered<-cbind(rolling_count_answered,list_dates,order)

rolling_count_answered_plot<-rolling_count_answered %>% 
  select(list_dates, order, contains("rollmean")) %>% 
  pivot_longer(contains("rollmean"), names_to="metric", values_to="val") %>% 
  mutate(name=fct_reorder(list_dates,order)) 

rolling_count_answered_plot %>% 
  ggplot(.,aes(x=name, y=val, group=metric, colour=metric))+
  geom_line(linetype='solid')+
  scale_y_continuous(labels=comma)+
  theme_THF()+
  # facet_grid(cols=vars(met_cat))+
  scale_colour_THF()+
  labs(x = "", y="counts", caption = "NHS England, Ambulance Quality Indicators")+
  theme(legend.text=element_text(size=11),
        legend.title = element_blank(),
        axis.text.x=element_text(size=8, angle=90), 
        axis.text.y=element_text(size=11),
        plot.caption = element_markdown(hjust=0, size=9),
        plot.margin = unit(c(1,1.5,0.5,0.5), "cm"),
        legend.margin=margin(0,0,0,0),
        legend.box.margin=margin(-10,-10,-10,-10))

v_date<-paste0(yearmonth(format(as.Date(ymd('2018-05-01'),"%Y-%m-%d"))),"-",
               yearmonth(format(as.Date(ymd('2019-04-01'),"%Y-%m-%d"))))
w_date<-paste0(yearmonth(format(as.Date(ymd('2019-05-01'),"%Y-%m-%d"))),"-",
               yearmonth(format(as.Date(ymd('2020-04-01'),"%Y-%m-%d"))))
x_date<-paste0(yearmonth(format(as.Date(ymd('2020-05-01'),"%Y-%m-%d"))),"-",
               yearmonth(format(as.Date(ymd('2021-04-01'),"%Y-%m-%d"))))
y_date<-paste0(yearmonth(format(as.Date(ymd('2021-05-01'),"%Y-%m-%d"))),"-",
               yearmonth(format(as.Date(ymd('2022-04-01'),"%Y-%m-%d"))))

dates_calcs=c(v_date,w_date, x_date, y_date)

calcs<-rolling_count_answered_plot %>% 
  filter(list_dates %in% dates_calcs ) %>% 
  pivot_wider(id_cols=metric,names_from=list_dates, values_from=val) %>% 
  clean_names() %>% 
  mutate(per_change_a_d=((.[[5]]-.[[2]])/.[[2]])*100,
         per_change_b_d=((.[[5]]-.[[3]])/.[[3]])*100,
         per_change_c_d=((.[[5]]-.[[4]])/.[[4]])*100)


#Mean answer times 
rolling_mean_answer<-amb_dta_plot %>% 
  filter(org_lab=="England"& metric=="answered_times_mean") %>% 
  dplyr::select(c(date2,date,answer_time)) %>% 
  mutate(answer_time_rollmean=rollmean(as.numeric(answer_time), k=12, fill=NA)) %>% 
  drop_na() %>% 
  mutate(answer_time2_rollmean=as.POSIXct(as.numeric(answer_time_rollmean),origin = "1970-01-01", tz="GMT")) %>% 
  mutate(answer_time2_rollmean=format(answer_time2_rollmean, format="%H:%M:%S")) %>% 
  mutate(answer_time2_rollmean=as_hms(answer_time2_rollmean))


rolling_mean_answer<-cbind(rolling_mean_answer,list_dates,order)

rolling_mean_answer %>%
  mutate(name=fct_reorder(list_dates,order)) %>% 
  ggplot(.,aes(x=name, y=answer_time2_rollmean, group=1))+
  geom_line(linetype='solid')+
  # scale_y_time(breaks = as_hms(seq(0, 72000, by = 1800)))+
  theme_THF()+
  scale_colour_THF()+
  labs(x = "", y="Answer time (seconds)", caption = "NHS England, Ambulance Quality Indicators")+
  theme(legend.text=element_text(size=11),
        legend.title = element_blank(),
        axis.text.x=element_text(size=8, angle=90), 
        axis.text.y=element_text(size=11),
        plot.caption = element_markdown(hjust=0, size=9),
        plot.margin = unit(c(1,1.5,0.5,0.5), "cm"),
        legend.margin=margin(0,0,0,0),
        legend.box.margin=margin(-10,-10,-10,-10))


calcs<-rolling_mean_answer %>% 
  filter(list_dates %in% dates_calcs )





#Calculations

pre_dates<-format(as.Date(seq(ymd('2018-08-01'),ymd('2019-07-01'),by='1 month')),"%Y-%m-%d")
post_dates<-format(as.Date(seq(ymd('2021-08-01'),ymd('2022-07-01'),by='1 month')),"%Y-%m-%d")
list_dates<-c(pre_dates, post_dates)

calcs<-amb_dta %>% 
filter(date %in% list_dates & region=="Eng") %>% 
  mutate(time=case_when(date  %in% pre_dates ~ "2018/19",
                        date %in% post_dates ~ "2021/22", 
                        TRUE~"NA")) %>% 
  select(time, calls_answered, answered_times_total) %>% 
  group_by(time) %>% 
  summarise(across(where(is.numeric), ~ sum(.x, na.rm = TRUE))) %>% 
  mutate(mean_answer_time=answered_times_total/calls_answered) %>% 
  mutate(answer_time2=as.POSIXct(as.numeric(mean_answer_time),origin = "1970-01-01", tz="GMT")) %>% 
  mutate(answer_time2=format(answer_time2, format="%H:%M:%S")) %>% 
  mutate(answer_time2=as_hms(answer_time2)) 



