#data load 
rm(list=ls())

#housekeeping
#library

library(here)
library(janitor)
library(tidyverse)
library(tsibble)
library(THFstyle)
library(ggtext)
library(curl)
library(readxl)
library(ggpubr)
library(Hmisc)

source("setup.R")


#Brief: % over 30 mins by trust by month. Could we plot against A&E performance?
#What is the relationship like between patients waiting over 4 hours to be admitted from A&E and the proportion of handovers over 30 minutes (or 60 minutes)? 



# Handovers ---------------------------------------------------------------


dt<-bread(myIHTbuck, 'emergency_care/hospital_handovers.xlsx')


handover_times<-dt %>% 
  clean_names() %>%  
  select(year_month, contains("percent")) %>% 
  mutate(date=as.Date(paste0("01 ", year_month), format="%d %b %Y")) %>% 
  mutate(monthyear=format(as.Date(date),"%b %y"))

#Visualise handovertimes
plot_data<-handover_times %>% 
  pivot_longer(percent_of_handovers_over_15_minutes:percent_of_handovers_over_90_minutes, names_to='metric', values_to='percent') %>% 
  mutate(metric2=substr(metric,str_locate(metric, "overs_")[,2],str_length(metric)))

#proportion of handovers completed within 15 minutes is increasing, but also the proportion of handovers completed 
plot<-plot_data%>% 
  ggplot(.,aes(x=date, y=percent, colour=metric2, group=metric2))+
  geom_line()+
  scale_x_yearmonth( breaks = '6 months',date_labels = "%b %g")+
  theme_THF()+
  # facet_grid(cols=vars(org_lab))+
  scale_colour_THF()+
  scale_y_continuous(labels = scales::percent)+
  labs(x = "", y="Proportion of handovers completed (%)", caption = "AACE")+
  theme(legend.text=element_text(size=11),
        legend.title = element_blank(),
        axis.text.x=element_text(size=8, angle=60), 
        axis.text.y=element_text(size=11),
        plot.caption = element_markdown(hjust=0, size=9),
        plot.margin = unit(c(1,1.5,0.5,0.5), "cm"),
        legend.margin=margin(0,0,0,0),
        legend.box.margin=margin(-10,-10,-10,-10))


ggplotly(plot) %>% 
  layout(legend = list(orientation = 'v',valign="top", font=list(size=8))) %>% 
  layout(legend=list(title=list(text=''))) 

#69% of handovers are now over 15 mins


# A&E capacity ------------------------------------------------------------

#Adjusted monthly time series 
link<-'https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2022/10/Adjusted-Monthly-AE-Time-Series-September-2022.xls'

destfile <- here::here('exploratory','data', "aevol.xls")
curl_download(link, destfile = destfile)

#Need to update range to include most recent data
aevolume<-read_excel("exploratory/data/aevol.xls", sheet='Activity', range="B16:N162")
aewait<-read_excel("exploratory/data/aevol.xls", sheet='Performance', range="B16:N159")


aevolume<-aevolume %>% 
  clean_names() %>% 
  select(c("period","type_1_departments_major_a_e","emergency_admissions_via_type_1_a_e",
           "total_emergency_admissions","number_of_patients_spending_4_hours_from_decision_to_admit_to_admission",
           "number_of_patients_spending_12_hours_from_decision_to_admit_to_admission")) %>% 
  filter(period>as.Date("2016-12-01"))

aewait<-aewait %>% 
  clean_names() %>% 
  select(c("period","percentage_in_4_hours_or_less_type_1")) %>% 
  filter(period>as.Date("2016-12-01"))

aevolwait <- merge(aevolume, aewait, by="period")

aevolwait <- aevolwait %>%
  mutate(Period=as.Date(period, origin="1899-12-30")) %>%
  mutate(monthyear=format(as.Date(period), "%b %y")) %>%
  mutate(aewait4plus=(1-percentage_in_4_hours_or_less_type_1)) %>%
  mutate(pct4to12admitted=number_of_patients_spending_4_hours_from_decision_to_admit_to_admission/total_emergency_admissions) %>%
  mutate(pct12admitted=number_of_patients_spending_12_hours_from_decision_to_admit_to_admission/total_emergency_admissions) %>%
  mutate(pct4plusadmitted=pct4to12admitted+pct12admitted) %>% 
  mutate(totattendaces=type_1_departments_major_a_e)

#Waiting 4+ to be admitted 
aevolwait_v2<-aevolwait %>% 
  select(c(Period, monthyear, pct4plusadmitted)) %>% 
  mutate(pct4plusadmitted2=pct4plusadmitted*100)

plot<-aevolwait_v2%>% 
  ggplot(.,aes(x=Period, y=pct4plusadmitted, group=1))+
  geom_line(colour='#dd0031')+
  scale_x_yearmonth( breaks = '6 months',date_labels = "%b %g")+
  theme_THF()+
  # facet_grid(cols=vars(org_lab))+
  # scale_colour_THF()+
  scale_y_continuous(labels = scales::percent)+
  labs(x = "", y="Proportion of patients waiting 4+ hours to be admitted (%)", caption = "A&E data")+
  theme(legend.text=element_text(size=11),
        legend.title = element_blank(),
        axis.text.x=element_text(size=8, angle=60), 
        axis.text.y=element_text(size=11),
        plot.caption = element_markdown(hjust=0, size=9),
        plot.margin = unit(c(1,1.5,0.5,0.5), "cm"),
        legend.margin=margin(0,0,0,0),
        legend.box.margin=margin(-10,-10,-10,-10))


ggplotly(plot) %>% 
  layout(legend = list(orientation = 'v',valign="top", font=list(size=8))) %>% 
  layout(legend=list(title=list(text=''))) 


# combining the data together ----------------------------------------------

dt<-handover_times %>% 
  left_join(aevolwait_v2, by="monthyear") %>% 
  mutate(year=format(as.Date(date),"%Y")) %>% 
  mutate(month=format(as.Date(date),"%b"))


#30 mins
dt %>% 
  ggplot(aes(x=pct4plusadmitted, y=percent_of_handovers_over_30_minutes, colour=year))+
  geom_point()+
  geom_smooth(method=lm, aes(fill=year))+
  theme_THF()+
  # facet_grid(cols=vars(year))+
  # scale_colour_THF()+
  scale_y_continuous(labels = scales::percent)+
  scale_x_continuous(labels=scales::percent)+
  labs(y = "Handovers taking 30+ minutes (%)", x="Patients waiting 4+ hours to be admitted (%)", caption = "")+
  theme(legend.text=element_text(size=14),
        legend.title = element_blank(),
        axis.text.x=element_text(size=14, angle=60), 
        axis.text.y=element_text(size=14),
        plot.caption = element_markdown(hjust=0, size=9),
        plot.margin = unit(c(1,1.5,0.5,0.5), "cm"),
        legend.margin=margin(0,0,0,0),
        legend.box.margin=margin(-10,-10,-10,-10))


ggsave('plot.png', dpi=300, device  = "png",
       width = 7, height = 5)


#60+ mins

dt %>% 
  ggplot(aes(y=percent_of_handovers_over_60_minutes, x=pct4plusadmitted, colour=year))+
  geom_point()+
  geom_smooth(method=lm, aes(fill=year))+
  theme_THF()+
  # facet_grid(cols=vars(year))+
  # scale_colour_THF()+
  scale_y_continuous(labels = scales::percent)+
  scale_x_continuous(labels=scales::percent)+
  labs(y = "handovers 60+ mins (%)", x="waiting in A&E 4+ hours (%)", caption = "")+
  theme(legend.text=element_text(size=11),
        legend.title = element_blank(),
        axis.text.x=element_text(size=8, angle=60), 
        axis.text.y=element_text(size=11),
        plot.caption = element_markdown(hjust=0, size=9),
        plot.margin = unit(c(1,1.5,0.5,0.5), "cm"),
        legend.margin=margin(0,0,0,0),
        legend.box.margin=margin(-10,-10,-10,-10))


#15+mins 

dt %>% 
  ggplot(aes(y=percent_of_handovers_over_15_minutes, x=pct4plusadmitted, colour=year))+
  geom_point()+
  geom_smooth(method=lm, aes(fill=year))+
  theme_THF()+
  # facet_grid(cols=vars(year))+
  # scale_colour_THF()+
  scale_y_continuous(labels = scales::percent)+
  scale_x_continuous(labels=scales::percent)+
  labs(y = "handovers 15+ mins (%)", x="waiting in A&E 4+ hours (%)", caption = "")+
  theme(legend.text=element_text(size=11),
        legend.title = element_blank(),
        axis.text.x=element_text(size=8, angle=60), 
        axis.text.y=element_text(size=11),
        plot.caption = element_markdown(hjust=0, size=9),
        plot.margin = unit(c(1,1.5,0.5,0.5), "cm"),
        legend.margin=margin(0,0,0,0),
        legend.box.margin=margin(-10,-10,-10,-10))


#90+ mins
dt %>% 
  ggplot(aes(y=percent_of_handovers_over_90_minutes, x=pct4plusadmitted, colour=year))+
  geom_point()+
  geom_smooth(method=lm, aes(fill=year))+
  theme_THF()+
  # facet_grid(cols=vars(year))+
  # scale_colour_THF()+
  scale_y_continuous(labels = scales::percent)+
  scale_x_continuous(labels=scales::percent)+
  labs(y = "handovers 15+ mins (%)", x="waiting in A&E 4+ hours (%)", caption = "")+
  theme(legend.text=element_text(size=11),
        legend.title = element_blank(),
        axis.text.x=element_text(size=8, angle=60), 
        axis.text.y=element_text(size=11),
        plot.caption = element_markdown(hjust=0, size=9),
        plot.margin = unit(c(1,1.5,0.5,0.5), "cm"),
        legend.margin=margin(0,0,0,0),
        legend.box.margin=margin(-10,-10,-10,-10))


###Correlations

#Functions
flattenCorrMatrix <- function(cormat, pmat) {
  ut <- upper.tri(cormat)
  data.frame(
    row = rownames(cormat)[row(cormat)[ut]],
    column = rownames(cormat)[col(cormat)[ut]],
    cor  =(cormat)[ut],
    p = pmat[ut]
  )
}


dt_corr<-dt %>% 
  select(contains(c("percent","pct"))& !contains("2")) 

corr <- rcorr(as.matrix(dt_corr), type="spearman")
corr

t<-flattenCorrMatrix(corr$r, corr$P)

t<-t %>% 
  filter(str_detect(column,"pct"))


write.csv(t,'exploratory/t.csv')


# End ---------------------------------------------------------------------


#Save this script to bucket

put_object('exploratory_analyses.R', 'exploratory_analyses.R', paste0(myIHTbuck,"/emergency_care"))
