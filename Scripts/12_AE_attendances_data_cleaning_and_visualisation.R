
library(aws.s3)
library(readxl)
library(tidyverse)
library(lubridate)


aevolume<-readxl::read_excel("data/aevol.xls", sheet='Activity')
aewait<-readxl::read_excel("data/aevol.xls", sheet='Performance')

aevolume<-aevolume %>% 
  clean_names() %>% 
  slice(which(title=="Period"):n()) %>% 
  row_to_names(., 1) %>% 
  clean_names()


aevolume<-aevolume %>% 
  clean_names() %>% 
  select(c("period","type_1_departments_major_a_e","emergency_admissions_via_type_1_a_e",
           "total_emergency_admissions","number_of_patients_spending_4_hours_from_decision_to_admit_to_admission",
           "number_of_patients_spending_12_hours_from_decision_to_admit_to_admission")) %>% 
  mutate(period=as.Date(as.numeric(period), origin="1899-12-30")) %>% 
  filter(period>as.Date("2016-12-01"))

aevolume[2:6] = lapply(aevolume[2:6], FUN = function(y){as.numeric(y)})

aewait<-aewait %>% 
  clean_names() %>% 
  slice(which(title=="Period"):n()) %>% 
  row_to_names(., 1) %>% 
  clean_names()

aewait<-aewait %>% 
  clean_names() %>% 
  select(c("period","percentage_in_4_hours_or_less_type_1")) %>% 
  mutate(period=as.Date(as.numeric(period), origin="1899-12-30")) %>% 
  filter(period>as.Date("2016-12-01")) %>% 
  mutate(percentage_in_4_hours_or_less_type_1=as.numeric(percentage_in_4_hours_or_less_type_1))

aevolwait <- merge(aevolume, aewait, by="period")

aevolwait <- aevolwait %>%
  mutate(Period=as.Date(period, origin="1899-12-30")) %>%
  mutate(monthyear=format(as.Date(period), "%b %y")) %>%
  mutate(aewait4plus=(1-percentage_in_4_hours_or_less_type_1)) %>%
  mutate(pct4to12admitted=number_of_patients_spending_4_hours_from_decision_to_admit_to_admission/total_emergency_admissions) %>%
  mutate(pct12admitted=number_of_patients_spending_12_hours_from_decision_to_admit_to_admission/total_emergency_admissions) %>%
  mutate(pct4plusadmitted=pct4to12admitted+pct12admitted) %>% 
  mutate(totattendaces=type_1_departments_major_a_e)

write_csv(aevolwait, 'aevolwait.csv')


aevolwait_v2<-aevolwait %>% 
  select(c(monthyear, pct4plusadmitted)) %>% 
  mutate(pct4plusadmitted=pct4plusadmitted*100) %>% 
  mutate(Metric="Waiting 4+ hours to be admitted (%)")
  
write_csv(aevolwait_v2, 'aevolwait_v2.csv')


plot<-aevolwait%>% 
  select(c(Period, monthyear, pct4plusadmitted)) %>% 
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

