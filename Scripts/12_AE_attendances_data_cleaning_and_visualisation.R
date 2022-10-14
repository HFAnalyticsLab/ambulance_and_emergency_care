
library(aws.s3)
library(readxl)
library(tidyverse)
library(lubridate)

#Need to update range to include most recent data
aevolume<-read_excel("data/aevol.xls", sheet='Activity', range="B16:N162")
aewait<-read_excel("data/aevol.xls", sheet='Performance', range="B16:N159")


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

write_csv(aevolwait, 'aevolwait.csv')


aevolwait_v2<-aevolwait %>% 
  select(c(monthyear, pct4plusadmitted)) %>% 
  mutate(pct4plusadmitted=pct4plusadmitted*100) %>% 
  mutate(Metric="Waiting 4+ hours to be admitted (%)")
  
write_csv(aevolwait_v2, 'aevolwait_v2.csv')

## Chart AE attendances
coeff=max(aevolwait$type_1_departments_major_a_e)
ggp1 <- ggplot(data=aevolwait) +
  geom_bar(aes(x=monthyear, y=type_1_departments_major_a_e), colour="blue", stat="identity") +
  xlab("Year and quarter") +
  theme(axis.text.x=element_text(angle=90, hjust=1))
ggp2 <- ggp1 + 
  geom_line(aes(monthyear, aewait4plus*coeff, group=1), color="red", lwd=2)
ggp3 <- ggp2 +
  scale_y_continuous(name="Number of A&E attendances", sec.axis=sec_axis(~./coeff, name="Proportion in A&E 4+ hours")) 
ggp3

## Chart emergency admissions
coeff=max(aevolwait$total_emergency_admissions)
ggp1 <- ggplot(aevolwait) +
  geom_bar(aes(x=monthyear, y=total_emergency_admissions), color="blue", stat="identity") +
  xlab("Year and quarter") +
  theme(axis.text.x=element_text(angle=90, hjust=1))
ggp2 <- ggp1 +
  geom_line(aes(monthyear, pct4plusadmitted*coeff, group=1), color="red", lwd=2 ) 
ggp3 <- ggp2 +
  scale_y_continuous(labels=scales::comma, name="Number of emergency admissions", sec.axis=sec_axis(~./coeff, name="Proportion waiting 4+ hours to be admitted")) 
ggp3

