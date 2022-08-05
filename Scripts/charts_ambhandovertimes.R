library(aws.s3)
library(tidyverse)
library(ggplot2)

amball <- readRDS("amball201722.rds")

amball <- amball %>%
  mutate(monthyear=format(as.Date(date), "%Y-%m"))
  
summamb <- amball %>%
  drop_na(pctdelay60plus) %>%
  drop_na(pctdelay3060) %>%
  group_by(monthyear) %>%
  summarise(mean60plus=mean(pctdelay60plus), mean3060=mean(pctdelay3060), n=n())

# drop months with fewer than 10 observations
summamb <- filter(summamb, n>9)

p = ggplot(summamb) + 
  geom_line(aes(monthyear, mean60plus, group=1), color="red", lwd=2) +
  geom_line(aes(monthyear, mean3060, group=1), color="blue", lwd=2 ) +
  xlab("Month and year") +
  theme(axis.text.x=element_text(angle=90, hjust=1)) +
  ylab("% delayed")
p