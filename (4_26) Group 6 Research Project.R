library(tidyverse)
library(ipumsr)
library(dplyr, warn.conflicts = FALSE)

#setwd("C:/Users/Owner/Desktop/GT/Spring 2021/ECON 4421 Lee/urban_sp_2021/data")

#xml file is usa_00007 and the .dat.gz file is usa_00005

ddi2 <- read_ipums_ddi("data/usa_00007.xml")

data <- read_ipums_micro(ddi2)


#Took out unobservable regions, states, poverty statuses, and non-accessible 
#wages and income
data1 <- data %>% filter(MET2013!=0, INCTOT!=9999999, POVERTY!=000, 
                         REGION!=99, STATEFIP!=99, INCWAGE!=999999, 
                         INCWAGE!=999998, REGION!=97) %>% 
#Labeling the numbers of RACE
  mutate(Race = 
           if_else(RACE==1, "White", 
           if_else(RACE==2, "Black",
           if_else(RACE==3,"American Indian/Alaskan Native", 
           if_else(RACE==4, "Chinese", 
           if_else(RACE==5, "Japanese", 
           if_else(RACE==6, "Other Asian or Pacific Islander", 
           if_else(RACE==7, "Other Race, nec",
           if_else(RACE==8, "Two Major Races", "Three or More Major Races"
           )))))))))


poptotal <- data1 %>% group_by(MET2013) %>% summarise(totalpop=sum(PERWT))

data1$RACE = as.factor(data1$RACE)

#How has the average income of each race changed over time?

data1 %>%
  group_by(MULTYEAR,Race) %>% 
  summarise(avg_inc = weighted.mean(INCTOT, w = PERWT)) %>%
  ggplot() + geom_line(aes(x = MULTYEAR, y = avg_inc, color = Race)) + 
  scale_y_continuous(labels = scales::label_comma(scale=.001, prefix="$", 
                                                  suffix="k")) +
  labs(title = "Average Income Over Time by Race",
       x = "Year", y = "Average Income")

#How has the average income of each race changed over time excluding those
#with zero or negative wages?

data1 %>% filter(INCTOT>0) %>% group_by(MULTYEAR,Race) %>% 
  summarise(avg_inc = weighted.mean(INCTOT, w = PERWT)) %>%
  ggplot() + geom_line(aes(x = MULTYEAR, y = avg_inc, color = Race)) + 
  scale_y_continuous(labels = scales::label_comma(scale=.001, prefix="$", 
                                                  suffix="k")) +
  labs(title = "Average Income Over Time by Race (Excluding Negative Income)",
       x = "Year", y = "Average Income")

#How health coverage changes over time by race as a percentage?

data1 %>% group_by(MULTYEAR, Race, HCOVANY) %>% 
  mutate(coverage = if_else(HCOVANY==2, "Covered", "Not Covered")) %>% 
  ggplot(aes(x = Race)) + 
  geom_bar(aes(fill = coverage, weight = PERWT), position = "fill") + 
  facet_wrap(~MULTYEAR) + 
  labs(title = "Health Insurance Coverage by Race Over Time", 
       y = "Percentage") +
  scale_fill_discrete(name = "Health Insurance") + 
  scale_y_continuous(labels = scales::label_comma(scale=100, suffix="%")) +
  theme_bw() + coord_flip()

#What is the percentage of people who have public vs. private insurance by race?

data1 %>% group_by(MULTYEAR, Race, HCOVPRIV, HCOVPUB) %>% 
  mutate(hcoverage = if_else(HCOVPRIV==2 & HCOVPUB == 2, "Both",
                            if_else(HCOVPRIV==2, "Private",
                                    if_else(HCOVPUB == 2, "Public",
                                            "Neither")))) %>%  
  ggplot(aes(x = Race)) + 
  geom_bar(aes(fill = hcoverage, weight = PERWT), position = "fill") + 
  coord_flip() + facet_wrap(~MULTYEAR) + 
  labs(title = "Public vs Private Health Insurance Coverage by Race Over Time", 
       y = "Percentage", fill = "Health Insurance Status") +
  #scale_fill_discrete(name = "Health Insurance Status") + 
  scale_y_continuous(labels = scales::label_comma(scale=100, suffix="%")) +
  theme_bw()

#How has health insurance coverage changed over time?

data1 %>% group_by(MULTYEAR,Race) %>% 
  mutate(tot_coverage = sum(PERWT)) %>%
  filter(HCOVANY!=1) %>% 
  mutate(yes_coverage = sum(PERWT)) %>%
  summarise(coverage = (yes_coverage/tot_coverage)*100) %>% 
  distinct() %>%
  ggplot() + geom_line(aes(x = MULTYEAR, y = coverage, color = Race)) + 
  scale_y_continuous(labels = scales::label_comma(suffix="%")) +
  labs(title = "Health Insurance Coverage Over Time by Race",
       x = "Year", y = "Percent Covered by Health Insurance")
  
#Health Insurance Coverage vs Income
#HCOVANY is in 1 or 2 format. HCOVANY2 makes it into a 1 or 0 format.
data1 %>%  mutate(HCOVANY2=if_else(HCOVANY>1, 1, 0)) %>% 
  group_by(MET2013, Race) %>%
  summarise(healthinsurance_sh=weighted.mean(HCOVANY2, w=PERWT), 
            avg_wage=weighted.mean(INCTOT, w=PERWT), pop=sum(PERWT)) %>%
  filter(pop>1000) %>% 
  ggplot(aes(healthinsurance_sh, avg_wage)) + 
  geom_point(aes(fill=Race, color=Race)) + 
  geom_smooth(aes(color=Race)) + 
  labs(title="Health Insurance Coverage vs. Income", y="Avg. Income",
       x="Share of residents with Health Insurance") +
  scale_x_continuous(labels=scales::label_comma(scale=100, suffix="%")) +
  scale_y_continuous(labels=scales::label_comma(scale=0.001, prefix="$", 
                                                suffix="K")) +
  theme(legend.position=c(0.16,0.76), legend.title=element_blank())

#Points Only
data1 %>%  mutate(HCOVANY2=if_else(HCOVANY>1, 1, 0)) %>% 
  group_by(MET2013, Race) %>%
  summarise(healthinsurance_sh=weighted.mean(HCOVANY2, w=PERWT), 
            avg_wage=weighted.mean(INCTOT, w=PERWT), pop=sum(PERWT)) %>%
  filter(pop>1000) %>% 
  ggplot(aes(healthinsurance_sh, avg_wage)) + 
  geom_point(aes(fill=Race, color=Race)) + 
  labs(title="Health Insurance Coverage vs. Income", y="Avg. Income",
       x="Share of residents with Health Insurance") +
  scale_x_continuous(labels=scales::label_comma(scale=100, suffix="%")) +
  scale_y_continuous(labels=scales::label_comma(scale=0.001, prefix="$", 
                                                suffix="K")) +
  theme(legend.position=c(0.16,0.76), legend.title=element_blank())

#Smooth Only
data1 %>%  mutate(HCOVANY2=if_else(HCOVANY>1, 1, 0)) %>% 
  group_by(MET2013, Race) %>%
  summarise(healthinsurance_sh=weighted.mean(HCOVANY2, w=PERWT), 
            avg_wage=weighted.mean(INCTOT, w=PERWT), pop=sum(PERWT)) %>%
  filter(pop>1000) %>% 
  ggplot(aes(healthinsurance_sh, avg_wage)) +
  geom_smooth(aes(color=Race)) + 
  labs(title="Health Insurance Coverage vs. Income", y="Avg. Income",
       x="Share of residents with Health Insurance") +
  scale_x_continuous(labels=scales::label_comma(scale=100, suffix="%")) +
  scale_y_continuous(labels=scales::label_comma(scale=0.001, prefix="$", 
                                                suffix="K")) +
  theme(legend.position=c(0.16,0.76), legend.title=element_blank())




