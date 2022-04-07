#summary statistics for capped states
library(tidyverse)
library(vtable)


df <- read_csv("Data/Clean/studyDat.csv")

#View(df %>%
#  group_by(STATECENSUS) %>%
#  summarise(minT = min(EVENTt, na.rm = T),
#            maxT = max(EVENTt,na.rm = T)))


#UNWEIGHTED summary stats only for tau = -1 and implemented caps
capped <- df %>%
  filter(EVENTt == -1 & CAPGrp != 0) %>%
  #select target group
  filter(EDUCATIONi == 0 & MARSTi == 0) %>%
  #select summary stat ariables and weights
  select(AGE, RACE_ETH, NCHILD, YNGCH, ELDCH, BIRTHi, AHRSWORKT, EMPLi) %>%
#clean up and factorise
  mutate(RACE_ETH = as.factor(RACE_ETH),
         BIRTHi = as.factor(
           case_when(BIRTHi == 0 ~ "Did not give birth in the past year",
                     BIRTHi == 1 ~ "Gave birth in the past year")),
         EMPLi = as.factor(
           case_when(EMPLi == 0 | EMPLi == 2 ~ "Unemployed",
                     EMPLi == 1 ~ "Employed")),
         YNGCH = as.numeric(case_when(
           YNGCH != "No children" ~ YNGCH)),
         ELDCH = as.numeric(case_when(
           ELDCH != "No children" ~ ELDCH)))

labels <- c("Age", "Race", "Number of children", "Age of youngest child", "Age of eldest child", "Birth in the past year", "Hours spent at work last week", "Employment status")

sumtable(capped, labels = labels, title = "Summary statics for capped states when tau = -1", out = "latex")

#Get state level summaries
capped <- df %>%
  filter(EVENTt == -1 & CAPGrp != 0) %>%
  group_by(STATECENSUS) %>%
  summarise(maxben = max(MAXBENEFIT),
            femUnemp = max(femUnemployment),
            timeLim = as.factor(max(TIMELIMITi)),
            medWage = max(MEDIANWAGE)) %>%
  select(-c(STATECENSUS))

sumtable(capped, labels = c("Maximum welfare benefit for a family of 3", "Female unemployment rate", "Welfare time limits", "Median female wage"), title = "Summary statics for capped states when tau = -1", out = "latex")
