library(ipumsr)
library(tidyverse)
library(labelled)
library(bacondecomp)

#for goodman bacon to work, you need a balanced dataset = the same number of observations per period so we need to find the birth rate among women aged 18-45 with lower education each year for each state

if (!require("ipumsr")) stop("Reading IPUMS data into R requires the ipumsr package. It can be installed using the following command: install.packages('ipumsr')")

ddi <- read_ipums_ddi("Data/Raw/CPS/cps_00003.xml")
data <- read_ipums_micro(ddi)

df <- data

#create variable for number of females in target group per state births 
df$targetFem <- ifelse(df$SEX==2 & df$AGE >= 18 & df$AGE <= 45 & df$EDUC < 74 & df$MARST > 2 & df$MARST <= 9, df$WTFINL, 0)
df$targetFemBirth <- ifelse(df$SEX==2 & df$AGE >= 18 & df$AGE <= 45 & df$EDUC < 74 & df$YNGCH == 0 & df$MARST > 2 & df$MARST <= 9, df$WTFINL, 0)

#calculate number of target women per state per year, number of births among them and birth rate
agg <- df %>%
  #lag it since birth = birth at t-1
  mutate(YEAR = YEAR - 1) %>%
  group_by(STATECENSUS, YEAR) %>%
  summarise(nTargetF = sum(targetFem), 
            nBirth = sum(targetFemBirth)) %>%
  #express birth rate as births per 1000 women
  mutate(birthrate = 1000*nBirth/nTargetF) %>%
  #add family cap information
  #add a year capped variable 
  mutate(
    YEARCAPPED = case_when(
      STATECENSUS %in% c(22)~ 1992,
      STATECENSUS %in% c(71, 58)~ 1994,
      STATECENSUS %in% c(86, 51, 32, 64, 54, 14, 46) ~ 1995,
      STATECENSUS %in% c(16, 59, 56, 62, 35, 52, 33) ~ 1996,
      STATECENSUS %in% c(93, 82, 57, 73, 83)~ 1997,
      STATECENSUS %in% c(44)~ 1998,
      STATECENSUS %in% c(41)~ 2003),
    YEARREPEALED  = case_when(
      STATECENSUS == 93 ~ (2016),
      STATECENSUS == 33 ~ (2004),
      STATECENSUS == 52 ~ (2004),
      STATECENSUS == 14 ~ (2019),
      STATECENSUS == 41 ~ (2015),
      STATECENSUS == 46 ~ (2007),
      STATECENSUS == 22 ~ (2020),
      STATECENSUS == 73 ~ (2009),
      STATECENSUS == 83 ~ (2008)
    )) %>%
  #add an indicator for capped years: 1 begins at the first full year of cap
  mutate(CAPi = case_when(
    #not capped
    is.na(YEARCAPPED) == F & YEAR <= YEARCAPPED ~ 0,
    #capped
    is.na(YEARCAPPED) == F  & is.na(YEARREPEALED) == T & YEAR > YEARCAPPED ~ 1,
    #capped before repeal
    is.na(YEARCAPPED) == F & is.na(YEARREPEALED) == F & YEAR <= YEARREPEALED & YEAR > YEARCAPPED ~ 1,
    #uncapped after repeal beginning first year after repeal
    is.na(YEARCAPPED) == F & is.na(YEARREPEALED) == F & YEAR > YEARREPEALED ~ 0
  )) %>%
  #create family cap variable
  mutate(CAPGrp = as.factor(case_when(
    #never capped
    is.na(YEARCAPPED) == T ~ 0,
    #capped
    is.na(YEARCAPPED) == F & is.na(YEARREPEALED) == T ~ 1,
    #repealed
    is.na(YEARCAPPED) == F & is.na(YEARREPEALED) == F ~ 2))) %>%
  #Adding event time variable 
  mutate(EVENTt = case_when(
    is.na(YEARCAPPED) == TRUE ~ 0,
    CAPGrp == 1 ~ YEAR - YEARCAPPED-1,
    CAPGrp == 2 & YEAR < YEARREPEALED ~ YEAR - YEARCAPPED-1))

write_csv(agg, "Data/Clean/stateAgg.csv")
