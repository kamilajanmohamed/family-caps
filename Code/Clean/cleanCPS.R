# Author: Kamila Janmohamed, 26 December 2021
# Purpose: Clean data
library(ipumsr)
library(tidyverse)
library(labelled)

if (!require("ipumsr")) stop("Reading IPUMS data into R requires the ipumsr package. It can be installed using the following command: install.packages('ipumsr')")

ddi <- read_ipums_ddi("Data/Raw/CPS/cps_00003.xml")
data <- read_ipums_micro(ddi)

cps <- data %>%
  #keep only females
  filter(SEX == 2) %>%
  #restrict age to 18 and 45
  filter(AGE >= 18 & AGE <= 45) %>%
  #define educated dummy as 0 = high school or less.
  mutate(EDUCATIONi = as.factor(case_when(
    EDUC < 74 ~ 0,
    EDUC > 73 & EDUC < 999 ~ 1))) %>%
  #define child under 1 dummy 
  mutate(BIRTHi = as.factor(case_when(
    YNGCH == 0 ~ 1,
    YNGCH > 0 & YNGCH <= 99 ~ 0))) %>%
  #define married/single dummy
  mutate(MARSTi = as.factor(case_when(
    MARST <= 2 ~ 1,
    MARST > 2 & MARST <= 9 ~ 0))) %>%
  #employment dummy 
  mutate(EMPLi = as.factor(case_when(
    EMPSTAT %in% c(1, 10, 12) ~ 1, #employed
    TRUE ~ 0 #unemployed + not in labour force
    #EMPSTAT %in% c(30, 31, 32, 33, 34, 35, 36) ~ 2 #not in labour force
    ))) %>%
  #turn age of oldest/youngest into a factor
  mutate(YNGCH = as.factor(case_when(
    YNGCH == 99 ~ "No children",
    TRUE ~ as.character(YNGCH))),
    ELDCH = as.factor(case_when(
      ELDCH == 99 ~ "No children",
      TRUE ~ as.character(ELDCH)))) %>%
  #create a new race variable
  mutate(RACE_ETH = case_when(
    RACE == 100 ~ "White",
    RACE == 200 ~ "Black",
    RACE != 100 & RACE != 200 & HISPAN != 0 & HISPAN < 901 ~ "Hispanic",
    TRUE ~ "Other")) %>%
  #tidy up the ahrsworkt variable so  = 0 for unemployed people after 1988 and NA before that since the variable was introduced in 1989
  mutate(AHRSWORKT = case_when(
    EMPLi == 1 & YEAR > 1988 & AHRSWORKT != 999  ~ AHRSWORKT,
    EMPLi == 1 & YEAR > 1988 & AHRSWORKT == 999  ~ 0)) %>%
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
    CAPGrp == 2 & YEAR < YEARREPEALED ~ YEAR - YEARCAPPED-1)) %>%
  dplyr::select(YEAR, STATECENSUS, WTFINL, CPSIDP, AGE, RACE_ETH, NCHILD, ELDCH, YNGCH, AHRSWORKT, EDUCATIONi, BIRTHi, MARSTi, EMPLi, CAPGrp, CAPi, YEARCAPPED, YEARREPEALED, EVENTt, FAMSIZE,FAMINC) %>%
  mutate(STATEL = to_character(STATECENSUS),
         FAMINC = to_character(FAMINC))

write_csv(cps, "Data/Clean/cpsEmployment.csv")


# Lag the year since birth is in t-1 and reassign all event time variables -----------------------
cps2 <- cps %>%
  mutate(YEAR = YEAR - 1) %>%
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
  mutate(EVENTt = case_when(
    is.na(YEARCAPPED) == TRUE ~ 0,
    CAPGrp == 1 ~ YEAR - YEARCAPPED-1,
    CAPGrp == 2 & YEAR < YEARREPEALED ~ YEAR - YEARCAPPED-1)) %>%
  dplyr::select(-c(EMPLi, AHRSWORKT))

write_csv(cps2, "Data/Clean/cpsBirth.csv")
  


