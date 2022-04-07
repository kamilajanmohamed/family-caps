#calculate birth rates by year and state for all women, less educated women, unmarried women and less educated unmarried women

library(ipumsr)
library(tidyverse)
library(grid)
library(ggnewscale)
library(gtable)
library(cowplot)

if (!require("ipumsr")) stop("Reading IPUMS data into R requires the ipumsr package. It can be installed using the following command: install.packages('ipumsr')")

ddi <- read_ipums_ddi("Data/Raw/CPS/cps_00003.xml")
data <- read_ipums_micro(ddi) %>%
  mutate(YEAR = YEAR-1)

# All people --------------------------------------------------------------
df <- data
#create variable for number of people per state
df$Female <- ifelse(df$SEX==2,  df$WTFINL, 0)
df$Male <- ifelse(df$SEX==1,  df$WTFINL, 0)
df$FemBirth <- ifelse(df$SEX==2 & df$YNGCH == 0, df$WTFINL, 0)

agg <- df %>%
  group_by(STATECENSUS, YEAR) %>%
  summarise(nF = sum(Female), 
            nM = sum(Male),
            nBirth = sum(FemBirth)) %>%
  #express birth rate as births per 1000 women
  mutate(birthrateFem = 1000*nBirth/nF,
         birthrateOverall = 1000*nBirth/(nF + nM)) %>%
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
    CAPGrp == 2 & YEAR < YEARREPEALED ~ YEAR - YEARCAPPED-1)) %>%
  mutate(treat = case_when(
    CAPGrp != 0 ~ 0,
    TRUE ~ 1))

fullSample <- agg %>%
  dplyr::select(STATECENSUS, YEAR, birthrateFem, YEARCAPPED, treat) %>%
  mutate(type = 'All women')

# Less educated women ------------------------------------------------------
df <- data

df$Female <- ifelse(df$SEX==2 & df$EDUC < 74,  df$WTFINL, 0)
df$FemBirth <- ifelse(df$SEX==2 & df$EDUC < 74 & df$YNGCH == 0, df$WTFINL, 0)

agg <- df %>%
  group_by(STATECENSUS, YEAR) %>%
  summarise(nF = sum(Female), 
            nBirth = sum(FemBirth)) %>%
  #express birth rate as births per 1000 women
  mutate(birthrateFem = 1000*nBirth/nF) %>%
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
    CAPGrp == 2 & YEAR < YEARREPEALED ~ YEAR - YEARCAPPED-1)) %>%
  mutate(treat = case_when(
    CAPGrp != 0 ~ 0,
    TRUE ~ 1))

lessEduc <- agg %>%
  dplyr::select(STATECENSUS, YEAR, birthrateFem, YEARCAPPED, treat) %>%
  mutate(type = "Less educated women")

# Single birth rate -------------------------------------------------------
df <- data

df$Female <- ifelse(df$SEX==2 & df$MARST > 2 & df$MARST <= 9,  df$WTFINL, 0)
df$FemBirth <- ifelse(df$SEX==2 & df$MARST > 2 & df$MARST <= 9  & df$YNGCH == 0, df$WTFINL, 0)

agg <- df %>%
  group_by(STATECENSUS, YEAR) %>%
  summarise(nF = sum(Female), 
            nBirth = sum(FemBirth)) %>%
  #express birth rate as births per 1000 women
  mutate(birthrateFem = 1000*nBirth/nF) %>%
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
    CAPGrp == 2 & YEAR < YEARREPEALED ~ YEAR - YEARCAPPED-1)) %>%
  mutate(treat = case_when(
    CAPGrp != 0 ~ 0,
    TRUE ~ 1))

unmarried <- agg %>%
  dplyr::select(STATECENSUS, YEAR, birthrateFem, YEARCAPPED, treat) %>%
  mutate(type = "Unmarried women")
# Less educated, unmarried ------------------------------------------------
df <- data

df$Female <- ifelse(df$SEX==2  & df$MARST > 2 & df$MARST <= 9 & df$EDUC < 74,  df$WTFINL, 0)
df$FemBirth <- ifelse(df$SEX==2 & df$MARST > 2 & df$MARST <= 9  & df$EDUC < 74 & df$YNGCH == 0, df$WTFINL, 0)

agg <- df %>%
  group_by(STATECENSUS, YEAR) %>%
  summarise(nF = sum(Female), 
            nBirth = sum(FemBirth)) %>%
  #express birth rate as births per 1000 women
  mutate(birthrateFem = 1000*nBirth/nF) %>%
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
    CAPGrp == 2 & YEAR < YEARREPEALED ~ YEAR - YEARCAPPED-1)) %>%
  mutate(treat = case_when(
    CAPGrp != 0 ~ 0,
    TRUE ~ 1))

lessEducUnmarried <- agg %>%
  dplyr::select(STATECENSUS, YEAR, birthrateFem, YEARCAPPED, treat) %>%
  mutate(type = "Unmarried, less-educated women")

# Less educated, married ------------------------------------------------
df <- data

df$Female <- ifelse(df$SEX==2  & df$MARST <=2 & df$EDUC < 74,  df$WTFINL, 0)
df$FemBirth <- ifelse(df$SEX==2 & df$MARST <=2  & df$EDUC < 74 & df$YNGCH == 0, df$WTFINL, 0)

agg <- df %>%
  group_by(STATECENSUS, YEAR) %>%
  summarise(nF = sum(Female), 
            nBirth = sum(FemBirth)) %>%
  #express birth rate as births per 1000 women
  mutate(birthrateFem = 1000*nBirth/nF) %>%
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
    CAPGrp == 2 & YEAR < YEARREPEALED ~ YEAR - YEARCAPPED-1)) %>%
  mutate(treat = case_when(
    CAPGrp != 0 ~ 0,
    TRUE ~ 1))

lessEducMarried <- agg %>%
  dplyr::select(STATECENSUS, YEAR, birthrateFem, YEARCAPPED, treat) %>%
  mutate(type = "Married, less-educated women")

# More educated, married ------------------------------------------------
df <- data

df$Female <- ifelse(df$SEX==2  & df$MARST <=2 & df$EDUC > 73 & df$EDUC < 999,  df$WTFINL, 0)
df$FemBirth <- ifelse(df$SEX==2 & df$MARST <=2  & df$EDUC > 73 & df$EDUC < 999 & df$YNGCH == 0, df$WTFINL, 0)

agg <- df %>%
  group_by(STATECENSUS, YEAR) %>%
  summarise(nF = sum(Female), 
            nBirth = sum(FemBirth)) %>%
  #express birth rate as births per 1000 women
  mutate(birthrateFem = 1000*nBirth/nF) %>%
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
    CAPGrp == 2 & YEAR < YEARREPEALED ~ YEAR - YEARCAPPED-1)) %>%
  mutate(treat = case_when(
    CAPGrp != 0 ~ 0,
    TRUE ~ 1))

moreEducMarried <- agg %>%
  dplyr::select(STATECENSUS, YEAR, birthrateFem, YEARCAPPED, treat) %>%
  mutate(type = "Married, more-educated women")

# More educated, unmarried ------------------------------------------------
df <- data

df$Female <- ifelse(df$SEX==2  & df$MARST > 2 & df$MARST <= 9 & df$EDUC > 73 & df$EDUC < 999,  df$WTFINL, 0)
df$FemBirth <- ifelse(df$SEX==2 & df$MARST > 2 & df$MARST <= 9  & df$EDUC > 73 & df$EDUC < 999 & df$YNGCH == 0, df$WTFINL, 0)

agg <- df %>%
  group_by(STATECENSUS, YEAR) %>%
  summarise(nF = sum(Female), 
            nBirth = sum(FemBirth)) %>%
  #express birth rate as births per 1000 women
  mutate(birthrateFem = 1000*nBirth/nF) %>%
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
    CAPGrp == 2 & YEAR < YEARREPEALED ~ YEAR - YEARCAPPED-1)) %>%
  mutate(treat = case_when(
    CAPGrp != 0 ~ 0,
    TRUE ~ 1))

moreEducUnmarried <- agg %>%
  dplyr::select(STATECENSUS, YEAR, birthrateFem, YEARCAPPED, treat) %>%
  mutate(type = "Unmarried, more-educated women")

# Merge datasets ----------------------------------------------------------
birthRates <- rbind(fullSample, lessEducUnmarried, lessEducMarried, moreEducUnmarried, moreEducMarried) %>%
  #drop data before 1982 because it's somehow 0
  filter(YEAR > 1981 & YEAR < 2010)


# Export dataset ----------------------------------------------------------
#write.csv(birthRates, "Data/Clean/groupedBirthRates.csv")


# Compute & Plot means by treatment ----------------------------------------
means <- birthRates %>%
  group_by(type, YEAR, treat) %>%
  summarise(meanBirthRate = mean(birthrateFem)) %>%
  mutate(treat = as.factor(treat))

ggplot(means, aes(x = YEAR, y = meanBirthRate, group = treat, colour = treat)) +
  geom_line() +
  facet_wrap(~type, scales = "free") + 
  theme_bw() +
  theme(legend.position = "bottom")


# Add function to put legend in empty facet cell --------------------------
shift_legend <- function(p){
  
  # check if p is a valid object
  if(!"gtable" %in% class(p)){
    if("ggplot" %in% class(p)){
      gp <- ggplotGrob(p) # convert to grob
    } else {
      message("This is neither a ggplot object nor a grob generated from ggplotGrob. Returning original plot.")
      return(p)
    }
  } else {
    gp <- p
  }
  
  # check for unfilled facet panels
  facet.panels <- grep("^panel", gp[["layout"]][["name"]])
  empty.facet.panels <- sapply(facet.panels, function(i) "zeroGrob" %in% class(gp[["grobs"]][[i]]))
  empty.facet.panels <- facet.panels[empty.facet.panels]
  if(length(empty.facet.panels) == 0){
    message("There are no unfilled facet panels to shift legend into. Returning original plot.")
    return(p)
  }
  
  # establish extent of unfilled facet panels (including any axis cells in between)
  empty.facet.panels <- gp[["layout"]][empty.facet.panels, ]
  empty.facet.panels <- list(min(empty.facet.panels[["t"]]), min(empty.facet.panels[["l"]]),
                             max(empty.facet.panels[["b"]]), max(empty.facet.panels[["r"]]))
  names(empty.facet.panels) <- c("t", "l", "b", "r")
  
  # extract legend & copy over to location of unfilled facet panels
  guide.grob <- which(gp[["layout"]][["name"]] == "guide-box")
  if(length(guide.grob) == 0){
    message("There is no legend present. Returning original plot.")
    return(p)
  }
  gp <- gtable_add_grob(x = gp,
                        grobs = gp[["grobs"]][[guide.grob]],
                        t = empty.facet.panels[["t"]],
                        l = empty.facet.panels[["l"]],
                        b = empty.facet.panels[["b"]],
                        r = empty.facet.panels[["r"]],
                        name = "new-guide-box")
  
  # squash the original guide box's row / column (whichever applicable)
  # & empty its cell
  guide.grob <- gp[["layout"]][guide.grob, ]
  if(guide.grob[["l"]] == guide.grob[["r"]]){
    gp <- gtable_squash_cols(gp, cols = guide.grob[["l"]])
  }
  if(guide.grob[["t"]] == guide.grob[["b"]]){
    gp <- gtable_squash_rows(gp, rows = guide.grob[["t"]])
  }
  gp <- gtable_remove_grobs(gp, "guide-box")
  
  return(gp)
}


# Compute and plot by cohort ----------------------------------------------
cohortMeans <- birthRates %>%
  mutate(YEARCAPPED = case_when(
    is.na(YEARCAPPED) == T ~ "Never treated",
    TRUE ~ as.character(YEARCAPPED))) %>%
  group_by(type, YEAR, YEARCAPPED) %>%
  summarise(meanBirthRate = mean(birthrateFem)) %>%
  mutate(YEARCAPPED = as.factor(YEARCAPPED))

fig <- ggplot(data = cohortMeans, aes(x = YEAR, y = meanBirthRate, group = YEARCAPPED)) +
  geom_point(aes(colour = YEARCAPPED), alpha = 0.7) +
  scale_colour_manual(values = c("#8dd3c7", "lightgoldenrod2", "#bebada", "#fb8072", "#80b1d3", "#fdb462", "#b3de69", "#fccde5")) +
  labs(colour = "Cohort") +
  new_scale_color()+
  geom_line(data = means, aes(x = YEAR, y = meanBirthRate, group = treat, colour = treat), size = 1) +
  scale_colour_manual(values = c("blue", "red"), labels = c("Untreated states", "Treated states")) +
  labs(colour = "") +
  facet_wrap(~type, scales = "free", nrow = 3, ncol = 2) +
  labs(y = "Births per 1000 women", x = "Year") +
  scale_x_continuous(breaks = seq(1982, 2010, 4), labels = seq(1982, 2010, 4)) +
  geom_vline(xintercept = 1992, colour = "black", linetype = "longdash") +
  theme_bw() +
  theme(legend.box = "horizontal",
        panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_blank())

grid.draw(shift_legend(fig))

#ggsave("Output/Figures/subgroupMeans.png", groupMeans, width = 6, height = 9)



