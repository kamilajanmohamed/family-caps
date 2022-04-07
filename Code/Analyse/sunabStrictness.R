#Sun Abahram DID estimator with strict treated states
library(tidyverse)
library(fixest)
library(stargazer)

#Source: https://lost-stats.github.io/Model_Estimation/Research_Design/event_study.html

#####All regressions on unmarried women aged 15-45 with high school or less: Linear probability models
#notes: YNGCH & ELDCH cause multicollinearity, push everything very close to 0 and raise standard errors. Adding weights did not change estimates, significance or R2. 

# Effect on birth ---------------------------------------------------------
df <- read_csv("Data/Clean/mergedBirth.csv")
#create a time to treatment variable. Set to 0 for never treated units
regDF <- df %>%
  #drop lenient states 
  filter(!STATEL %in% c("Connecticut", "Idaho", "Florida", "Wisconsin")) %>%
  #Create indicator of which states received treatment
  mutate(TREAT = case_when(
    CAPGrp == 0 ~ 0,
    CAPGrp %in% c(1,2) ~ 1)) %>%
  #shorten the window
  #filter(EVENTt >= -11 & EVENTt <= 6 & EDUCATIONi == 0 & MARSTi == 0) %>%
  filter(YEAR > 1981 & YEAR < 2011 & EDUCATIONi == 0 & MARSTi == 0)

#make a subset of treated with values for the desired event time window
treated <- regDF %>%
  filter(TREAT == 1 & EVENTt >= -11 & EVENTt <= 6)

untreated <- regDF %>%
  filter(TREAT == 0)

regDF <- rbind(treated, untreated) %>%
  # Following Sun and Abraham, we give our never-treated units a fake "treatment" date far outside the relevant study period.
  mutate(YEARCAPPED = case_when(
    TREAT == 1 ~ YEARCAPPED,
    TREAT == 0 ~ 10000)) %>%
  #sunab function needs never-treated to always have negative relative periods. 
  mutate(EVENTt = case_when(
    TREAT == 1 ~ EVENTt,
    TRUE ~ YEAR - YEARCAPPED))

birth <- feols(BIRTHi ~ sunab(YEARCAPPED, EVENTt) + AGE + RACE_ETH + NCHILD + TIMELIMITi + FEMUNEMP + MAXBENEFIT + MEDIANWAGE + lagMEDIANWAGE + lagUNEMP + lagBENEFIT|
                 STATECENSUS + YEAR,
               cluster = ~STATECENSUS,                          ## Clustered SEs
               data = regDF)
etable(summary(birth, agg = "ATT"))

# Repeat excluding in kind benefits ---------------------------------------
#create a time to treatment variable. Set to 0 for never treated units
regDF <- df %>%
  #drop lenient states 
  filter(!STATEL %in% c("Connecticut", "Idaho", "Florida", "Wisconsin", "Maryland", "Oklahoma", "South Carolina")) %>%
  #Create indicator of which states received treatment
  mutate(TREAT = case_when(
    CAPGrp == 0 ~ 0,
    CAPGrp %in% c(1,2) ~ 1)) %>%
  #shorten the window
  #filter(EVENTt >= -11 & EVENTt <= 6 & EDUCATIONi == 0 & MARSTi == 0) %>%
  filter(YEAR > 1981 & YEAR < 2011 & EDUCATIONi == 0 & MARSTi == 0)

#make a subset of treated with values for the desired event time window
treated <- regDF %>%
  filter(TREAT == 1 & EVENTt >= -11 & EVENTt <= 6)

untreated <- regDF %>%
  filter(TREAT == 0)

regDF <- rbind(treated, untreated) %>%
  # Following Sun and Abraham, we give our never-treated units a fake "treatment" date far outside the relevant study period.
  mutate(YEARCAPPED = case_when(
    TREAT == 1 ~ YEARCAPPED,
    TREAT == 0 ~ 10000)) %>%
  #sunab function needs never-treated to always have negative relative periods. 
  mutate(EVENTt = case_when(
    TREAT == 1 ~ EVENTt,
    TRUE ~ YEAR - YEARCAPPED))

birth <- feols(BIRTHi ~ sunab(YEARCAPPED, EVENTt) + AGE + RACE_ETH + NCHILD + TIMELIMITi + FEMUNEMP + MAXBENEFIT + MEDIANWAGE + lagMEDIANWAGE + lagUNEMP + lagBENEFIT|
                 STATECENSUS + YEAR,
               cluster = ~STATECENSUS,                          ## Clustered SEs
               data = regDF)
etable(summary(birth, agg = "ATT"))


# Effect on empl ----------------------------------------------------------
df <- read_csv("Data/Clean/mergedEmployment.csv")
#create a time to treatment variable. Set to 0 for never treated units
regDF <- df %>%
  #drop lenient states 
  filter(!STATEL %in% c("Connecticut", "Idaho", "Florida", "Wisconsin")) %>%
  #Create indicator of which states received treatment
  mutate(TREAT = case_when(
    CAPGrp == 0 ~ 0,
    CAPGrp %in% c(1,2) ~ 1)) %>%
  #shorten the window
  #filter(EVENTt >= -11 & EVENTt <= 6 & EDUCATIONi == 0 & MARSTi == 0) %>%
  filter(YEAR > 1981 & YEAR < 2011 & EDUCATIONi == 0 & MARSTi == 0)

#make a subset of treated with values for the desired event time window
treated <- regDF %>%
  filter(TREAT == 1 & EVENTt >= -11 & EVENTt <= 6)

untreated <- regDF %>%
  filter(TREAT == 0)

regDF <- rbind(treated, untreated) %>%
  # Following Sun and Abraham, we give our never-treated units a fake "treatment" date far outside the relevant study period.
  mutate(YEARCAPPED = case_when(
    TREAT == 1 ~ YEARCAPPED,
    TREAT == 0 ~ 10000)) %>%
  #sunab function needs never-treated to always have negative relative periods. 
  mutate(EVENTt = case_when(
    TREAT == 1 ~ EVENTt,
    TRUE ~ YEAR - YEARCAPPED))


empl1 <- feols(EMPLi ~ sunab(YEARCAPPED, EVENTt) + AGE + RACE_ETH + NCHILD + TIMELIMITi + FEMUNEMP + MAXBENEFIT + MEDIANWAGE+ lagMEDIANWAGE + lagUNEMP + lagBENEFIT|
                 STATECENSUS + YEAR,
               cluster = ~STATECENSUS,                          ## Clustered SEs
               data = regDF)
empl2 <- feols(EMPLi ~ sunab(YEARCAPPED, EVENTt) +
                 BIRTHi + AGE + RACE_ETH + NCHILD + TIMELIMITi + FEMUNEMP + MAXBENEFIT + MEDIANWAGE+ lagMEDIANWAGE + lagUNEMP + lagBENEFIT|
                 STATECENSUS + YEAR,
               cluster = ~STATECENSUS,                          ## Clustered SEs
               data = regDF)
etable(summary(empl1, agg = "ATT"), summary(empl2, agg = "ATT"), tex = T)

# Effect on ahrsworkt -----------------------------------------------------
regDF <- df %>%
  #drop lenient states 
  filter(!STATEL %in% c("Connecticut", "Idaho", "Florida", "Wisconsin")) %>%
  #Create indicator of which states received treatment
  mutate(TREAT = case_when(
    CAPGrp == 0 ~ 0,
    CAPGrp %in% c(1,2) ~ 1)) %>%
  #drop new jersey because ahrsworkt starts in 1989
  filter(STATEL != "New Jersey") %>%
  #shorten the window
  filter(EVENTt >= -11 & EVENTt <= 6 & EDUCATIONi == 0 & MARSTi == 0) %>%
  #change year bounds to reflect 11 years before 1994 states first full year (1995)
  filter(YEAR > 1983 & YEAR < 2011) %>%
  # Following Sun and Abraham, we give our never-treated units a fake "treatment" date far outside the relevant study period.
  mutate(YEARCAPPED = case_when(
    TREAT == 1 ~ YEARCAPPED,
    TREAT == 0 ~ 10000)) %>%
  #sunab function needs never-treated to always have negative relative periods.
  mutate(EVENTt = case_when(
    TREAT == 1 ~ EVENTt,
    TRUE ~ YEAR - YEARCAPPED))

ahrs1 <- feols(AHRSWORKT ~ sunab(YEARCAPPED, EVENTt) + AGE + RACE_ETH + NCHILD + TIMELIMITi + FEMUNEMP + MAXBENEFIT + MEDIANWAGE+ lagMEDIANWAGE + lagUNEMP + lagBENEFIT|
                 STATECENSUS + YEAR,
               cluster = ~STATECENSUS,## Clustered SEs
               data = regDF)

ahrs2 <- feols(AHRSWORKT ~ sunab(YEARCAPPED, EVENTt) + BIRTHi + AGE + RACE_ETH + NCHILD + TIMELIMITi + FEMUNEMP + MAXBENEFIT + MEDIANWAGE+ lagMEDIANWAGE + lagUNEMP + lagBENEFIT|
                 STATECENSUS + YEAR,
               cluster = ~STATECENSUS,## Clustered SEs
               data = regDF)

etable(summary(ahrs1, agg = "ATT"), summary(ahrs2, agg = "ATT"))


# Repeat excluding in kind benefits ---------------------------------------
#create a time to treatment variable. Set to 0 for never treated units
regDF <- df %>%
  #drop lenient states 
  filter(!STATEL %in% c("Connecticut", "Idaho", "Florida", "Wisconsin", "Maryland", "Oklahoma", "South Carolina")) %>%
  #Create indicator of which states received treatment
  mutate(TREAT = case_when(
    CAPGrp == 0 ~ 0,
    CAPGrp %in% c(1,2) ~ 1)) %>%
  #shorten the window
  #filter(EVENTt >= -11 & EVENTt <= 6 & EDUCATIONi == 0 & MARSTi == 0) %>%
  filter(YEAR > 1981 & YEAR < 2011 & EDUCATIONi == 0 & MARSTi == 0)

#make a subset of treated with values for the desired event time window
treated <- regDF %>%
  filter(TREAT == 1 & EVENTt >= -11 & EVENTt <= 6)

untreated <- regDF %>%
  filter(TREAT == 0)

regDF <- rbind(treated, untreated) %>%
  # Following Sun and Abraham, we give our never-treated units a fake "treatment" date far outside the relevant study period.
  mutate(YEARCAPPED = case_when(
    TREAT == 1 ~ YEARCAPPED,
    TREAT == 0 ~ 10000)) %>%
  #sunab function needs never-treated to always have negative relative periods. 
  mutate(EVENTt = case_when(
    TREAT == 1 ~ EVENTt,
    TRUE ~ YEAR - YEARCAPPED))

empl1 <- feols(EMPLi ~ sunab(YEARCAPPED, EVENTt) + AGE + RACE_ETH + NCHILD + TIMELIMITi + FEMUNEMP + MAXBENEFIT + MEDIANWAGE+ lagMEDIANWAGE + lagUNEMP + lagBENEFIT|
                 STATECENSUS + YEAR,
               cluster = ~STATECENSUS,                          ## Clustered SEs
               data = regDF)
empl2 <- feols(EMPLi ~ sunab(YEARCAPPED, EVENTt) +
                 BIRTHi + AGE + RACE_ETH + NCHILD + TIMELIMITi + FEMUNEMP + MAXBENEFIT + MEDIANWAGE+ lagMEDIANWAGE + lagUNEMP + lagBENEFIT|
                 STATECENSUS + YEAR,
               cluster = ~STATECENSUS,                          ## Clustered SEs
               data = regDF)
etable(summary(empl1, agg = "ATT"), summary(empl2, agg = "ATT"), tex = T)

# Effect on ahrsworkt -----------------------------------------------------
regDF <- df %>%
  #drop lenient states 
  filter(!STATEL %in% c("Connecticut", "Idaho", "Florida", "Wisconsin", "Maryland", "Oklahoma", "South Carolina")) %>%
  #Create indicator of which states received treatment
  mutate(TREAT = case_when(
    CAPGrp == 0 ~ 0,
    CAPGrp %in% c(1,2) ~ 1)) %>%
  #drop new jersey because ahrsworkt starts in 1989
  filter(STATEL != "New Jersey") %>%
  #shorten the window
  filter(EVENTt >= -11 & EVENTt <= 6 & EDUCATIONi == 0 & MARSTi == 0) %>%
  #change year bounds to reflect 11 years before 1994 states first full year (1995)
  filter(YEAR > 1983 & YEAR < 2011) %>%
  # Following Sun and Abraham, we give our never-treated units a fake "treatment" date far outside the relevant study period.
  mutate(YEARCAPPED = case_when(
    TREAT == 1 ~ YEARCAPPED,
    TREAT == 0 ~ 10000)) %>%
  #sunab function needs never-treated to always have negative relative periods.
  mutate(EVENTt = case_when(
    TREAT == 1 ~ EVENTt,
    TRUE ~ YEAR - YEARCAPPED))

ahrs1 <- feols(AHRSWORKT ~ sunab(YEARCAPPED, EVENTt) + AGE + RACE_ETH + NCHILD + TIMELIMITi + FEMUNEMP + MAXBENEFIT + MEDIANWAGE+ lagMEDIANWAGE + lagUNEMP + lagBENEFIT|
                 STATECENSUS + YEAR,
               cluster = ~STATECENSUS,## Clustered SEs
               data = regDF)

ahrs2 <- feols(AHRSWORKT ~ sunab(YEARCAPPED, EVENTt) + BIRTHi + AGE + RACE_ETH + NCHILD + TIMELIMITi + FEMUNEMP + MAXBENEFIT + MEDIANWAGE+ lagMEDIANWAGE + lagUNEMP + lagBENEFIT|
                 STATECENSUS + YEAR,
               cluster = ~STATECENSUS,## Clustered SEs
               data = regDF)

etable(summary(ahrs1, agg = "ATT"), summary(ahrs2, agg = "ATT"))

