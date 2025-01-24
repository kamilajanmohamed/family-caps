library(tidyverse)
library(fixest)
library(stargazer)
library(miceadds)


# SUNAB and TWFEDD on birth -----------------------------------------------
df <- read_csv("Data/Clean/mergedBirth.csv")

#create a time to treatment variable. Set to 0 for never treated units
regDF <- df %>%
  #Create indicator of which states received treatment
  mutate(TREAT = case_when(
    CAPGrp == 0 ~ 0,
    CAPGrp %in% c(1,2) ~ 1)) %>%
  #shorten the window
  #filter(EVENTt >= -11 & EVENTt <= 6 & EDUCATIONi == 0 & MARSTi == 0) %>%
  filter(YEAR > 1981 & YEAR < 2011 & proxyNeedStandard == 1)

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


# Sunab -------------------------------------------------------------------
IW <- feols(BIRTHi ~ sunab(YEARCAPPED, EVENTt) + AGE + RACE_ETH + NCHILD + TIMELIMITi + FEMUNEMP + MAXBENEFIT + MEDIANWAGE + lagMEDIANWAGE + lagUNEMP + lagBENEFIT|
              STATECENSUS + YEAR,
            cluster = ~STATECENSUS,                          ## Clustered SEs
            data = regDF)
etable(summary(IW, agg = "ATT"))

# excluding cash increments
regDF <- regDF %>%
  filter(!STATEL %in% c("Connecticut", "Idaho", "Florida", "Wisconsin"))
IW <- feols(BIRTHi ~ sunab(YEARCAPPED, EVENTt) + AGE + RACE_ETH + NCHILD + TIMELIMITi + FEMUNEMP + MAXBENEFIT + MEDIANWAGE + lagMEDIANWAGE + lagUNEMP + lagBENEFIT|
              STATECENSUS + YEAR,
            cluster = ~STATECENSUS,                          ## Clustered SEs
            data = regDF)
etable(summary(IW, agg = "ATT"))

# excluding all lenient states
regDF <- regDF %>%
  filter(!STATEL %in% c("Connecticut", "Idaho", "Florida", "Wisconsin", "Maryland", "Oklahoma", "South Carolina"))
IW <- feols(BIRTHi ~ sunab(YEARCAPPED, EVENTt) + AGE + RACE_ETH + NCHILD + TIMELIMITi + FEMUNEMP + MAXBENEFIT + MEDIANWAGE + lagMEDIANWAGE + lagUNEMP + lagBENEFIT|
              STATECENSUS + YEAR,
            cluster = ~STATECENSUS,                          ## Clustered SEs
            data = regDF)
etable(summary(IW, agg = "ATT"))

# TWFEDD ------------------------------------------------------------------
regDF <- df %>%
  #Create indicator of which states received treatment
  mutate(TREAT = case_when(
    CAPGrp == 0 ~ 0,
    CAPGrp %in% c(1,2) ~ 1)) %>%
  #shorten the window
  #filter(EVENTt >= -11 & EVENTt <= 6 & EDUCATIONi == 0 & MARSTi == 0) %>%
  filter(YEAR > 1981 & YEAR < 2011 & proxyNeedStandard == 1)

#make a subset of treated with values for the desired event time window
treated <- regDF %>%
  filter(TREAT == 1 & EVENTt >= -11 & EVENTt <= 6)
#note that time to treatment for the never treated is set to 0. 
untreated <- regDF %>%
  filter(TREAT == 0)

regDF <- rbind(treated, untreated) %>%
  # Following Sun and Abraham, we give our never-treated units a fake "treatment" date far outside the relevant study period.
  mutate(YEARCAPPED = case_when(
    TREAT == 1 ~ YEARCAPPED,
    TREAT == 0 ~ 0))

summary(lm.cluster(BIRTHi ~ TREAT + AGE + RACE_ETH + NCHILD + as.factor(TIMELIMITi) + FEMUNEMP + MAXBENEFIT + MEDIANWAGE + lagMEDIANWAGE + lagUNEMP + lagBENEFIT + as.factor(STATECENSUS) + as.factor(YEAR), cluster = "STATECENSUS", 
                   data = regDF))

# excluding cash increments
regDF <- regDF %>%
  filter(!STATEL %in% c("Connecticut", "Idaho", "Florida", "Wisconsin"))
summary(lm.cluster(BIRTHi ~ TREAT + AGE + RACE_ETH + NCHILD + as.factor(TIMELIMITi) + FEMUNEMP + MAXBENEFIT + MEDIANWAGE + lagMEDIANWAGE + lagUNEMP + lagBENEFIT + as.factor(STATECENSUS) + as.factor(YEAR), cluster = "STATECENSUS", 
                   data = regDF))

# excluding all lenient states
regDF <- regDF %>%
  filter(!STATEL %in% c("Connecticut", "Idaho", "Florida", "Wisconsin", "Maryland", "Oklahoma", "South Carolina"))
summary(lm.cluster(BIRTHi ~ TREAT + AGE + RACE_ETH + NCHILD + as.factor(TIMELIMITi) + FEMUNEMP + MAXBENEFIT + MEDIANWAGE + lagMEDIANWAGE + lagUNEMP + lagBENEFIT + as.factor(STATECENSUS) + as.factor(YEAR), cluster = "STATECENSUS", 
                   data = regDF))



# SUNAB TWFEDD on employment ----------------------------------------------
df <- read_csv("Data/Clean/mergedEmployment.csv")

#create a time to treatment variable. Set to 0 for never treated units
regDF <- df %>%
  #Create indicator of which states received treatment
  mutate(TREAT = case_when(
    CAPGrp == 0 ~ 0,
    CAPGrp %in% c(1,2) ~ 1)) %>%
  #shorten the window
  #filter(EVENTt >= -11 & EVENTt <= 6 & EDUCATIONi == 0 & MARSTi == 0) %>%
  filter(YEAR > 1981 & YEAR < 2011 & proxyNeedStandard == 1)

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


# Sunab -------------------------------------------------------------------
IW1 <- feols(EMPLi ~ sunab(YEARCAPPED, EVENTt) + AGE + RACE_ETH + NCHILD + TIMELIMITi + FEMUNEMP + MAXBENEFIT + MEDIANWAGE + lagMEDIANWAGE + lagUNEMP + lagBENEFIT |
              STATECENSUS + YEAR,
            cluster = ~STATECENSUS,                          ## Clustered SEs
            data = regDF)

IW2 <- feols(EMPLi ~ sunab(YEARCAPPED, EVENTt) + BIRTHi + AGE + RACE_ETH + NCHILD + TIMELIMITi + FEMUNEMP + MAXBENEFIT + MEDIANWAGE + lagMEDIANWAGE + lagUNEMP + lagBENEFIT |
               STATECENSUS + YEAR,
             cluster = ~STATECENSUS,                          ## Clustered SEs
             data = regDF)


etable(summary(IW1, agg = "ATT"), summary(IW2, agg = "ATT"), tex = T, digits = 3)

# excluding cash increments
regDF <- regDF %>%
  filter(!STATEL %in% c("Connecticut", "Idaho", "Florida", "Wisconsin"))

IW1 <- feols(EMPLi ~ sunab(YEARCAPPED, EVENTt) + AGE + RACE_ETH + NCHILD + TIMELIMITi + FEMUNEMP + MAXBENEFIT + MEDIANWAGE + lagMEDIANWAGE + lagUNEMP + lagBENEFIT |
               STATECENSUS + YEAR,
             cluster = ~STATECENSUS,                          ## Clustered SEs
             data = regDF)

IW2 <- feols(EMPLi ~ sunab(YEARCAPPED, EVENTt) + BIRTHi + AGE + RACE_ETH + NCHILD + TIMELIMITi + FEMUNEMP + MAXBENEFIT + MEDIANWAGE + lagMEDIANWAGE + lagUNEMP + lagBENEFIT |
               STATECENSUS + YEAR,
             cluster = ~STATECENSUS,                          ## Clustered SEs
             data = regDF)


etable(summary(IW1, agg = "ATT"), summary(IW2, agg = "ATT"), tex = T, digits = 3)

# excluding all lenient states
regDF <- regDF %>%
  filter(!STATEL %in% c("Connecticut", "Idaho", "Florida", "Wisconsin", "Maryland", "Oklahoma", "South Carolina"))

IW1 <- feols(EMPLi ~ sunab(YEARCAPPED, EVENTt) + AGE + RACE_ETH + NCHILD + TIMELIMITi + FEMUNEMP + MAXBENEFIT + MEDIANWAGE + lagMEDIANWAGE + lagUNEMP + lagBENEFIT |
               STATECENSUS + YEAR,
             cluster = ~STATECENSUS,                          ## Clustered SEs
             data = regDF)

IW2 <- feols(EMPLi ~ sunab(YEARCAPPED, EVENTt) + BIRTHi + AGE + RACE_ETH + NCHILD + TIMELIMITi + FEMUNEMP + MAXBENEFIT + MEDIANWAGE + lagMEDIANWAGE + lagUNEMP + lagBENEFIT |
               STATECENSUS + YEAR,
             cluster = ~STATECENSUS,                          ## Clustered SEs
             data = regDF)


etable(summary(IW1, agg = "ATT"), summary(IW2, agg = "ATT"), tex = T, digits = 3)


# TWFEDD ------------------------------------------------------------------
regDF <- df %>%
  #Create indicator of which states received treatment
  mutate(TREAT = case_when(
    CAPGrp == 0 ~ 0,
    CAPGrp %in% c(1,2) ~ 1)) %>%
  #shorten the window
  #filter(EVENTt >= -11 & EVENTt <= 6 & EDUCATIONi == 0 & MARSTi == 0) %>%
  filter(YEAR > 1981 & YEAR < 2011 & proxyNeedStandard == 1)

#make a subset of treated with values for the desired event time window
treated <- regDF %>%
  filter(TREAT == 1 & EVENTt >= -11 & EVENTt <= 6)
#note that time to treatment for the never treated is set to 0. 
untreated <- regDF %>%
  filter(TREAT == 0)

regDF <- rbind(treated, untreated) %>%
  # Following Sun and Abraham, we give our never-treated units a fake "treatment" date far outside the relevant study period.
  mutate(YEARCAPPED = case_when(
    TREAT == 1 ~ YEARCAPPED,
    TREAT == 0 ~ 0))

summary(lm.cluster(EMPLi ~ TREAT + AGE + RACE_ETH + NCHILD + as.factor(TIMELIMITi) + FEMUNEMP + MAXBENEFIT + MEDIANWAGE + lagMEDIANWAGE + lagUNEMP + lagBENEFIT + as.factor(STATECENSUS) + as.factor(YEAR), cluster = "STATECENSUS", 
                   data = regDF))

summary(lm.cluster(EMPLi ~ TREAT + BIRTHi + AGE + RACE_ETH + NCHILD + as.factor(TIMELIMITi) + FEMUNEMP + MAXBENEFIT + MEDIANWAGE + lagMEDIANWAGE + lagUNEMP + lagBENEFIT+ as.factor(STATECENSUS) + as.factor(YEAR), cluster = "STATECENSUS", 
                   data = regDF))

# excluding cash increments
regDF <- regDF %>%
  filter(!STATEL %in% c("Connecticut", "Idaho", "Florida", "Wisconsin"))
summary(lm.cluster(EMPLi ~ TREAT + AGE + RACE_ETH + NCHILD + as.factor(TIMELIMITi) + FEMUNEMP + MAXBENEFIT + MEDIANWAGE + lagMEDIANWAGE + lagUNEMP + lagBENEFIT + as.factor(STATECENSUS) + as.factor(YEAR), cluster = "STATECENSUS", 
                   data = regDF))

summary(lm.cluster(EMPLi ~ TREAT + BIRTHi + AGE + RACE_ETH + NCHILD + as.factor(TIMELIMITi) + FEMUNEMP + MAXBENEFIT + MEDIANWAGE + lagMEDIANWAGE + lagUNEMP + lagBENEFIT+ as.factor(STATECENSUS) + as.factor(YEAR), cluster = "STATECENSUS", 
                   data = regDF))


# excluding all lenient states
regDF <- regDF %>%
  filter(!STATEL %in% c("Connecticut", "Idaho", "Florida", "Wisconsin", "Maryland", "Oklahoma", "South Carolina"))
summary(lm.cluster(EMPLi ~ TREAT + AGE + RACE_ETH + NCHILD + as.factor(TIMELIMITi) + FEMUNEMP + MAXBENEFIT + MEDIANWAGE + lagMEDIANWAGE + lagUNEMP + lagBENEFIT + as.factor(STATECENSUS) + as.factor(YEAR), cluster = "STATECENSUS", 
                   data = regDF))

summary(lm.cluster(EMPLi ~ TREAT + BIRTHi + AGE + RACE_ETH + NCHILD + as.factor(TIMELIMITi) + FEMUNEMP + MAXBENEFIT + MEDIANWAGE + lagMEDIANWAGE + lagUNEMP + lagBENEFIT+ as.factor(STATECENSUS) + as.factor(YEAR), cluster = "STATECENSUS", 
                   data = regDF))

