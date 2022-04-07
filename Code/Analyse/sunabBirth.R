#Sun Abahram DID estimator
library(tidyverse)
library(fixest)
library(stargazer)

#Source: https://lost-stats.github.io/Model_Estimation/Research_Design/event_study.html

#####All regressions on unmarried women aged 15-45 with high school or less: Linear probability models
#notes: YNGCH & ELDCH cause multicollinearity, push everything very close to 0 and raise standard errors. Adding weights did not change estimates, significance or R2. 

df <- read_csv("Data/Clean/mergedBirth.csv")

#create a time to treatment variable. Set to 0 for never treated units
regDF <- df %>%
  #Create indicator of which states received treatment
  mutate(TREAT = case_when(
    CAPGrp == 0 ~ 0,
    CAPGrp %in% c(1,2) ~ 1)) %>%
  #shorten the window
  #filter(EVENTt >= -11 & EVENTt <= 6 & EDUCATIONi == 0 & MARSTi == 0) %>%
  filter(YEAR > 1981 & YEAR < 2011 & proxyDem == 1)

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

#Make sure we haven't dropped any untreated observations
table(regDF$YEAR, regDF$YEARCAPPED)


# No controls -------------------------------------------------------------
reg <- feols(BIRTHi ~ sunab(YEARCAPPED, EVENTt) + 1, cluster = ~STATECENSUS,                          ## Clustered SEs
             data = regDF)

# Individual characteristics ----------------------------------------------
reg1 <- feols(BIRTHi ~ sunab(YEARCAPPED, EVENTt) + AGE + RACE_ETH + NCHILD, cluster = ~STATECENSUS,                          ## Clustered SEs
              data = regDF)

# Individual + State ------------------------------------------------------
reg2 <- feols(BIRTHi ~ sunab(YEARCAPPED, EVENTt) + AGE + RACE_ETH + NCHILD + TIMELIMITi + FEMUNEMP + MAXBENEFIT + MEDIANWAGE, cluster = ~STATECENSUS,                          ## Clustered SEs
              data = regDF)


# Individual + state + lagged state ---------------------------------------
reg3 <- feols(BIRTHi ~ sunab(YEARCAPPED, EVENTt) + AGE + RACE_ETH + NCHILD + TIMELIMITi + FEMUNEMP + MAXBENEFIT + MEDIANWAGE + lagMEDIANWAGE + lagUNEMP + lagBENEFIT , cluster = ~STATECENSUS,                          ## Clustered SEs
              data = regDF)

# Individual + State + State FE -------------------------------------------
reg4 <- feols(BIRTHi ~ sunab(YEARCAPPED, EVENTt) + AGE + RACE_ETH + NCHILD + TIMELIMITi + FEMUNEMP + MAXBENEFIT + MEDIANWAGE + lagMEDIANWAGE + lagUNEMP + lagBENEFIT|
                STATECENSUS, 
              cluster = ~STATECENSUS,                          ## Clustered SEs
              data = regDF)

# Individual + State  + State FE + Year FE --------------------------------
reg5 <- feols(BIRTHi ~ sunab(YEARCAPPED, EVENTt) + AGE + RACE_ETH + NCHILD + TIMELIMITi + FEMUNEMP + MAXBENEFIT + MEDIANWAGE + + lagMEDIANWAGE + lagUNEMP + lagBENEFIT|
                STATECENSUS + YEAR,
              cluster = ~STATECENSUS,                          ## Clustered SEs
              data = regDF)

# Make table --------------------------------------------------------------
etable(reg, reg1, reg2, reg3, reg4, reg5, digits = 3, se.below = T)

etable(summary(reg, agg = "ATT"), summary(reg1, agg = "ATT"), summary(reg2, agg = "ATT"), summary(reg3, agg = "ATT"), summary(reg4, agg = "ATT"), summary(reg5, agg = "ATT"), digits = 3, se.below = T, dict = c(AGE = "Age", RACE_ETHHispanic="Hispanic", RACE_ETHOther = "Other", RACE_ETHWhite = "White", NCHILD = "Number of children", TIMELIMITi = "Welfare time limit", FEMUNEMP = "Female unemployment rate", MAXBENEFIT = "Maximum monthly benefit for a family of 3", MEDIANWAGE = "Median female weekly wage"), tex = T)

# Figure ------------------------------------------------------------------

iplot(list(reg, reg1, reg2, reg3, reg4))


# Compare to less educated women -------------------------------
broadgroup <- df %>%
  #Create indicator of which states received treatment
  mutate(TREAT = case_when(
    CAPGrp == 0 ~ 0,
    CAPGrp %in% c(1,2) ~ 1)) %>%
  #shorten the window
  filter(EVENTt >= -11 & EVENTt <= 6 & EDUCATIONi == 0) %>%
  filter(YEAR > 1981 & YEAR < 2011) %>%
  # Following Sun and Abraham, we give our never-treated units a fake "treatment" date far outside the relevant study period.
  mutate(YEARCAPPED = case_when(
    TREAT == 1 ~ YEARCAPPED,
    TREAT == 0 ~ 10000)) %>%
  #sunab function needs never-treated to always have negative relative periods. 
  mutate(EVENTt = case_when(
    TREAT == 1 ~ EVENTt,
    TRUE ~ YEAR - YEARCAPPED))

reg6  <- feols(BIRTHi ~ sunab(YEARCAPPED, EVENTt) + AGE + RACE_ETH + NCHILD + TIMELIMITi + FEMUNEMP + MAXBENEFIT + MEDIANWAGE + lagMEDIANWAGE + lagUNEMP + lagBENEFIT|
                STATECENSUS + YEAR,
              cluster = ~STATECENSUS,                          ## Clustered SEs
              data = broadgroup)

# Compare to unmarried women --------------------------------
unmarried <- df %>%
  #Create indicator of which states received treatment
  mutate(TREAT = case_when(
    CAPGrp == 0 ~ 0,
    CAPGrp %in% c(1,2) ~ 1)) %>%
  #shorten the window
  filter(EVENTt >= -11 & EVENTt <= 6 & MARSTi == 0) %>%
  filter(YEAR > 1981 & YEAR < 2011) %>%
  # Following Sun and Abraham, we give our never-treated units a fake "treatment" date far outside the relevant study period.
  mutate(YEARCAPPED = case_when(
    TREAT == 1 ~ YEARCAPPED,
    TREAT == 0 ~ 10000)) %>%
  #sunab function needs never-treated to always have negative relative periods. 
  mutate(EVENTt = case_when(
    TREAT == 1 ~ EVENTt,
    TRUE ~ YEAR - YEARCAPPED))

reg7  <- feols(BIRTHi ~ sunab(YEARCAPPED, EVENTt) + AGE + RACE_ETH + NCHILD + TIMELIMITi + FEMUNEMP + MAXBENEFIT + MEDIANWAGE + lagMEDIANWAGE + lagUNEMP + lagBENEFIT|
                 STATECENSUS + YEAR,
               cluster = ~STATECENSUS,                          ## Clustered SEs
               data = unmarried)

# Compare to full sample of women --------------------------
fullSamp <- df %>%
  #Create indicator of which states received treatment
  mutate(TREAT = case_when(
    CAPGrp == 0 ~ 0,
    CAPGrp %in% c(1,2) ~ 1)) %>%
  #shorten the window
  filter(EVENTt >= -11 & EVENTt <= 6) %>%
  filter(YEAR > 1981 & YEAR < 2011) %>%
  # Following Sun and Abraham, we give our never-treated units a fake "treatment" date far outside the relevant study period.
  mutate(YEARCAPPED = case_when(
    TREAT == 1 ~ YEARCAPPED,
    TREAT == 0 ~ 10000)) %>%
  #sunab function needs never-treated to always have negative relative periods. 
  mutate(EVENTt = case_when(
    TREAT == 1 ~ EVENTt,
    TRUE ~ YEAR - YEARCAPPED))

reg8  <- feols(BIRTHi ~ sunab(YEARCAPPED, EVENTt) + AGE + RACE_ETH + NCHILD + TIMELIMITi + FEMUNEMP + MAXBENEFIT + MEDIANWAGE + lagMEDIANWAGE + lagUNEMP + lagBENEFIT|
                 STATECENSUS + YEAR,
               cluster = ~STATECENSUS,                          ## Clustered SEs
               data = fullSamp)

etable(summary(reg5, agg = "ATT"), summary(reg6, agg = "ATT"), summary(reg7, agg = "ATT"), summary(reg8, agg = "ATT"), digits = 3, se.below = T,  headers = c("Unmarried, less-educated women", "Less educated women", "Unmarried women", "All women"), dict = c(AGE = "Age", RACE_ETHHispanic="Hispanic", RACE_ETHOther = "Other", RACE_ETHWhite = "White", NCHILD = "Number of children", TIMELIMITi = "Welfare time limit", FEMUNEMP = "Female unempoyment rate", MAXBENEFIT = "Maximum monthly benefit for a family of 3", MEDIANWAGE = "Median female weekly wage"), tex = T)
