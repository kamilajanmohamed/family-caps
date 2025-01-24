#Sun Abahram DID estimator - effect on probability of employment conditional on birth
library(tidyverse)
library(fixest)
library(stargazer)

#Source: https://lost-stats.github.io/Model_Estimation/Research_Design/event_study.html

#####All regressions on women aged 15-45 with high school or less: Linear probability models
df <- read_csv("Data/Clean/mergedEmployment.csv")

#create a time to treatment variable. Set to 0 for never treated units
regDF <- df %>%
  #Create indicator of which states received treatment
  mutate(TREAT = case_when(
    CAPGrp == 0 ~ 0,
    CAPGrp %in% c(1,2) ~ 1)) %>%
  #shorten the window
  filter(EVENTt >= -11 & EVENTt <= 6 & EDUCATIONi == 0 & MARSTi == 0) %>%
  filter(YEAR > 1981 & YEAR < 2011) %>%
  # Following Sun and Abraham, we give our never-treated units a fake "treatment" date far outside the relevant study period.
  mutate(YEARCAPPED = case_when(
    TREAT == 1 ~ YEARCAPPED,
    TREAT == 0 ~ 10000)) %>%
  #sunab function needs never-treated to always have negative relative periods.
  mutate(EVENTt = case_when(
    TREAT == 1 ~ EVENTt,
    TRUE ~ YEAR - YEARCAPPED))

# No controls -------------------------------------------------------------
reg <- feols(EMPLi ~ sunab(YEARCAPPED, EVENTt), cluster = ~STATECENSUS,                          ## Clustered SEs
              data = regDF)

reg0 <- feols(EMPLi ~ sunab(YEARCAPPED, EVENTt) + BIRTHi, cluster = ~STATECENSUS,                          ## Clustered SEs
             data = regDF)

# Individual characteristics ----------------------------------------------
reg1 <- feols(EMPLi ~ sunab(YEARCAPPED, EVENTt) + BIRTHi + AGE + RACE_ETH + NCHILD, cluster = ~STATECENSUS,                          ## Clustered SEs
              data = regDF)

# Individual + State ------------------------------------------------------
reg2 <- feols(EMPLi ~ sunab(YEARCAPPED, EVENTt) + BIRTHi + AGE + RACE_ETH + NCHILD + TIMELIMITi + FEMUNEMP + MAXBENEFIT + MEDIANWAGE, cluster = ~STATECENSUS,                          ## Clustered SEs
              data = regDF)
# Individual + State + State lag -------------------------------------------------
reg3 <- feols(EMPLi ~ sunab(YEARCAPPED, EVENTt) + BIRTHi + AGE + RACE_ETH + NCHILD + TIMELIMITi + FEMUNEMP + MAXBENEFIT + MEDIANWAGE+lagMEDIANWAGE + lagUNEMP + lagBENEFIT, cluster = ~STATECENSUS,                          ## Clustered SEs
              data = regDF)

# Individual + State + State FE -------------------------------------------
reg4 <- feols(EMPLi ~ sunab(YEARCAPPED, EVENTt) + BIRTHi + AGE + RACE_ETH + NCHILD + TIMELIMITi + FEMUNEMP + MAXBENEFIT + MEDIANWAGE+lagMEDIANWAGE + lagUNEMP + lagBENEFIT|
                STATECENSUS, 
              cluster = ~STATECENSUS,                          ## Clustered SEs
              data = regDF)

# Individual + State  + State FE + Year FE --------------------------------
reg5 <- feols(EMPLi ~ sunab(YEARCAPPED, EVENTt) + BIRTHi + AGE + RACE_ETH + NCHILD + TIMELIMITi + FEMUNEMP + MAXBENEFIT + MEDIANWAGE+lagMEDIANWAGE + lagUNEMP + lagBENEFIT|
                STATECENSUS + YEAR,
              cluster = ~STATECENSUS,## Clustered SEs
              data = regDF)

# Make table --------------------------------------------------------------
etable(reg1, reg2, reg3, reg4, reg5, digits = 3, se.below = T)

etable(summary(reg, agg = "ATT"), summary(reg0, agg = "ATT"), summary(reg1, agg = "ATT"), summary(reg2, agg = "ATT"), summary(reg3, agg = "ATT"), summary(reg4, agg = "ATT"), summary(reg5, agg = "ATT"), digits = 3, se.below = T, tex = T)
