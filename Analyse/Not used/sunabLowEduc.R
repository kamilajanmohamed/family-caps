#Sun Abahram DID estimator
library(tidyverse)
library(fixest)
library(stargazer)

#Source: https://lost-stats.github.io/Model_Estimation/Research_Design/event_study.html

#####All regressions on women aged 15-45 with high school or less: Linear probability models
df <- read_csv("Data/Clean/studyDat.csv")

df$STATECENSUS <- as.factor(df$STATECENSUS)
df$YEAR <- as.factor(df$YEAR)
df$STATEL <- as.factor(df$STATEL)
df$TIMELIMITi <- as.factor(df$TIMELIMITi)

#create a time to treatment variable. Set to 0 for never treated units
regDF <- df %>%
  #Create indicator of which states received treatment
  mutate(TREAT = case_when(
    CAPGrp == 0 ~ 0,
    CAPGrp %in% c(1,2) ~ 1)) %>%
  #shorten the window
  filter(EVENTt >= -12 & EVENTt <= 5 & EDUCATIONi == 0) 


# Regular two way fixed effects -------------------------------------------
mod_twfe <-  feols(BIRTHi ~ i(EVENTt, TREAT, ref = -1) + ## Our key interaction: time × treatment status
                   AGE + RACE + NCHILD + YNGCH + ELDCH + TIMELIMITi + femUnemployment + MAXBENEFIT + MEDIANWAGE|                    ## Other controls
                   STATECENSUS + YEAR,                             ## FEs
                 cluster = ~STATECENSUS,                          ## Clustered SEs
                 data = regDF)

iplot(mod_twfe, 
      xlab = 'Time to treatment',
      main = 'Event study: Staggered treatment (TWFE)')


# The Sun Abraham Way -----------------------------------------------------
# Following Sun and Abraham, we give our never-treated units a fake "treatment" date far outside the relevant study period.
sunAB <- regDF %>%
  mutate(YEARCAPPED = case_when(
    TREAT == 1 ~ YEARCAPPED,
    TREAT == 0 ~ 10000
  ))

mod_sa = feols(BIRTHi ~ sunab(YEARCAPPED, YEAR) + 
                 AGE + RACE + NCHILD + YNGCH + ELDCH  + TIMELIMITi + femUnemployment + MAXBENEFIT + MEDIANWAGE|                    ## Other controls
                 STATECENSUS + YEAR,                             ## FEs
               cluster = ~STATECENSUS,                          ## Clustered SEs
               data = sunAB)

iplot(list(mod_twfe, mod_sa), sep = 0.5, ref.line = -1,
      xlab = 'Time to treatment',
      main = 'Event study: Staggered treatment, Less educated females')
legend("bottom", col = c(1, 2), pch = c(20, 17), 
       legend = c("TWFE", "Sun & Abraham (2020)"))