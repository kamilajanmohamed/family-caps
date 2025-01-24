library(tidyverse)
library(lfe)
library(fixest)
library(miceadds) #for lm.cluster function

df <- read_csv("Data/Clean/studyDat.csv")

#create a time to treatment variable. Set to 0 for never treated units
regDF <- df %>%
  #Create indicator of which states received treatment
  mutate(TREAT = case_when(
    CAPGrp == 0 ~ 0,
    CAPGrp %in% c(1,2) ~ 1)) %>%
  filter(STATEL != "New Jersey") %>%
  #shorten the window
  #filter(EVENTt >= -11 & EVENTt <= 6 & EDUCATIONi == 0 & MARSTi == 0) %>%
  filter(YEAR > 1983 & YEAR < 2011 & EDUCATIONi == 0 & MARSTi == 0)

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


# No controls -------------------------------------------------------------
reg <- feols(AHRSWORKT ~ i(EVENTt, TREAT, ref = -1) + 1, cluster = ~STATECENSUS,                          ## Clustered SEs
             data = regDF)  
summary(lm.cluster(AHRSWORKT ~ TREAT, cluster = "STATECENSUS",
                   data = regDF)) 

reg0 <- feols(AHRSWORKT ~ i(EVENTt, TREAT, ref = -1) + BIRTHi, cluster = ~STATECENSUS,                          ## Clustered SEs
              data = regDF)  
summary(lm.cluster(AHRSWORKT ~ TREAT + BIRTHi, cluster = "STATECENSUS",
                   data = regDF)) 

# Individual characteristics ----------------------------------------------
reg1 <- feols(AHRSWORKT ~ i(EVENTt, TREAT, ref = -1) + BIRTHi + AGE + RACE_ETH + NCHILD, cluster = ~STATECENSUS,                          ## Clustered SEs
              data = regDF)

summary(lm.cluster(AHRSWORKT ~ TREAT + BIRTHi + AGE + RACE_ETH + NCHILD, cluster = "STATECENSUS", 
                   data = regDF))

# Individual + State ------------------------------------------------------
reg2 <- feols(AHRSWORKT ~ i(EVENTt, TREAT, ref = -1) + BIRTHi + AGE + RACE_ETH + NCHILD + TIMELIMITi + femUnemployment + MAXBENEFIT + MEDIANWAGE, cluster = ~STATECENSUS,                          ## Clustered SEs
              data = regDF)

summary(lm.cluster(AHRSWORKT ~ TREAT + BIRTHi + AGE + RACE_ETH + NCHILD + as.factor(TIMELIMITi) + femUnemployment + MAXBENEFIT + MEDIANWAGE, cluster = "STATECENSUS", 
                   data = regDF))

# Individual + State + State FE -------------------------------------------
reg3 <- feols(AHRSWORKT ~ i(EVENTt, TREAT, ref = -1) + BIRTHi + AGE + RACE_ETH + NCHILD + TIMELIMITi + femUnemployment + MAXBENEFIT + MEDIANWAGE|
                STATECENSUS, 
              cluster = ~STATECENSUS,                          ## Clustered SEs
              data = regDF)

summary(lm.cluster(AHRSWORKT ~ TREAT + BIRTHi + AGE + RACE_ETH + NCHILD + as.factor(TIMELIMITi) + femUnemployment + MAXBENEFIT + MEDIANWAGE + as.factor(STATECENSUS), cluster = "STATECENSUS", 
                   data = regDF))

# Individual + State  + State FE + Year FE --------------------------------
reg4 <- feols(AHRSWORKT ~ i(EVENTt, TREAT, ref = -1) + BIRTHi + AGE + RACE_ETH + NCHILD + TIMELIMITi + femUnemployment + MAXBENEFIT + MEDIANWAGE|
                STATECENSUS + YEAR,
              cluster = ~STATECENSUS,                          ## Clustered SEs
              data = regDF)

summary(lm.cluster(AHRSWORKT ~ TREAT + BIRTHi + AGE + RACE_ETH + NCHILD + as.factor(TIMELIMITi) + femUnemployment + MAXBENEFIT + MEDIANWAGE + as.factor(STATECENSUS) + as.factor(YEAR), cluster = "STATECENSUS", 
                   data = regDF))
