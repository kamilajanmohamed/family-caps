#Estimate full regression on income proxy group
library(tidyverse)
library(fixest)

df <- read_csv("Data/Clean/incomeProxyMerged.csv")
#create a time to treatment variable. Set to 0 for never treated units
regDF <- df %>%
  #drop lenient states 
  #Create indicator of which states received treatment
  mutate(TREAT = case_when(
    CAPGrp == 0 ~ 0,
    CAPGrp %in% c(1,2) ~ 1)) %>%
  filter(proxyInc == 1) %>%
  #shorten the window
  filter(YEAR > 1981 & YEAR < 2011)

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
    TRUE ~ YEAR - YEARCAPPED)) %>%
  mutate(MARSTi = as.factor(MARSTi),
         TIMELIMITi = as.factor(TIMELIMITi),
         EDUCATIONi = as.factor(EDUCATIONi))

# Effect on birth ----------------------------------------------------------
birth1 <- feols(BIRTHi ~ sunab(YEARCAPPED, EVENTt) + AGE + RACE_ETH + NCHILD + TIMELIMITi + femUnemployment + MAXBENEFIT + MEDIANWAGE|
                  STATECENSUS + YEAR,
                cluster = ~STATECENSUS,                          ## Clustered SEs
                data = regDF)

birth2 <- feols(BIRTHi ~ sunab(YEARCAPPED, EVENTt) + EDUCATIONi + AGE + RACE_ETH + NCHILD + TIMELIMITi + femUnemployment + MAXBENEFIT + MEDIANWAGE|
                  STATECENSUS + YEAR,
                cluster = ~STATECENSUS,                          ## Clustered SEs
                data = regDF)

birth3 <- feols(BIRTHi ~ sunab(YEARCAPPED, EVENTt) + MARSTi + AGE + RACE_ETH + NCHILD + TIMELIMITi + femUnemployment + MAXBENEFIT + MEDIANWAGE|
                  STATECENSUS + YEAR,
                cluster = ~STATECENSUS,                          ## Clustered SEs
                data = regDF)

birth4 <- feols(BIRTHi ~ sunab(YEARCAPPED, EVENTt) + EDUCATIONi + MARSTi + AGE + RACE_ETH + NCHILD + TIMELIMITi + femUnemployment + MAXBENEFIT + MEDIANWAGE|
                  STATECENSUS + YEAR,
                cluster = ~STATECENSUS,                          ## Clustered SEs
                data = regDF)
birthTab <- etable(summary(birth1, agg = "ATT"), summary(birth2, agg = "ATT"), summary(birth3, agg = "ATT"), summary(birth4, agg = "ATT"))


# Effect on employment ----------------------------------------------------
empl1 <- feols(EMPLi ~ sunab(YEARCAPPED, EVENTt) + AGE + RACE_ETH + NCHILD + TIMELIMITi + femUnemployment + MAXBENEFIT + MEDIANWAGE|
                 STATECENSUS + YEAR,
               cluster = ~STATECENSUS,                          ## Clustered SEs
               data = regDF)

empl2 <- feols(EMPLi ~ sunab(YEARCAPPED, EVENTt) + BIRTHi + AGE + RACE_ETH + NCHILD + TIMELIMITi + femUnemployment + MAXBENEFIT + MEDIANWAGE|
                 STATECENSUS + YEAR,
               cluster = ~STATECENSUS,                          ## Clustered SEs
               data = regDF)

empl3 <- feols(EMPLi ~ sunab(YEARCAPPED, EVENTt) + EDUCATIONi + BIRTHi + AGE + RACE_ETH + NCHILD + TIMELIMITi + femUnemployment + MAXBENEFIT + MEDIANWAGE|
                 STATECENSUS + YEAR,
               cluster = ~STATECENSUS,                          ## Clustered SEs
               data = regDF)

empl4 <- feols(EMPLi ~ sunab(YEARCAPPED, EVENTt) + MARSTi + AGE + RACE_ETH + NCHILD + TIMELIMITi + femUnemployment + MAXBENEFIT + MEDIANWAGE|
                 STATECENSUS + YEAR,
               cluster = ~STATECENSUS,                          ## Clustered SEs
               data = regDF)

empl5 <- feols(EMPLi ~ sunab(YEARCAPPED, EVENTt) + BIRTHi + EDUCATIONi + MARSTi + AGE + RACE_ETH + NCHILD + TIMELIMITi + femUnemployment + MAXBENEFIT + MEDIANWAGE|
                 STATECENSUS + YEAR,
               cluster = ~STATECENSUS,                          ## Clustered SEs
               data = regDF)

emplTab <- etable(summary(empl1, agg = "ATT"), summary(empl2, agg = "ATT"), summary(empl3, agg = "ATT"), summary(empl4, agg = "ATT"), summary(empl5, agg = "ATT"))

# Effect on ahrsworkt -----------------------------------------------------
regDF <- regDF %>%
  filter(STATEL != "New Jersey" & YEAR > 1983)

hours1 <- feols(AHRSWORKT ~ sunab(YEARCAPPED, EVENTt) + AGE + RACE_ETH + NCHILD + TIMELIMITi + femUnemployment + MAXBENEFIT + MEDIANWAGE|
                 STATECENSUS + YEAR,
               cluster = ~STATECENSUS,                          ## Clustered SEs
               data = regDF)

hours2 <- feols(AHRSWORKT ~ sunab(YEARCAPPED, EVENTt) + BIRTHi + AGE + RACE_ETH + NCHILD + TIMELIMITi + femUnemployment + MAXBENEFIT + MEDIANWAGE|
                 STATECENSUS + YEAR,
               cluster = ~STATECENSUS,                          ## Clustered SEs
               data = regDF)

hours3 <- feols(AHRSWORKT ~ sunab(YEARCAPPED, EVENTt) + EDUCATIONi + BIRTHi + AGE + RACE_ETH + NCHILD + TIMELIMITi + femUnemployment + MAXBENEFIT + MEDIANWAGE|
                 STATECENSUS + YEAR,
               cluster = ~STATECENSUS,                          ## Clustered SEs
               data = regDF)

hours4 <- feols(AHRSWORKT ~ sunab(YEARCAPPED, EVENTt) + MARSTi + AGE + RACE_ETH + NCHILD + TIMELIMITi + femUnemployment + MAXBENEFIT + MEDIANWAGE|
                 STATECENSUS + YEAR,
               cluster = ~STATECENSUS,                          ## Clustered SEs
               data = regDF)

hours5 <- feols(AHRSWORKT ~ sunab(YEARCAPPED, EVENTt) + EMPLi + AGE + RACE_ETH + NCHILD + TIMELIMITi + femUnemployment + MAXBENEFIT + MEDIANWAGE|
                  STATECENSUS + YEAR,
                cluster = ~STATECENSUS,                          ## Clustered SEs
                data = regDF)


hours6 <- feols(AHRSWORKT ~ sunab(YEARCAPPED, EVENTt) + BIRTHi + EDUCATIONi + MARSTi + AGE + RACE_ETH + NCHILD + TIMELIMITi + femUnemployment + MAXBENEFIT + MEDIANWAGE|
                 STATECENSUS + YEAR,
               cluster = ~STATECENSUS,                          ## Clustered SEs
               data = regDF)

hours7 <- feols(AHRSWORKT ~ sunab(YEARCAPPED, EVENTt) + BIRTHi + EDUCATIONi + MARSTi + EMPLi + AGE + RACE_ETH + NCHILD + TIMELIMITi + femUnemployment + MAXBENEFIT + MEDIANWAGE|
                  STATECENSUS + YEAR,
                cluster = ~STATECENSUS,                          ## Clustered SEs
                data = regDF)

hoursTab <- etable(summary(hours1, agg = "ATT"), summary(hours2, agg = "ATT"), summary(hours3, agg = "ATT"), summary(hours4, agg = "ATT"), summary(hours5, agg = "ATT"), summary(hours6, agg = "ATT"), summary(hours6, agg = "ATT"))  

