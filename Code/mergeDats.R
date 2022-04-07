####merge datasets
library(tidyverse)

birth <- read_csv("Data/Clean/birthProxies.csv")
empl <- read_csv("Data/Clean/emplProxies.csv")
unemp <- read_csv("Data/Clean/femUnemployment.csv")
benef <- read_csv("Data/Clean/maxBenefit.csv")
limits <- read_csv("Data/Clean/timeLimitsClean.csv")
wage <- read_csv("Data/Raw/medianWage/cpsMedianWage.csv")

#clean up covariate datasets
unemp <- unemp %>%
  rename(STATEL = State, 
         YEAR = Year) %>%
  mutate(STATEL = case_when(
    STATEL == "D.C." ~ "District of Columbia", 
    STATEL != "D.C." ~ STATEL
  )) %>%
  rename(FEMUNEMP = femUnemployment)

benef <- benef %>%
  rename(STATEL = State) %>%
  pivot_longer(!STATEL, names_to = "YEAR", values_to = "MAXBENEFIT") %>%
  mutate(YEAR = as.numeric(YEAR),
         STATEL = case_when(
           STATEL == "D.C." ~ "District of Columbia", 
           STATEL != "D.C." ~ STATEL))

limits <- limits %>%
  rename(STATEL = State) %>%
  pivot_longer(!STATEL, names_to = "YEAR", values_to = "TIMELIMITi") %>%
  mutate(YEAR = as.numeric(YEAR),
         STATEL = case_when(
           STATEL == "D.C." ~ "District of Columbia", 
           STATEL != "D.C." ~ STATEL))

wage <- wage %>%
  rename(STATEL = State) %>%
  pivot_longer(!STATEL, names_to = "YEAR", values_to = "MEDIANWAGE") %>%
  mutate(YEAR = as.numeric(YEAR))
  

# Merge into cps birth ----------------------------------------------------
df <- left_join(wage, unemp, by = c("YEAR", "STATEL"))
df <- left_join(df, benef, by = c("YEAR", "STATEL"))
df <- left_join(df, limits, by = c("YEAR", "STATEL"))
#add lags for state level controls
df <- df %>%
  arrange(YEAR) %>%
  group_by(STATEL) %>%
  mutate(lagMEDIANWAGE = lag(MEDIANWAGE),
         lagUNEMP = lag(FEMUNEMP),
         lagBENEFIT = lag(MAXBENEFIT)) 
df <- left_join(df, birth, by = c("YEAR", "STATEL"))

#change state and year to factor (not continuous) for fixed effects
df$STATECENSUS <- as.factor(df$STATECENSUS)
df$YEAR <- as.factor(df$YEAR)
df$STATEL <- as.factor(df$STATEL)

write_csv(df, "Data/Clean/mergedBirth.csv")

# Merge into cps employment -----------------------------------------------
df <- left_join(wage, unemp, by = c("YEAR", "STATEL"))
df <- left_join(df, benef, by = c("YEAR", "STATEL"))
df <- left_join(df, limits, by = c("YEAR", "STATEL"))
#add lagged state controls
df <- df %>%
  arrange(YEAR) %>%
  group_by(STATEL) %>%
  mutate(lagMEDIANWAGE = lag(MEDIANWAGE),
         lagUNEMP = lag(FEMUNEMP),
         lagBENEFIT = lag(MAXBENEFIT)) 
df <- left_join(df, empl, by = c("YEAR", "STATEL"))

#change state and year to factor (not continuous) for fixed effects
df$STATECENSUS <- as.factor(df$STATECENSUS)
df$YEAR <- as.factor(df$YEAR)
df$STATEL <- as.factor(df$STATEL)

write_csv(df, "Data/Clean/mergedEmployment.csv")

