#clean up state data book for exogenous treatment test. Each variable is in a different file. 

library(tidyverse)
library(haven)


# Pick out variables ------------------------------------------------------

#1990 population
df <- read_dta("Data/Raw/stateDataBook/1991/ICPSR_06398/DS0028/06398-0028-Data.dta") %>%
  filter(LEVEL %in% c(4, 5)) # only keep state level data
#Select variables to keep
population <- df %>%
  select(STABBREV, ITEM5) %>%
  mutate(ITEM5 = ITEM5*1000) %>%
  rename(pop1990 = ITEM5)

#urban pop pct 1980
df <- read_dta("Data/Raw/stateDataBook/1991/ICPSR_06398/DS0030/06398-0030-Data.dta") %>%
  filter(LEVEL %in% c(4, 5)) # only keep state level data
urban <- df %>%
  select(STABBREV, ITEM44) %>%
  rename(urbanPopPct1980 = ITEM44)

#1989 population by age group
df <- read_dta("Data/Raw/stateDataBook/1991/ICPSR_06398/DS0032/06398-0032-Data.dta") %>%
  filter(LEVEL %in% c(4, 5)) # only keep state level data
popByAge <- df %>%
  select(STABBREV, ITEM61, ITEM68, ITEM69) %>%
  mutate(under51989 = ITEM61 * 1000,
         over651989 = (ITEM68 + ITEM69)*1000) %>%
  select(-c(ITEM61, ITEM68, ITEM69))

#1990 population by race
df <- read_dta("Data/Raw/stateDataBook/1991/ICPSR_06398/DS0034/06398-0034-Data.dta") %>%
  filter(LEVEL %in% c(4, 5)) # only keep state level data
pctWhite <- df %>%
  select(STABBREV, ITEM95) %>%
  rename(pctWhite1990 = ITEM95)

df <- read_dta("Data/Raw/stateDataBook/1991/ICPSR_06398/DS0035/06398-0035-Data.dta") %>%
  filter(LEVEL %in% c(4, 5)) # only keep state level data
pctBlack <- df %>%
  select(STABBREV, ITEM109) %>%
  rename(pctBlack1990 = ITEM109)

#PCT hispanic 1990
df <- read_dta("Data/Raw/stateDataBook/1991/ICPSR_06398/DS0036/06398-0036-Data.dta") %>%
  filter(LEVEL %in% c(4, 5)) # only keep state level data
pctHispanic <- df %>%
  select(STABBREV, ITEM132) %>%
  rename(pctHisp1990 = ITEM132)

#1988 births per 1000 people in state of occurence 
df <- read_dta("Data/Raw/stateDataBook/1991/ICPSR_06398/DS0038/06398-0038-Data.dta") %>%
  filter(LEVEL %in% c(4, 5)) # only keep state level data
birthRate <- df %>%
  select(STABBREV, ITEM158) %>%
  rename(birthPer1k1988 = ITEM158)

#1988 medicaid recipients
df <- read_dta("Data/Raw/stateDataBook/1991/ICPSR_06398/DS0046/06398-0046-Data.dta") %>%
  filter(LEVEL %in% c(4, 5)) # only keep state level data
medicaidRecipients <- df %>%
  select(STABBREV, ITEM296) %>%
  rename(medicaidRecipients1988 = ITEM296) %>%
  mutate(medicaidRecipients1988 = medicaidRecipients1988 *1000)

#1987-1988 public high school graduates
#df <- read_dta("Data/Raw/stateDataBook/1991/ICPSR_06398/DS0053/06398-0053-Data.dta") %>%
#  filter(LEVEL %in% c(4, 5)) # only keep state level data
#hsGraduates1988 <- df %>% select(STABBREV, ITEM403) %>% rename(hsGraduates19881988 = ITEM403) %>% mutate(hsGraduates1988 = hsGraduates1988 *1000)

#AFDC recipients as pctent of total number of households 1988
df <- read_dta("Data/Raw/stateDataBook/1991/ICPSR_06398/DS0055/06398-0055-Data.dta") %>%
  filter(LEVEL %in% c(4, 5)) # only keep state level data
afdcRecProp <- df %>%
  select(STABBREV, ITEM454) %>%
  rename(afdcRecProp1988 = ITEM454)

#AFDC expenditure 1988, foodstamp participants and food stamp cost
df <- read_dta("Data/Raw/stateDataBook/1991/ICPSR_06398/DS0056/06398-0056-Data.dta") %>%
  filter(LEVEL %in% c(4, 5)) # only keep state level data
socialWelfare <- df %>%
  select(STABBREV, ITEM457, ITEM461, ITEM463) %>%
  rename(afdcExpenditure1988 = ITEM457,
         foodStampRecipients1988 = ITEM461,
         foodStampCost1988 = ITEM463) %>%
  mutate(afdcExpenditure1988 = afdcExpenditure1988*1000000,
         foodStampRecipients1988 = foodStampRecipients1988*1000,
         foodStampCost1988 = foodStampCost1988*1000000)

#civilian unemployment rate 1990
#LFPS 1989
df <- read_dta("Data/Raw/stateDataBook/1991/ICPSR_06398/DS0068/06398-0068-Data.dta") %>%
  filter(LEVEL %in% c(4, 5)) # only keep state level data
unemp <- df %>%
  select(STABBREV, ITEM648, ITEM657, ITEM658) %>%
  rename(unempRate1990 = ITEM648,
         femaleLFP1989 = ITEM658,
         maleLFP1989 = ITEM657)

#pct below poverty level 1979
df <- read_dta("Data/Raw/stateDataBook/1991/ICPSR_06398/DS0080/06398-0080-Data.dta") %>%
  filter(LEVEL %in% c(4, 5)) # only keep state level data
poverty <- df %>%
  select(STABBREV, ITEM848) %>%
  rename(pctBelowPoverty1979 = ITEM848)

#marriage rate 1988
df <- read_dta("Data/Raw/stateDataBook/1991/ICPSR_06398/DS0042/06398-0042-Data.dta") %>%
  filter(LEVEL %in% c(4, 5)) # only keep state level data
marriageRate <- df %>%
  select(STABBREV, ITEM223) %>%
  rename(marriagePer1K1988 = ITEM223)

#median household income 1979, disposable personal income per capita 1989
df <- read_dta("Data/Raw/stateDataBook/1991/ICPSR_06398/DS0079/06398-0079-Data.dta") %>%
  filter(LEVEL %in% c(4, 5)) # only keep state level data
income <- df %>%
  select(STABBREV, ITEM838, ITEM826) %>%
  rename(medianHouseholdIncome1979 = ITEM838,
         perCapitaPersonalIncome1989 = ITEM826)

#presidential popular vote variables
df <- read_dta("Data/Raw/stateDataBook/1991/ICPSR_06398/DS0126/06398-0126-Data.dta") %>%
  filter(LEVEL %in% c(4, 5)) # only keep state level data
popVote <- df %>%
  select(STABBREV, ITEM1575, ITEM1576) %>%
  rename(popVotePresidentDem1988 = ITEM1575,
         popVotePresidentRep1988 = ITEM1576) %>%
  mutate(presidentPopularVote1988 = case_when(
    popVotePresidentDem1988 > popVotePresidentRep1988 ~ "Democrat",
    popVotePresidentDem1988 < popVotePresidentRep1988 ~ "Republican")) %>%
  select(STABBREV, presidentPopularVote1988)

#senate composition
df <- read_dta("Data/Raw/stateDataBook/1991/ICPSR_06398/DS0127/06398-0127-Data.dta") %>%
  filter(LEVEL %in% c(4, 5)) # only keep state level data
senateComp <- df %>%
  select(STABBREV, ITEM1594, ITEM1595, ITEM1596, ITEM1597) %>%
  rename(senateDem1991 = ITEM1594,
         senateRep1991 = ITEM1595,
         houseRepDem1991 = ITEM1596,
         houseRepRep1991 = ITEM1597, ) %>%
  mutate(senateMajority1991 = case_when(
    senateDem1991 > senateRep1991 ~ "Democrat",
    senateDem1991 < senateRep1991 ~ "Republican",
    senateDem1991 == senateRep1991 ~ "Balanced"),
    houseRepMajority1991 = case_when(
      houseRepDem1991 > houseRepRep1991 ~ "Democrat",
      houseRepDem1991 < houseRepRep1991 ~ "Republican",
      houseRepDem1991 == houseRepRep1991 ~ "Balanced")) %>%
  select(STABBREV, senateMajority1991, houseRepMajority1991)

#abortion
df <- read_dta("Data/Raw/stateDataBook/1991/ICPSR_06398/DS0039/06398-0039-Data.dta") %>%
  filter(LEVEL %in% c(4, 5)) # only keep state level data
abortion <- df %>%
  select(STABBREV, ITEM172) %>%
  rename(legalAbortionPer1kWomen1988 = ITEM172)


# Merge variables ------------------------------------------------------------
df <- inner_join(afdcRecProp, birthRate, by = "STABBREV")
df <- inner_join(df, marriageRate, by = "STABBREV")
df <- inner_join(df, medicaidRecipients, by = "STABBREV")
df <- inner_join(df, pctBlack, by = "STABBREV")
df <- inner_join(df, pctWhite, by = "STABBREV")
df <- inner_join(df, pctHispanic, by = "STABBREV")
df <- inner_join(df, popByAge, by = "STABBREV")
df <- inner_join(df, population, by = "STABBREV")
df <- inner_join(df, poverty, by = "STABBREV")
df <- inner_join(df, socialWelfare, by = "STABBREV")
df <- inner_join(df, unemp, by = "STABBREV")
df <- inner_join(df, urban, by = "STABBREV")
df <- inner_join(df, income, by = "STABBREV")
df <- inner_join(df, popVote, by = "STABBREV")
df <- inner_join(df, senateComp, by = "STABBREV")
df <- inner_join(df, abortion, by = "STABBREV")

#make state name/abbreviation crosswalk
st_crosswalk <- tibble(STATE = toupper(state.name)) %>%
  bind_cols(tibble(abb = state.abb)) %>% 
  bind_rows(tibble(STATE = "DIST. OF COL.", abb = "DC")) %>%
  bind_rows(tibble(STATE = "D.C.", abb = "DC"))

# Merge in additional state values  --------
#afdc recipients 1990
afdcCaseload <- read_csv("Data/Raw/caseloads/1990.csv") %>%
  select(STATE, RECIPIENTS) %>%
  filter(STATE != "U.S. TOTALS")
#merge in cross walk
afdcCaseload <- left_join(afdcCaseload, st_crosswalk, by = "STATE") %>%
  #drop non states problems
  filter(STATE != "GUAM" & STATE != "VIRGIN ISLANDS" & STATE != "PUERTO RICO")

#assign New Hampshire an abbreviation
afdcCaseload$abb[30] <- "NH"

#rename variables
afdcCaseload <- afdcCaseload %>%
  rename(STABBREV = abb,
         afdcRecipients1990 = RECIPIENTS) %>%
  select(-STATE)

#merge with df
df <- inner_join(df, afdcCaseload, by = "STABBREV") 

#max welfare benefit family of 3 (1990)
maxBenefit <- read_csv("Data/Clean/maxBenefit.csv") %>%
  select(State, `1990`) %>%
  mutate(State = toupper(State)) %>%
  rename(STATE = State)
#merge in abbreviations
  

maxBenefit <- left_join(maxBenefit, st_crosswalk, by = "STATE") %>% #rename
  rename(STABBREV = abb,
         maxBenefit1990 = `1990`) %>%
  select(-STATE) 

#merge with df
df <- inner_join(df, maxBenefit, by = "STABBREV") 


# Convert absolute numbers to pct ---------------------------------------
df <- df %>%
  mutate(pctMedicaidRecip1988 = 100*medicaidRecipients1988/pop1990,
         pctUnder51989 = 100*under51989/pop1990,
         pctOver651989 = 100*over651989/pop1990,
         pctFoodStampRecipients1988 = 100*foodStampRecipients1988/pop1990,
         pctMedicaidRecipients1988 = 100*medicaidRecipients1988/pop1990,
         pctAFDCRecipients1990 = 100*afdcRecipients1990/pop1990)


# Add in treatment status and cohort via crosswalk ------------------------
caps <- read_csv("Data/Raw/capCrosswalk.csv") %>%
  rename(STABBREV = State)
#merge with df
df <- inner_join(df, caps, by = "STABBREV") 


# Add state regions -------------------------------------------------------
#create cross walk
region_crosswalk <- tibble(STABBREV = state.abb) %>%
  bind_cols(tibble(REGION = state.region)) %>% 
  bind_rows(tibble(STABBREV = "DC", REGION = "South"))

df <- left_join(df, region_crosswalk, by = "STABBREV")

#Summarise regions into bigger group (north, east, south, west)
df <- df %>%
  mutate(BROAD_REGION = case_when(
    REGION %in% c("Northeast", "North Central") ~ "North",
    TRUE ~ REGION
  ))

# Save data ---------------------------------------------------------------
write_csv(df, "Data/Clean/exogeneityTest.csv")