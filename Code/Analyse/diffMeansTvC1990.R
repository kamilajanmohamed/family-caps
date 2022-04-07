#test treatment exogeneity: treated vs untreated on state and individual characteristics?

library(tidyverse)
library(knitr)


# Individual level characteristics ----------------------------------------
dat <- read_csv("Data/Clean/studyDat.csv")

#only keep observations for the target group in 1990 and convert factor levels to individual variables
s1990 <- dat %>%
  filter(YEAR == 1990 & EDUCATIONi == 0 & MARSTi == 0) %>%
  mutate(group = case_when(
    CAPGrp %in% c(1,2) ~ 1,
    TRUE ~ 0)) %>%
  mutate(black = case_when(
    RACE_ETH == "Black" ~ 1,
    TRUE ~ 0),
    white = case_when(
      RACE_ETH == "White" ~ 1,
      TRUE ~ 0),
    hispanic = case_when(
      RACE_ETH == "Hispanic" ~ 1,
      TRUE ~ 0),
    otherRace = case_when(
      RACE_ETH == "Other" ~ 1,
      TRUE ~ 0)) %>%
  #drop unneeded variables
  select(-c(TIMELIMITi, CAPGrp, CAPi, YEARCAPPED, YEARREPEALED, EVENTt, STATECENSUS, WTFINL, YEAR, CPSIDP, EDUCATIONi, RACE_ETH, RACEL, STATEL, RACE, NCHLT5, MARSTi)) %>%
  #drop state level variables
  select(-c(MAXBENEFIT, MEDIANWAGE, femUnemployment))

longDF <- s1990 %>%
  mutate(ELDCH = as.numeric(ELDCH),
         YNGCH = as.numeric(YNGCH)) %>%
  pivot_longer(-group, names_to = "variables", values_to = "value")

#differenc in means test
stat.test <- longDF %>%
  group_by(variables) %>%
  t_test(value ~ group) %>%
  adjust_pvalue(method = "BH") %>%
  add_significance()

diffs <- stat.test %>%
  select(variables, p.adj)

groupMeans <- s1990 %>%
  mutate(ELDCH = as.numeric(ELDCH),
         YNGCH = as.numeric(YNGCH)) %>%
  group_by(group) %>%
  summarise_all(mean, na.rm = T) %>% pivot_longer(-group, names_to = "variables", values_to = "value")

group0 <- groupMeans %>%
  filter(group == 0) %>%
  select(-c(group)) %>%
  rename(`Untreated states` = value)

group1 <- groupMeans %>%
  filter(group == 1) %>%
  select(-c(group)) %>%
  rename(`Treated states` = value)

groupMeans <- inner_join(group0, group1, by = "variables") %>%
  mutate(`Diff.` = `Treated states` - `Untreated states`)

groupMeans <- inner_join(groupMeans, diffs, by = "variables") %>%
  rename(`P-val on Diff.` = p.adj) %>%
  mutate_if(is.numeric, round, digits = 2)
groupMeans$variables <- c("Age", "Number of children", "Age of eldest child", "Age of youngest child", "Hours worked in the past week", "Gave birth in the past year", "Employed", "Black", "White", "Hispanic", "Other")

kable(groupMeans, "latex", caption = "Summary statistics for main data set in 1990",  booktabs = TRUE)

#get number per group for table
groupCount <- s1990 %>%
  mutate(ELDCH = as.numeric(ELDCH),
         YNGCH = as.numeric(YNGCH)) %>% pivot_longer(-group, names_to = "variables", values_to = "value") %>%
  group_by(group, variables) %>%
  summarise(counts = n())
groupCount


# State level characteristics from same file file ---------------------------------------------
s1990 <- dat %>%
  filter(YEAR == 1990) %>%
  mutate(group = case_when(
    CAPGrp %in% c(1,2) ~ 1,
    TRUE ~ 0)) %>%
  select(MAXBENEFIT, MEDIANWAGE, femUnemployment, TIMELIMITi, STATECENSUS, group) %>%
  distinct() %>%
  select(-c(STATECENSUS))

longDF <- s1990 %>%
  pivot_longer(-group, names_to = "variables", values_to = "value")

stat.test <- longDF %>%
  group_by(variables) %>%
  t_test(value ~ group) %>%
  adjust_pvalue(method = "BH") %>%
  add_significance()
stat.test

diffs <- stat.test %>%
  select(variables, p.adj)

groupMeans <- s1990 %>%
  group_by(group) %>%
  summarise_all(mean) %>% pivot_longer(-group, names_to = "variables", values_to = "value")

group0 <- groupMeans %>%
  filter(group == 0) %>%
  select(-c(group)) %>%
  rename(`Untreated states` = value)

group1 <- groupMeans %>%
  filter(group == 1) %>%
  select(-c(group)) %>%
  rename(`Treated states` = value)

groupMeans <- inner_join(group0, group1, by = "variables") %>%
  mutate(`Diff.` = `Treated states` - `Untreated states`)

groupMeans <- inner_join(groupMeans, diffs, by = "variables") %>%
  rename(`P-val on Diff.` = p.adj) %>%
  mutate_if(is.numeric, round, digits = 2)
groupMeans$variables <- c("Maximum benefit for a family of 3", "Median female wage", "Female unemployment rate", "Welfare time limits")

kable(groupMeans, "latex", caption = "Summary statistics for main data set in 1990",  booktabs = TRUE)

# State level means for political controls from state databook file -------
df <- read_csv("Data/Clean/exogeneityTest.csv") %>%
  select(presidentPopularVote1988, senateMajority1991, houseRepMajority1991, iCapped) %>%
  #turn factor variables into dummies
  mutate(presRep = case_when(
    presidentPopularVote1988 == "Republican" ~ 1,
    TRUE ~ 0),
    presDem = case_when(
      presidentPopularVote1988 == "Democrat" ~ 1,
      TRUE ~ 0),
    senateRep = case_when(
      senateMajority1991 == "Republican" ~ 1,
      TRUE ~ 0),
    senateDem = case_when(
      senateMajority1991 == "Democrat" ~ 1,
      TRUE ~ 0),
    senateBalanced = case_when(
      senateMajority1991 == "Balanced" ~ 1,
      TRUE ~ 0),
    houseRep = case_when(
      houseRepMajority1991 == "Republican" ~ 1,
      TRUE ~ 0),
    houseDem = case_when(
      houseRepMajority1991 == "Democrat" ~ 1,
      TRUE ~ 0),
    houseBalanced = case_when(
      houseRepMajority1991 == "Balanced" ~ 1,
      TRUE ~ 0)) %>%
  select(-c(presidentPopularVote1988, senateMajority1991, houseRepMajority1991))

#calculate groupmeans
longDF <- df %>%
  pivot_longer(-iCapped, names_to = "variables", values_to = "value")

stat.test <- longDF %>%
  group_by(variables) %>%
  t_test(value ~ iCapped) %>%
  adjust_pvalue(method = "BH") %>%
  add_significance()

diffs <- stat.test %>%
  select(variables, p.adj)

groupMeans <- df %>%
  group_by(iCapped) %>%
  summarise_all(mean) %>% pivot_longer(-iCapped, names_to = "variables", values_to = "value")

group0 <- groupMeans %>%
  filter(iCapped == 0) %>%
  select(-c(iCapped)) %>%
  rename(`Untreated states` = value)

group1 <- groupMeans %>%
  filter(iCapped == 1) %>%
  select(-c(iCapped)) %>%
  rename(`Treated states` = value)

groupMeans <- inner_join(group0, group1, by = "variables") %>%
  mutate(`Diff.` = `Treated states` - `Untreated states`)

groupMeans <- inner_join(groupMeans, diffs, by = "variables") %>%
  rename(`P-val on Diff.` = p.adj) %>%
  mutate_if(is.numeric, round, digits = 2)
groupMeans$variables <- c("Maximum benefit for a family of 3", "Median female wage", "Female unemployment rate")

kable(groupMeans, "latex", caption = "Summary statistics for main data set in 1990",  booktabs = TRUE)
