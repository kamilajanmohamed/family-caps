# Author: Kamila Janmohamed, 15 Feb 2022
# Purpose: Create poverty and welfare receipt proxy dummies
library(tidyverse)

thresh <- read_csv("Data/Clean/povertyThresholds.csv") %>%
  rename(FAMSIZE = hhSize,
         YEAR = Year)

needStandard <- read_csv("Data/Clean/needStandard.csv") %>%
  rename(FAMSIZE = unitSize,
         YEAR = year,
         STATEL = state) %>%
  dplyr::select(-c(`...1`))

birth <- read_csv("Data/Clean/cpsBirth.csv")
empl <- read_csv("Data/Clean/cpsEmployment.csv")


# Merge in for birth ------------------------------------------------------
birth2 <- left_join(birth, thresh, by = c("YEAR", "FAMSIZE"))
birth2 <- left_join(birth2, needStandard, by = c("YEAR", "FAMSIZE", "STATEL"))

#get the upper bound for each family's income 
birth2$upperBound <- sapply(strsplit(birth2$FAMINC, " - "), tail, 1)
#address specific cases
birth2 <- birth2 %>%
  mutate(upperBound = case_when(
    #upperBound %in% c("Blank", "Missing", "Don't know", "Refused") ~ NA,
    upperBound == "$50,000 and over" ~ "50,000",
    upperBound == "Under $1,000" ~ "1,000",
    upperBound == "Under $5,000" ~ "5,000",
    upperBound == "$75,000 and over" ~ "75,000",
    upperBound == "$150,000 and over" ~ "150,000",
    TRUE ~ upperBound)) %>%
  #remove commas, assign NA to missing income, convert to numeric
  mutate(upperBound = as.numeric(gsub(",", "", upperBound)))

# Check what types are missing thresholds ---------------------------------
#over9 <- birth2 %>%
#  filter(FAMSIZE > 9) %>%
#  dplyr::select(upperBound, povertyThresh, FAMSIZE) %>%
#  filter(is.na(upperBound) == F)

#View(table(over9$upperBound, over9$FAMSIZE))

#get max thresh per year. if family size exceeds 9, assign poverty threshold for 9 to it
maxThresh <- thresh$povertyThresh[thresh$FAMSIZE == 9]
years <- c(1982:2010)
indices <- c(1:29)

for(i in indices){
  birth2$povertyThresh[birth2$FAMSIZE > 9 & birth2$YEAR == years[i]] <- maxThresh[i]
}

#repeat for families bigger than 12 and the need standard
stateCode <- c(unique(birth2$STATEL))
years <- c(1980:2017)
birth3 <- birth2

over12Standard <- needStandard %>%
  filter(FAMSIZE == 12)

for (state in stateCode){
  for (year in years){
    birth3$needStandard[birth3$YEAR == year & birth3$STATEL  ==  state & birth3$FAMSIZE > 12] <- over12Standard$needStandard[over12Standard$STATEL == state & over12Standard$YEAR == year]
  }
}

# Make proxy dummies for birth --------------------------------------------
birth4 <- birth3 %>%
  mutate(proxyDem = case_when(
    EDUCATIONi == 0 & MARSTi == 0 ~ 1,
    EDUCATIONi == 1 | MARSTi == 1 ~ 0),
    proxyPoverty = case_when(
      upperBound <= povertyThresh ~ 1,
      upperBound > povertyThresh ~ 0),
    proxyNeedStandard = case_when(
      upperBound <= needStandard ~ 1,
      upperBound > needStandard ~ 0)) %>%
  dplyr::select(-c(upperBound, FAMINC, FAMSIZE, povertyThresh, needStandard))

#measure correlation
cor(na.omit(cbind(birth4$proxyDem, birth4$proxyNeedStandard, birth4$proxyPoverty)))

write.csv(birth4, "Data/Clean/birthProxies.csv")

# Merge in for employment ------------------------------------------------------
empl2 <- left_join(empl, thresh, by = c("YEAR", "FAMSIZE"))
empl2 <- left_join(empl2, needStandard, by = c("YEAR", "FAMSIZE", "STATEL"))

#get the upper bound for each family's income 
empl2$upperBound <- sapply(strsplit(empl2$FAMINC, " - "), tail, 1)
#address specific cases
empl2 <- empl2 %>%
  mutate(upperBound = case_when(
    #upperBound %in% c("Blank", "Missing", "Don't know", "Refused") ~ NA,
    upperBound == "$50,000 and over" ~ "50,000",
    upperBound == "Under $1,000" ~ "1,000",
    upperBound == "Under $5,000" ~ "5,000",
    upperBound == "$75,000 and over" ~ "75,000",
    upperBound == "$150,000 and over" ~ "150,000",
    TRUE ~ upperBound)) %>%
  #remove commas, assign NA to missing income, convert to numeric
  mutate(upperBound = as.numeric(gsub(",", "", upperBound)))

# Check what types are missing thresholds ---------------------------------
#over9 <- birth2 %>%
#  filter(FAMSIZE > 9) %>%
#  dplyr::select(upperBound, povertyThresh, FAMSIZE) %>%
#  filter(is.na(upperBound) == F)

#View(table(over9$upperBound, over9$FAMSIZE))

#get max thresh per year. if family size exceeds 9, assign poverty threshold for 9 to it
maxThresh <- thresh$povertyThresh[thresh$FAMSIZE == 9]
years <- c(1982:2010)
indices <- c(1:29)

for(i in indices){
  empl2$povertyThresh[empl2$FAMSIZE > 9 & empl2$YEAR == years[i]] <- maxThresh[i]
}

#repeat for families bigger than 12 and the need standard
stateCode <- c(unique(empl2$STATEL))
years <- c(1980:2017)
empl3 <- empl2

over12Standard <- needStandard %>%
  filter(FAMSIZE == 12)

for (state in stateCode){
  for (year in years){
    empl3$needStandard[empl3$YEAR == year & empl3$STATEL  ==  state & empl3$FAMSIZE > 12] <- over12Standard$needStandard[over12Standard$STATEL == state & over12Standard$YEAR == year]
  }
}


# Make proxy dummies for birth --------------------------------------------
empl4 <- empl3 %>%
  mutate(proxyDem = case_when(
    EDUCATIONi == 0 & MARSTi == 0 ~ 1,
    EDUCATIONi == 1 | MARSTi == 1 ~ 0),
    proxyPoverty = case_when(
      upperBound <= povertyThresh ~ 1,
      upperBound > povertyThresh ~ 0),
    proxyNeedStandard = case_when(
      upperBound <= needStandard ~ 1,
      upperBound > needStandard ~ 0)) %>%
  dplyr::select(-c(upperBound, FAMINC, FAMSIZE, povertyThresh, needStandard))

cor(na.omit(cbind(empl4$proxyDem, empl4$proxyNeedStandard, empl4$proxyPoverty)))

write.csv(empl4, "Data/Clean/emplProxies.csv")

