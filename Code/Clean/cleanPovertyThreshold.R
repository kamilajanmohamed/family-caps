#clean poverty threshold data
library(tidyverse)
library(readxl)


#start with 1982 so susbequent dataframes have something to bind to 
df <- read_csv("Data/Raw/povertyThreshold/thresh82.csv", skip =4) %>%
  select(`Size of family unit`, `Weighted\nAverage\nThresholds`) %>%
  na.omit() %>%
  rename(povertyThresh = `Weighted\nAverage\nThresholds`)

df <- df[-c(1,3,4,6),] %>%
  mutate(hhSize = c(1:9)) %>%
  select(-c(`Size of family unit`)) %>%
  mutate(Year = 1982)

thresholds <- df

#formats change between years so start with 1983 to 2003
years <- c(1983:2003)
for(year in years){
  suff = sprintf('%02d', year %% 100)
  df <- read_csv(paste("Data/Raw/povertyThreshold/thresh", suff, ".csv", sep = ""), skip = 4) %>%
    select(`Size of family unit`, `Weighted\nAverage\nThresholds`) %>%
    na.omit() %>%
    rename(povertyThresh = `Weighted\nAverage\nThresholds`)
  
  df <- df[-c(1,3,4,6),] %>%
    mutate(hhSize = c(1:9)) %>%
    select(-c(`Size of family unit`)) %>%
    mutate(Year = year)

  thresholds <- rbind(thresholds, df)
}

#2004 onwards
df <- read_csv("Data/Raw/povertyThreshold/thresh04.csv", skip =5) %>%
  select(`Size of Family Unit`, `Weighted\nAverage\nThresholds`) %>%
  na.omit() %>%
  rename(povertyThresh = `Weighted\nAverage\nThresholds`)

df <- df[-c(1,3,4,6),] %>%
  mutate(hhSize = c(1:9)) %>%
  select(-c(`Size of Family Unit`)) %>%
  mutate(Year = 2004)

thresholds <- rbind(thresholds, df)

df <- read_csv("Data/Raw/povertyThreshold/thresh05.csv", skip =6) %>%
  select(`Size of Family Unit`, `Weighted\nAverage\nThresholds`) %>%
  na.omit() %>%
  rename(povertyThresh = `Weighted\nAverage\nThresholds`)

df <- df[-c(1,3,4,6),] %>%
  mutate(hhSize = c(1:9)) %>%
  select(-c(`Size of Family Unit`)) %>%
  mutate(Year = 2005)

thresholds <- rbind(thresholds, df)

df <- read_csv("Data/Raw/povertyThreshold/thresh06.csv", skip =6) %>%
  select(`Size of Family Unit`, `Weighted\nAverage\nThresholds`) %>%
  na.omit() %>%
  rename(povertyThresh = `Weighted\nAverage\nThresholds`)

df <- df[-c(1,3,4,6),] %>%
  mutate(hhSize = c(1:9)) %>%
  select(-c(`Size of Family Unit`)) %>%
  mutate(Year = 2006)

thresholds <- rbind(thresholds, df)

df <- read_excel("Data/Raw/povertyThreshold/thresh07.xls", skip =2) %>%
  select(`Size of Family Unit`, `Weighted Average Thresholds`) %>%
  na.omit() %>%
  rename(povertyThresh = `Weighted Average Thresholds`)

df <- df[-c(1,3,4,6),] %>%
  mutate(hhSize = c(1:9)) %>%
  select(-c(`Size of Family Unit`)) %>%
  mutate(Year = 2007)

thresholds <- rbind(thresholds, df)

df <- read_excel("Data/Raw/povertyThreshold/thresh08.xls", skip =4) %>%
  select(`Size of family unit`, Weighted) %>%
  na.omit() %>%
  rename(povertyThresh = Weighted)

df <- df[-c(1,3,4,6),] %>%
  mutate(hhSize = c(1:9)) %>%
  select(-c(`Size of family unit`)) %>%
  mutate(Year = 2008)

thresholds <- rbind(thresholds, df)

df <- read_excel("Data/Raw/povertyThreshold/thresh09.xls", skip =5) %>%
  select(`Size of family unit`, Weighted) %>%
  na.omit() %>%
  rename(povertyThresh = Weighted)

df <- df[-c(1,3,4,6),] %>%
  mutate(hhSize = c(1:9)) %>%
  select(-c(`Size of family unit`)) %>%
  mutate(Year = 2009)

thresholds <- rbind(thresholds, df)

df <- read_excel("Data/Raw/povertyThreshold/thresh10.xls", skip =5) %>%
  select(`Size of family unit`, Weighted) %>%
  na.omit() %>%
  rename(povertyThresh = Weighted)

df <- df[-c(1,3,4,6),] %>%
  mutate(hhSize = c(1:9)) %>%
  select(-c(`Size of family unit`)) %>%
  mutate(Year = 2010)

thresholds <- rbind(thresholds, df)

write.csv(thresholds, "Data/Clean/povertyThresholds.csv", row.names = F)

