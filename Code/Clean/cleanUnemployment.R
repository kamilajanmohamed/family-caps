library(tidyverse)
library(readxl)

df <- read_xlsx("Data/Raw/unemployment/stgen7620.xlsx", sheet = 1, skip = 3)

#keep female only
fem <- df %>%
  filter(Gender == "Women") %>%
  mutate(Year = as.numeric(Year)) %>%
  filter(Year >= 1980) %>%
  select(State, Year, `Unemployment rate`) %>%
  rename(femUnemployment = `Unemployment rate`)

write_csv(fem, "Data/Clean/femUnemployment.csv")
