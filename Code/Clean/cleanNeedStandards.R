#clean needs standards data
library(tidyverse)

tanf <- read_csv("Data/Raw/needStandards/tanfNeedStandard.csv")
afdc <- read_csv("Data/Raw/needStandards/afdcNeedStandard.csv")


# Make data longer --------------------------------------------------------
tanf <- tanf %>%
  pivot_longer(!c(state, unitSize), names_to = "year", values_to = "needStandard")

afdc <- afdc %>%
  pivot_longer(!c(state, unitSize), names_to = "year", values_to = "needStandard")

needStandards <- rbind(tanf, afdc) %>%
  mutate(needStandard = 12*needStandard)

write.csv(needStandards, "Data/Clean/needStandard.csv")
