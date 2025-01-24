#difference in means, capped and uncapped, state level characteristics
library(tidyverse)
library(fixest)
library(rstatix)
library(corrplot)
library(stargazer)
df <- read_csv("Data/Clean/exogeneityTest.csv")


# Treated vs control -------------------------------------------
tVc <- df %>%
  select(-c(yearCapped, yearRepealed, medicaidRecipients1988, under51989, over651989, foodStampRecipients1988, medicaidRecipients1988, afdcRecProp1988, STABBREV, REGION, BROAD_REGION, afdcRecipients1990, marriagePer1K1988, afdcExpenditure1988, foodStampCost1988, pctMedicaidRecip1988)) %>%
  mutate(pop1990 = log(pop1990))

longDF <- tVc %>%
  pivot_longer(-iCapped, names_to = "variables", values_to = "value")

stat.test <- longDF %>%
  group_by(variables) %>%
  t_test(value ~ iCapped) %>%
  adjust_pvalue(method = "BH") %>%
  add_significance()
stat.test

diffs <- stat.test %>%
  select(variables, p.adj)

groupMeans <- tVc %>%
  group_by(iCapped) %>%
  summarise_all(mean, na.rm = T) %>% pivot_longer(-iCapped, names_to = "variables", values_to = "value")

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

#rename variables with labels
groupMeans$variables <- c("Births/1000", "Percent Black", "Percent White", "Percent Hispanic", "Population", "Percent below poverty", "Unemployment rate", "Male labour force participation rate", "Female labour force participation rate", "Percent urban-dwelling", "Maximum benefit for a family of three", "Percent aged ≤ 5", "Percent aged 65+", "Percent food stam recipients", "Percent Medicaid recipients", "Percent AFDC recipients")

kable(groupMeans, "latex", caption = "Difference in means for state-level characteristics in 1990",  booktabs = TRUE)