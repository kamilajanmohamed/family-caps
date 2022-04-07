#test predictors of treatment timing
library(tidyverse)
library(fixest)
library(rstatix)
library(stargazer)
library(car)
df <- read_csv("Data/Clean/exogeneityTest.csv")


# Early treated vs late treated -------------------------------------------
eVl <- df %>%
  filter(iCapped == 1) %>%
#normalise year of treatment to 1 in 1992
  mutate(normCapYear = yearCapped - 1992 + 1) %>%
  select(-c(iCapped, yearCapped, yearRepealed, medicaidRecipients1988, under51989, over651989, foodStampRecipients1988, medicaidRecipients1988, afdcRecProp1988)) %>%
  mutate(senateMajority1991 = as.factor(senateMajority1991),
         houseRepMajority1991 = as.factor(houseRepMajority1991),
         presidentPopularVote1988 = as.factor(presidentPopularVote1988),
         logPop1990 = log(pop1990),
         logMedianInc1979 = log(medianHouseholdIncome1979))

#try and predict "year of treatment" relative to the first cohort date with state level characteristics.
#demographic characteristics
pred1 <- lm(normCapYear ~ logPop1990, data  = eVl)
pred2 <- lm(normCapYear ~ pctUnder51989, data  = eVl)
pred3 <- lm(normCapYear ~ pctOver651989, data  = eVl)
pred4 <- lm(normCapYear ~ pctBlack1990, data  = eVl)
pred5 <- lm(normCapYear ~ marriagePer1K1988, data  = eVl)
#vital statistics
pred6 <- lm(normCapYear ~ birthPer1k1988, data  = eVl)
pred7 <- lm(normCapYear ~ legalAbortionPer1kWomen1988, data  = eVl)
#welfare & income
pred8 <- lm(normCapYear ~ logMedianInc1979, data  = eVl)
pred9 <- lm(normCapYear ~ log(perCapitaPersonalIncome1989), data  = eVl)
pred10 <- lm(normCapYear ~ maxBenefit1990, data  = eVl)
pred11 <- lm(normCapYear ~ pctAFDCRecipients1990, data  = eVl)
#political
pred12 <- lm(normCapYear ~ senateMajority1991, data  = eVl)
pred13 <- lm(normCapYear ~ houseRepMajority1991, data  = eVl)
pred14 <- lm(normCapYear ~ presidentPopularVote1988, data  = eVl)
#geography
pred15 <- lm(normCapYear ~ as.factor(BROAD_REGION), data  = eVl)
#all variables
pred16 <- lm(normCapYear ~ logPop1990 + pctBlack1990 + marriagePer1K1988 + birthPer1k1988 + legalAbortionPer1kWomen1988 + logMedianInc1979 + maxBenefit1990 + pctAFDCRecipients1990 + senateMajority1991 + houseRepMajority1991 + presidentPopularVote1988, data  = eVl)

stargazer(pred1, pred2, pred3, pred4, pred5, pred6, pred7, pred8, pred9, pred10, pred11, pred12, pred13, pred14, pred15, pred16, type = "text")

stargazer(pred1, pred4, pred5, pred6, pred7, pred8, pred10, pred11, pred12, pred13, pred14, pred16, title = "Determinants of State Family Cap Implementation Year Analysis Using the 1991 State and Metropolican Area Data Book", align = T, covariate.labels=c("Log population (1990)", "Percent of population black 1990", "Marriages per 1000 people (1988)", "Births per 1000 people (1988)", "Legal abortions per 1000 women (15-44 years) (1988)", "Log median household income (1979)", "Maximum monthly benefit for a family of 3 (1990)", "Percent of population AFDC recipients (1990)", "Senate majority - Democrat (1991)", "Senate majority - Republican (1991)","House of Representatives majority - Democrat (1991)", "House of Representatives majority - Republican (1991)", "Presidential popular vote - Republican (1988)"), out = "Output/Tables/startDateAnalysis.tex")


# F test for joint significance --------------------------------------------
coefs <- names(coef(pred16))
linearHypothesis(pred16, coefs[-1])

# Regressions but weighted by 1990 population -----------------------------


pred1 <- lm(normCapYear ~ logPop1990, weights = pop1990, data  = eVl)
pred2 <- lm(normCapYear ~ pctUnder51989, weights = pop1990, data  = eVl)
pred3 <- lm(normCapYear ~ pctOver651989, weights = pop1990, data  = eVl)
pred4 <- lm(normCapYear ~ pctBlack1990, weights = pop1990, data  = eVl)
pred5 <- lm(normCapYear ~ marriagePer1K1988, weights = pop1990, data  = eVl)
#vital statistics
pred6 <- lm(normCapYear ~ birthPer1k1988, weights = pop1990, data  = eVl)
pred7 <- lm(normCapYear ~ legalAbortionPer1kWomen1988, weights = pop1990, data  = eVl)
#welfare & income
pred8 <- lm(normCapYear ~ logMedianInc1979, weights = pop1990, data  = eVl)
pred9 <- lm(normCapYear ~ log(perCapitaPersonalIncome1989), weights = pop1990, data  = eVl)
pred10 <- lm(normCapYear ~ maxBenefit1990, weights = pop1990, data  = eVl)
pred11 <- lm(normCapYear ~ pctAFDCRecipients1990, weights = pop1990, data  = eVl)
#political
pred12 <- lm(normCapYear ~ senateMajority1991, weights = pop1990, data  = eVl)
pred13 <- lm(normCapYear ~ houseRepMajority1991, weights = pop1990, data  = eVl)
pred14 <- lm(normCapYear ~ presidentPopularVote1988, weights = pop1990, data  = eVl)
#geography
pred15 <- lm(normCapYear ~ as.factor(BROAD_REGION), weights = pop1990, data  = eVl)
#all variables
pred16 <- lm(normCapYear ~ logPop1990 + pctBlack1990 + marriagePer1K1988 + marriagePer1K1988 + birthPer1k1988 + legalAbortionPer1kWomen1988 + log(medianHouseholdIncome1979) + maxBenefit1990 + pctAFDCRecipients1990 + as.factor(senateMajority1991) + as.factor(houseRepMajority1991) + as.factor(presidentPopularVote1988), weights = pop1990, data  = eVl)

stargazer(pred1, pred4, pred5, pred6, pred7, pred8, pred10, pred11, pred12, pred13, pred14, pred16, title = "Determinants of State Family Cap Implementation Year Analysis Using the 1991 State and Metropolican Area Data Book", align = T, covariate.labels=c("Log population (1990)", "Percent of population black 1990", "Marriages per 1000 people (1988)", "Births per 1000 people (1988)", "Legal abortions per 1000 women (15-44 years) (1988)", "Log median household income (1979)", "Maximum monthly benefit for a family of 3 (1990)", "Percent of population AFDC recipients (1990)", "Senate majority - Democrat (1991)", "Senate majority - Republican (1991)","House of Representatives majority - Democrat (1991)", "House of Representatives majority - Republican (1991)", "Presidential popular vote - Republican (1988)"), out = "Output/Tables/startDateAnalysisWeighted.tex")
