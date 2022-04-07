#callaway santanna DD estimator - low educated females -12 to 5 event time
library(tidyverse)
library(did)
library(stargazer)

#Source: https://bcallaway11.github.io/did/reference/att_gt.html
# http://resources.oliviajhealy.com/TWFE_Healy.pdf

df <- read_csv("Data/Clean/studyDat.csv")

#calculating grouptime average treatment effects so need to make the group variable "firstCapped" - year when state first implemented family cap. 0 if state never implemented family cap

regDF <- df %>%
  mutate(firstCapped = case_when(
    CAPGrp == 0 ~ 0,
    CAPGrp != 0 ~ YEARCAPPED)) %>%
  #event time
  mutate(EVENTt = case_when(
  is.na(YEARCAPPED) == TRUE ~ 0,
  CAPGrp == 1 ~ YEAR - YEARCAPPED,
  CAPGrp == 2 & YEAR < YEARREPEALED ~ YEAR - YEARCAPPED)) %>%
  #Create indicator of which states received treatment
  mutate(TREAT = case_when(
    CAPGrp == 0 ~ 0,
    CAPGrp %in% c(1,2) ~ 1)) %>%
  #shorten the window
  filter(EVENTt >= -12 & EVENTt <= 5 & EDUCATIONi == 0) 


mw.attgt <- att_gt(yname = "BIRTHi",
                   gname = "firstCapped",
                   panel = FALSE,
                   tname = "YEAR",
                   xformla = ~1,
                   data = regDF)

summary(mw.attgt)
ggdid(mw.attgt)


# event study -------------------------------------------------------------
es <- aggte(mw.attgt, type = "dynamic", na.rm = T)
summary(es)
ggdid(es)

save.image(file = "Data/Analysis/csFemLeduc.RData")
