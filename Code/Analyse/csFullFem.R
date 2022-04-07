#callaway santanna DD estimator - basic attempt
library(tidyverse)
library(did)

#Source: https://bcallaway11.github.io/did/reference/att_gt.html
# http://resources.oliviajhealy.com/TWFE_Healy.pdf

df <- read_csv("Data/Clean/studyDat.csv")

#calculating grouptime average treatment effects so need to make the group variable "firstCapped" - year when state first implemented family cap. 0 if state never implemented family cap

regDF <- df %>%
  mutate(firstCapped = case_when(
    CAPGrp == 0 ~ 0,
    CAPGrp != 0 ~ YEARCAPPED))


mw.attgt <- att_gt(yname = "BIRTHi",
                   gname = "firstCapped",
                   panel = FALSE,
                   tname = "YEAR",
                   xformla = ~1,
                   data = regDF)
#x here is supposed to be 
#what if i put YEAR in event time?

summary(mw.attgt)
ggdid(mw.attgt)


# event study -------------------------------------------------------------
es <- aggte(mw.attgt, type = "dynamic")
summary(es)
ggdid(es)

save.image(file = "Data/Analysis/csFemFull.RData")
