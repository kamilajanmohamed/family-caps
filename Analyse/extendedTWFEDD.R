#woolidge extended twfedd
library(tidyverse)
library(fixest)

#Source:http://arelbundock.com/posts/2021-09-30-extendedtwfe/

#####All regressions on women aged 15-45 with high school or less: Linear probability models
df <- read_csv("Data/Clean/studyDat.csv")

#create a time to treatment variable. Set to 0 for never treated units
regDF <- df %>%
  #Create indicator of which states received treatment
  mutate(TREAT = case_when(
    CAPGrp == 0 ~ 0,
    CAPGrp %in% c(1,2) ~ 1)) %>%
  #YEARCAPPED should be 0 for all never treated 
  mutate(YEARCAPPED = case_when(
    is.na(YEARCAPPED) == TRUE ~ 0,
    is.na(YEARCAPPED) == FALSE ~ YEARCAPPED
    )) %>%
  #shorten the window
  filter(EVENTt >= -12 & EVENTt <= 5 & EDUCATIONi == 0) 

etwfe = feols(BIRTHi ~ TREAT : factor(EVENTt) : factor(YEARCAPPED) +
                + ## Our key interaction: time × treatment status
                AGE + i(RACE) + NCHILD + YNGCH + ELDCH + i(TIMELIMITi) + femUnemployment + MAXBENEFIT + MEDIANWAGE
              | STATECENSUS + YEAR, 
              cluster = ~STATECENSUS,## Clustered SEs
              data = regDF)

tidyReg <- tidy(etwfe) %>%
  mutate(Group = case_when(
    grepl("1992", term) == T ~ 1992,
    grepl("1994", term) == T ~ 1994,
    grepl("1995", term) == T ~ 1995,
    grepl("1996", term) == T ~ 1996,
    grepl("1997", term) == T ~ 1997,
    grepl("1998", term) == T ~ 1998,
    grepl("2002", term) == T ~ 2002,
    grepl("2003", term) == T ~ 2003),
    eventT = case_when(
      grepl("-11:", term) == T ~ -11,
      grepl("-10:", term) == T ~ -10,
      grepl("-9:", term) == T ~ -9,
      grepl("-8:", term) == T ~ -8,
      grepl("-7:", term) == T ~ -7,
      grepl("-6:", term) == T ~ -6,
      grepl("-5:", term) == T ~ -5,
      grepl("-4:", term) == T ~ -4,
      grepl("-3:", term) == T ~ -3,
      grepl("-2:", term) == T ~ -2,
      grepl("-1:", term) == T ~ -1,
      grepl("0:", term) == T ~ 0,
      grepl("1:", term) == T ~ 1,
      grepl("2:", term) == T ~ 2,
      grepl("3:", term) == T ~ 3,
      grepl("4:", term) == T ~ 4
    )) %>%
  na.omit()


lin <- ggplot(tidyReg, aes(y = estimate, x = eventT, group = Group, colour = as.factor(Group))) +
  geom_point()+
  geom_errorbar(aes(ymin=estimate-std.error, ymax=estimate+std.error))

# compare to CS -----------------------------------------------------------
csa = att_gt(
  yname = "BIRTHi",
  gname = "YEARCAPPED",
  panel = FALSE,
  tname = "YEAR",
  control_group = "notyettreated",
  clustervars = "STATECENSUS",
  xformla = ~1,
  data = regDF)

summary(csa)
ggdid(csa)

csaES <- aggte(csa, type = "dynamic", na.rm = T)
summary(csaES)
ggdid(csaES)
###weirdly negative

#compare results if control group never treated
csaNev = att_gt(
  yname = "BIRTHi",
  gname = "YEARCAPPED",
  panel = FALSE,
  tname = "YEAR",
  clustervars = "STATECENSUS",
  xformla = ~1,
  data = regDF)

summary(csaNev)
ggdid(csa)

csaESNev <- aggte(csaNev, type = "dynamic", na.rm = T)
summary(csaESNev)
ggdid(csaESNev)
#how do you go from this nonsense to point estimates per event time??

