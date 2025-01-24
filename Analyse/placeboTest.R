#anticipation test with sun abraham
library(tidyverse)
library(fixest)
library(stargazer)
library(patchwork)

#Source: https://lost-stats.github.io/Model_Estimation/Research_Design/event_study.html

#####All regressions on unmarried women aged 15-45 with high school or less: Linear probability models
#notes: YNGCH & ELDCH cause multicollinearity, push everything very close to 0 and raise standard errors. Adding weights did not change estimates, significance or R2. 

# Placebo for birth -------------------------------------------------------
df <- read_csv("Data/Clean/mergedBirth.csv")

#create a time to treatment variable. Set to 0 for never treated units
regDF <- df %>%
  #Create indicator of which states received treatment
  mutate(TREAT = case_when(
    CAPGrp == 0 ~ 0,
    CAPGrp %in% c(1,2) ~ 1)) %>%
  #shorten the window - remove all observations from treatmet period 
  #filter(EVENTt >= -11 & EVENTt < 0 | is.na(EVENTt) == T) %>%
  filter(YEAR > 1981 & YEAR < 2004 & EDUCATIONi == 0 & MARSTi == 0)

#make a subset of treated with values for the desired event time window
treated <- regDF %>%
  filter(TREAT == 1 & EVENTt >= -11 & EVENTt < 0)

untreated <- regDF %>%
  filter(TREAT == 0)

regDF <- rbind(treated, untreated) %>%
  # Following Sun and Abraham, we give our never-treated units a fake "treatment" date far outside the relevant study period.
  #change the treated years so sunab doesn't treat them as never treated
  mutate(YEARCAPPED = case_when(
    TREAT == 1 ~ YEARCAPPED - 6,
    #TREAT == 1 ~ YEARCAPPED,
    TREAT == 0 ~ 10000)) %>%
  #sunab function needs never-treated to always have negative relative periods. 
  mutate(EVENTt = EVENTt+6)


# Regression --------------------------------------------------------------
reg1 <- feols(BIRTHi ~ sunab(YEARCAPPED, EVENTt, ref.p = 0) + AGE + RACE_ETH + NCHILD + TIMELIMITi + FEMUNEMP + MAXBENEFIT + MEDIANWAGE + lagMEDIANWAGE + lagUNEMP + lagBENEFIT|
                STATECENSUS + YEAR,
              cluster = ~STATECENSUS,                          ## Clustered SEs                          ## Clustered SEs
              data = regDF)

iplot(reg1)
# Plot --------------------------------------------------------------------
coef <- c(reg1$coeftable[1:10], 0)
se <- c(reg1$coeftable[1:10,2],NA)
eventT <- c(seq(-11, -7, 1), seq(-5,-1,1), -6)
reg <- cbind(coef, se, eventT) %>% 
  as.data.frame() %>%
  mutate(coef = as.numeric(coef), 
         se = as.numeric(se),
         eventT = as.numeric(eventT))

placeboBirth <- ggplot(reg, aes(x = eventT)) +
  geom_point(aes(y = coef), position = position_dodge(width=0.3)) +
  geom_errorbar(aes(ymax = coef + 2*se, ymin = coef-2*se), width = 0.3, position = "dodge") +
  scale_x_continuous(breaks = seq(-11, -1, 1), labels = seq(-11, -1, 1)) + 
  scale_y_continuous(breaks = seq(-0.015, 0.015, 0.005), labels = seq(-0.015, 0.015, 0.005)) + 
  geom_hline(yintercept = 0) + 
  ylab("Estimate and 95% confidence interval") + 
  xlab("Time to treatment") +
  theme_bw() +
  theme(legend.position = "bottom",
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        legend.title = element_blank())

ggsave("Output/Figures/placeboBirth.png", placeboBirth, height = 3, width = 5)

# Placebo for employment -------------------------------------------------------
df <- read_csv("Data/Clean/mergedEmployment.csv")

#create a time to treatment variable. Set to 0 for never treated units
regDF <- df %>%
  #Create indicator of which states received treatment
  mutate(TREAT = case_when(
    CAPGrp == 0 ~ 0,
    CAPGrp %in% c(1,2) ~ 1)) %>%
  #shorten the window - remove all observations from treatmet period 
  #filter(EVENTt >= -11 & EVENTt < 0 | is.na(EVENTt) == T) %>%
  filter(YEAR > 1981 & YEAR < 2004 & EDUCATIONi == 0 & MARSTi == 0)

#make a subset of treated with values for the desired event time window
treated <- regDF %>%
  filter(TREAT == 1 & EVENTt >= -11 & EVENTt < 0)

untreated <- regDF %>%
  filter(TREAT == 0)

regDF <- rbind(treated, untreated) %>%
  # Following Sun and Abraham, we give our never-treated units a fake "treatment" date far outside the relevant study period.
  #change the treated years so sunab doesn't treat them as never treated
  mutate(YEARCAPPED = case_when(
    TREAT == 1 ~ YEARCAPPED - 6,
    #TREAT == 1 ~ YEARCAPPED,
    TREAT == 0 ~ 10000)) %>%
  #sunab function needs never-treated to always have negative relative periods. 
  mutate(EVENTt = EVENTt+6)


# Regression --------------------------------------------------------------
reg2 <- feols(EMPLi ~ sunab(YEARCAPPED, EVENTt, ref.p = 0) + BIRTHi + AGE + RACE_ETH + NCHILD + TIMELIMITi + FEMUNEMP + MAXBENEFIT + MEDIANWAGE + lagMEDIANWAGE + lagUNEMP + lagBENEFIT|
                STATECENSUS + YEAR,
              cluster = ~STATECENSUS,                          ## Clustered SEs                          ## Clustered SEs
              data = regDF)

iplot(reg2)
# Plot --------------------------------------------------------------------
coef <- c(reg2$coeftable[1:10], 0)
se <- c(reg2$coeftable[1:10,2],NA)
eventT <- c(seq(-11, -7, 1), seq(-5,-1,1), -6)
reg <- cbind(coef, se, eventT) %>% 
  as.data.frame() %>%
  mutate(coef = as.numeric(coef), 
         se = as.numeric(se),
         eventT = as.numeric(eventT))

placeboEmpl <- ggplot(reg, aes(x = eventT)) +
  geom_point(aes(y = coef), position = position_dodge(width=0.3)) +
  geom_errorbar(aes(ymax = coef + 2*se, ymin = coef-2*se), width = 0.3, position = "dodge") +
  scale_x_continuous(breaks = seq(-11, -1, 1), labels = seq(-11, -1, 1)) + 
  scale_y_continuous(breaks = seq(-0.05, 0.04, 0.01), labels = seq(-0.05, 0.04, 0.01)) + 
  geom_hline(yintercept = 0) + 
  ylab("Estimate and 95% confidence interval") + 
  xlab("Time to treatment") +
  theme_bw() +
  theme(legend.position = "bottom",
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        legend.title = element_blank())

ggsave("Output/Figures/placeboEmpl.png", placeboEmpl, height = 3, width = 5)

placeboBirth <- placeboBirth + 
  ggtitle("A: Probability of birth")

placeboEmpl <- placeboEmpl + 
  ggtitle("B: Probability of employment")

placebo <- placeboBirth + placeboEmpl

ggsave("Output/Figures/placeboTest.png", placebo, height = 4, width = 8)


# Regression table --------------------------------------------------------
etable(summary(reg1, agg = "ATT"), summary(reg2, agg = "ATT"), digits = 3, tex = T)

