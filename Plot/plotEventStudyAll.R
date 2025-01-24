#Plot event studies for all outcomes
library(tidyverse)
library(fixest)
library(patchwork)


# Birth lpm ---------------------------------------------------------------
df <- read_csv("Data/Clean/mergedBirth.csv")

#create a time to treatment variable. Set to 0 for never treated units
regDF <- df %>%
  #Create indicator of which states received treatment
  mutate(TREAT = case_when(
    CAPGrp == 0 ~ 0,
    CAPGrp %in% c(1,2) ~ 1)) %>%
  #shorten the window
  #filter(EVENTt >= -11 & EVENTt <= 6 & EDUCATIONi == 0 & MARSTi == 0) %>%
  filter(YEAR > 1981 & YEAR < 2011 & EDUCATIONi == 0 & MARSTi == 0)

#make a subset of treated with values for the desired event time window
treated <- regDF %>%
  filter(TREAT == 1 & EVENTt >= -11 & EVENTt <= 6)

untreated <- regDF %>%
  filter(TREAT == 0)

regDF <- rbind(treated, untreated) %>%
  # Following Sun and Abraham, we give our never-treated units a fake "treatment" date far outside the relevant study period.
  mutate(YEARCAPPED = case_when(
    TREAT == 1 ~ YEARCAPPED,
    TREAT == 0 ~ 10000)) %>%
  #sunab function needs never-treated to always have negative relative periods. 
  mutate(EVENTt = case_when(
    TREAT == 1 ~ EVENTt,
    TRUE ~ YEAR - YEARCAPPED))


# sunab birth -------------------------------------------------------------
sunABBirth <- feols(BIRTHi ~ sunab(YEARCAPPED, EVENTt) + AGE + RACE_ETH + NCHILD + TIMELIMITi + FEMUNEMP + MAXBENEFIT + MEDIANWAGE + lagMEDIANWAGE + lagUNEMP + lagBENEFIT|
                 STATECENSUS + YEAR,
               cluster = ~STATECENSUS,                          ## Clustered SEs
               data = regDF)

#create a time to treatment variable. Set to 0 for never treated units
regDF <- df %>%
  #Create indicator of which states received treatment
  mutate(TREAT = case_when(
    CAPGrp == 0 ~ 0,
    CAPGrp %in% c(1,2) ~ 1)) %>%
  #shorten the window
  #filter(EVENTt >= -11 & EVENTt <= 6 & EDUCATIONi == 0 & MARSTi == 0) %>%
  filter(YEAR > 1981 & YEAR < 2011 & EDUCATIONi == 0 & MARSTi == 0)

#make a subset of treated with values for the desired event time window
treated <- regDF %>%
  filter(TREAT == 1 & EVENTt >= -11 & EVENTt <= 6)
#note that time to treatment for the never treated is set to 0. 
untreated <- regDF %>%
  filter(TREAT == 0)

regDF <- rbind(treated, untreated) %>%
  # Following Sun and Abraham, we give our never-treated units a fake "treatment" date far outside the relevant study period.
  mutate(YEARCAPPED = case_when(
    TREAT == 1 ~ YEARCAPPED,
    TREAT == 0 ~ 0))


# TWFEDD estimate ---------------------------------------------------------
twfeDDBirth <- feols(BIRTHi ~ i(EVENTt, TREAT, ref = -1) + AGE + RACE_ETH + NCHILD + TIMELIMITi + FEMUNEMP + MAXBENEFIT + MEDIANWAGE + lagMEDIANWAGE + lagUNEMP + lagBENEFIT|
                       STATECENSUS + YEAR,
                     cluster = ~STATECENSUS,                          ## Clustered SEs
                     data = regDF)


# Empl lpm ----------------------------------------------------------------
df <- read_csv("Data/Clean/mergedEmployment.csv")

#create a time to treatment variable. Set to 0 for never treated units
regDF <- df %>%
  #Create indicator of which states received treatment
  mutate(TREAT = case_when(
    CAPGrp == 0 ~ 0,
    CAPGrp %in% c(1,2) ~ 1)) %>%
  #shorten the window
  #filter(EVENTt >= -11 & EVENTt <= 6 & EDUCATIONi == 0 & MARSTi == 0) %>%
  filter(YEAR > 1981 & YEAR < 2011 & EDUCATIONi == 0 & MARSTi == 0)

#make a subset of treated with values for the desired event time window
treated <- regDF %>%
  filter(TREAT == 1 & EVENTt >= -11 & EVENTt <= 6)

untreated <- regDF %>%
  filter(TREAT == 0)

regDF <- rbind(treated, untreated) %>%
  # Following Sun and Abraham, we give our never-treated units a fake "treatment" date far outside the relevant study period.
  mutate(YEARCAPPED = case_when(
    TREAT == 1 ~ YEARCAPPED,
    TREAT == 0 ~ 10000)) %>%
  #sunab function needs never-treated to always have negative relative periods. 
  mutate(EVENTt = case_when(
    TREAT == 1 ~ EVENTt,
    TRUE ~ YEAR - YEARCAPPED))


# sunab empl --------------------------------------------------------------
sunABEmpl <- feols(EMPLi ~ sunab(YEARCAPPED, EVENTt) + BIRTHi + AGE + RACE_ETH + NCHILD + TIMELIMITi + FEMUNEMP + MAXBENEFIT + MEDIANWAGE + lagMEDIANWAGE + lagUNEMP + lagBENEFIT|
                      STATECENSUS + YEAR,
                    cluster = ~STATECENSUS,                          ## Clustered SEs
                    data = regDF)


# TWFEDD prep -------------------------------------------------------------
#create a time to treatment variable. Set to 0 for never treated units
regDF <- df %>%
  #Create indicator of which states received treatment
  mutate(TREAT = case_when(
    CAPGrp == 0 ~ 0,
    CAPGrp %in% c(1,2) ~ 1)) %>%
  #shorten the window
  #filter(EVENTt >= -11 & EVENTt <= 6 & EDUCATIONi == 0 & MARSTi == 0) %>%
  filter(YEAR > 1981 & YEAR < 2011 & EDUCATIONi == 0 & MARSTi == 0)

#make a subset of treated with values for the desired event time window
treated <- regDF %>%
  filter(TREAT == 1 & EVENTt >= -11 & EVENTt <= 6)
#note that time to treatment for the never treated is set to 0. 
untreated <- regDF %>%
  filter(TREAT == 0)

regDF <- rbind(treated, untreated) %>%
  # Following Sun and Abraham, we give our never-treated units a fake "treatment" date far outside the relevant study period.
  mutate(YEARCAPPED = case_when(
    TREAT == 1 ~ YEARCAPPED,
    TREAT == 0 ~ 0))

twfeDDEmpl <- feols(EMPLi ~ i(EVENTt, TREAT, ref = -1) + BIRTHi + AGE + RACE_ETH + NCHILD + TIMELIMITi + FEMUNEMP + MAXBENEFIT + MEDIANWAGE + lagMEDIANWAGE + lagUNEMP + lagBENEFIT|
                       STATECENSUS + YEAR,
                     cluster = ~STATECENSUS,                          ## Clustered SEs
                     data = regDF)


# Hours worked ------------------------------------------------------------
regDF <- df %>%
  #Create indicator of which states received treatment
  mutate(TREAT = case_when(
    CAPGrp == 0 ~ 0,
    CAPGrp %in% c(1,2) ~ 1)) %>%
  #drop new jersey because ahrsworkt starts in 1989
  filter(STATEL != "New Jersey") %>%
  #shorten the window
  filter(EVENTt >= -7 & EVENTt <= 6 & EDUCATIONi == 0 & MARSTi == 0) %>%
  #change year bounds to reflect 11 years before 1994 states first full year (1995)
  filter(YEAR > 1988 & YEAR < 2011) %>%
  # Following Sun and Abraham, we give our never-treated units a fake "treatment" date far outside the relevant study period.
  mutate(YEARCAPPED = case_when(
    TREAT == 1 ~ YEARCAPPED,
    TREAT == 0 ~ 10000)) %>%
  #sunab function needs never-treated to always have negative relative periods.
  mutate(EVENTt = case_when(
    TREAT == 1 ~ EVENTt,
    TRUE ~ YEAR - YEARCAPPED))

sunabAhrsworkt <- feols(AHRSWORKT ~ sunab(YEARCAPPED, EVENTt) + BIRTHi + AGE + RACE_ETH + NCHILD + TIMELIMITi + FEMUNEMP + MAXBENEFIT + MEDIANWAGE + lagMEDIANWAGE + lagUNEMP + lagBENEFIT|
                          STATECENSUS + YEAR,
                        cluster = ~STATECENSUS,## Clustered SEs
                        data = regDF)

regDF <- df %>%
  #Create indicator of which states received treatment
  mutate(TREAT = case_when(
    CAPGrp == 0 ~ 0,
    CAPGrp %in% c(1,2) ~ 1)) %>%
  filter(STATEL != "New Jersey") %>%
  #shorten the window
  #filter(EVENTt >= -11 & EVENTt <= 6 & EDUCATIONi == 0 & MARSTi == 0) %>%
  filter(YEAR > 1988 & YEAR < 2011 & EDUCATIONi == 0 & MARSTi == 0)

#make a subset of treated with values for the desired event time window
treated <- regDF %>%
  filter(TREAT == 1 & EVENTt >= -7 & EVENTt <= 6)
#note that time to treatment for the never treated is set to 0. 
untreated <- regDF %>%
  filter(TREAT == 0)

regDF <- rbind(treated, untreated) %>%
  # Following Sun and Abraham, we give our never-treated units a fake "treatment" date far outside the relevant study period.
  mutate(YEARCAPPED = case_when(
    TREAT == 1 ~ YEARCAPPED,
    TREAT == 0 ~ 0))


twfeddAhrsworkt <- feols(AHRSWORKT ~ i(EVENTt, TREAT, ref = -1) + BIRTHi + AGE + RACE_ETH + NCHILD + TIMELIMITi + FEMUNEMP + MAXBENEFIT + MEDIANWAGE + lagMEDIANWAGE + lagUNEMP + lagBENEFIT|
        STATECENSUS + YEAR,
      cluster = ~STATECENSUS,                          ## Clustered SEs
      data = regDF)


# Plot birth --------------------------------------------------------------
iplot(list(sunABBirth, twfeDDBirth))

#extract estimates 
coef <- c(sunABBirth$coeftable[1:17], 0)
se <- c(sunABBirth$coeftable[1:17,2],NA)
type <- rep("IW Estimate", 18)
eventT <- c(seq(-11, -2, 1), seq(0,6,1), -1)
sunab <- cbind(coef, se, type, eventT) %>% 
  as.data.frame()

coef <- c(twfeDDBirth$coeftable[1:17], 0)
se <- c(twfeDDBirth$coeftable[1:17,2], NA)
type <- rep("TWFEDD Estimate", 18)

twfedd <- cbind(coef, se, type, eventT) %>% 
  as.data.frame()

eventStudy <- rbind(sunab, twfedd) %>%
  mutate(coef = as.numeric(coef), 
         se = as.numeric(se),
         eventT = as.numeric(eventT))

eventBirth <- ggplot(eventStudy, aes(x = eventT, group = type, colour = type, shape = type)) +
  geom_point(aes(y = coef), position = position_dodge(width=0.5)) +
  geom_errorbar(aes(ymax = coef + 2*se, ymin = coef-2*se), width = 0.5, position = "dodge") + 
  scale_colour_manual(values = c("red", "blue")) +
  scale_x_continuous(breaks = seq(-11, 6, 1), labels = seq(-11, 6, 1)) + 
  scale_y_continuous(breaks = c(-0.01, 0.00, 0.01, 0.02), labels = c(-0.01, 0.00, 0.01, 0.02)) + 
  geom_hline(yintercept = 0) + 
  ylab("Estimate and 95% CI") + 
  xlab("Time to treatment") +
  ggtitle("A: Probability of birth") +
  theme_bw() +
  theme(legend.position = "none",
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        legend.title = element_blank(),
        axis.title.x = element_blank())


# Plot p empl -------------------------------------------------------------
iplot(list(sunABEmpl, twfeDDEmpl))

#extract estimates 
coef <- c(sunABEmpl$coeftable[1:17], 0)
se <- c(sunABEmpl$coeftable[1:17,2],NA)
type <- rep("IW Estimate", 18)
eventT <- c(seq(-11, -2, 1), seq(0,6,1), -1)
sunab <- cbind(coef, se, type, eventT) %>% 
  as.data.frame()

coef <- c(twfeDDEmpl$coeftable[1:17], 0)
se <- c(twfeDDEmpl$coeftable[1:17,2], NA)
type <- rep("TWFEDD Estimate", 18)

twfedd <- cbind(coef, se, type, eventT) %>% 
  as.data.frame()

eventStudy <- rbind(sunab, twfedd) %>%
  mutate(coef = as.numeric(coef), 
         se = as.numeric(se),
         eventT = as.numeric(eventT))

eventEmpl <- ggplot(eventStudy, aes(x = eventT, group = type, colour = type, shape = type)) +
  geom_point(aes(y = coef), position = position_dodge(width=0.5)) +
  geom_errorbar(aes(ymax = coef + 2*se, ymin = coef-2*se), width = 0.5, position = "dodge") + 
  scale_colour_manual(values = c("red", "blue")) +
  scale_x_continuous(breaks = seq(-11, 6, 1), labels = seq(-11, 6, 1)) + 
  scale_y_continuous(breaks = seq(-0.07, 0.05, 0.01), labels = seq(-0.07, 0.05, 0.01)) + 
  geom_hline(yintercept = 0) + 
  ylab("Estimate and 95% CI") + 
  xlab("Time to treatment") +
  ggtitle("B: Probability of employment") +
  theme_bw() +
  theme(legend.position = "bottom",
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        legend.title = element_blank(),
        axis.title.x = element_blank())

# Plot ahrsworkt --------------------------------------------------------------------
iplot(list(sunabAhrsworkt, twfeddAhrsworkt))

#extract estimates 
coef <- c(sunabAhrsworkt$coeftable[1:13], 0)
se <- c(sunabAhrsworkt$coeftable[1:13,2],NA)
type <- rep("IW Estimate", 14)
eventT <- c(seq(-7, -2, 1), seq(0,6,1), -1)
sunab <- cbind(coef, se, type, eventT) %>% 
  as.data.frame()

coef <- c(twfeddAhrsworkt$coeftable[1:13], 0)
se <- c(twfeddAhrsworkt$coeftable[1:13,2], NA)
type <- rep("TWFEDD Estimate", 14)

twfedd <- cbind(coef, se, type, eventT) %>% 
  as.data.frame()

eventStudy <- rbind(sunab, twfedd) %>%
  mutate(coef = as.numeric(coef), 
         se = as.numeric(se),
         eventT = as.numeric(eventT))

eventAhrsworkt <- ggplot(eventStudy, aes(x = eventT, group = type, colour = type, shape = type)) +
  geom_point(aes(y = coef), position = position_dodge(width=0.5)) +
  geom_errorbar(aes(ymax = coef + 2*se, ymin = coef-2*se), width = 0.5, position = "dodge") + 
  scale_colour_manual(values = c("red", "blue")) +
  scale_x_continuous(breaks = seq(-7, 6, 1), labels = seq(-7, 6, 1)) + 
  scale_y_continuous(breaks = seq(-1, 1.5, 0.5), labels = seq(-1, 1.5, 0.5)) + 
  geom_hline(yintercept = 0) + 
  ylab("Estimate and 95% CI") + 
  xlab("Time to treatment") +
  #ggtitle("C: Number of hours usually worked weekly") +
  theme_bw() +
  theme(legend.position = "bottom",
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        legend.title = element_blank())

# Combine plots -----------------------------------------------------------
#events <- eventBirth / eventEmpl / eventAhrsworkt

#ggsave("Output/Figures/eventStudyOutcomes.png", events, height = 8.5, width = 6)

events2 <- eventBirth / eventEmpl

ggsave("Output/Figures/eventStudyBE.png", events2, height = 7, width = 6)

ggsave("Output/Figures/eventStudyAhrs.png", eventAhrsworkt, height = 4, width = 6)
